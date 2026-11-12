# Property tests (design §9): random closed-form models, written as NONMEM
# control streams, must round-trip through every format and simulate the
# same on both engines.

# A random closed-form model: an ADVAN, positive parameter values, etas on
# some parameters, a residual model, and for a depot a lag and a
# bioavailability. Returns the control stream and what it holds.
random_closed_model <- function() {
  advan <- sample(c(1L, 2L, 3L, 4L, 11L, 12L), 1)
  form <- closed_forms[[as.character(advan)]]
  names <- unname(form$names)
  values <- signif(exp(stats::runif(length(names), log(0.2), log(60))), 4)
  depot <- "KA" %in% names
  eta_on <- names[stats::runif(length(names)) < 0.5]
  if (length(eta_on) == 0) {
    eta_on <- names[[1]]
  }
  pk <- vapply(
    seq_along(names),
    function(i) {
      eta <- match(names[[i]], eta_on)
      sprintf(
        "%s = THETA(%d)%s",
        names[[i]],
        i,
        if (is.na(eta)) "" else sprintf(" * EXP(ETA(%d))", eta)
      )
    },
    character(1)
  )
  thetas <- sprintf("$THETA (0, %s)", format(values))
  dosing <- character()
  lag <- NA
  bio <- NA
  if (depot && stats::runif(1) < 0.5) {
    lag <- signif(stats::runif(1, 0.1, 1), 3)
    bio <- signif(stats::runif(1, 0.3, 1), 3)
    k <- length(names)
    dosing <- c(
      sprintf("ALAG1 = THETA(%d)", k + 1),
      sprintf("F1 = THETA(%d)", k + 2)
    )
    thetas <- c(
      thetas,
      sprintf("$THETA (0, %s)", lag),
      sprintf("$THETA (0, %s, 1)", bio)
    )
  }
  central <- form$names[["v"]]
  residual <- sample(
    c("additive", "proportional", "combined", "exponential"),
    1
  )
  y <- switch(
    residual,
    additive = "Y = IPRED + EPS(1)",
    proportional = "Y = IPRED * (1 + EPS(1))",
    combined = "Y = IPRED * (1 + EPS(1)) + EPS(2)",
    exponential = "Y = IPRED * EXP(EPS(1))"
  )
  lines <- c(
    "$PROBLEM random closed form",
    sprintf("$SUBROUTINES ADVAN%d TRANS%d", advan, form$trans),
    "$PK",
    pk,
    dosing,
    sprintf("S%d = %s", form$central, central),
    "$ERROR",
    "IPRED = F",
    y,
    thetas,
    sprintf(
      "$OMEGA %s",
      paste(signif(stats::runif(length(eta_on), 0.01, 0.3), 3), collapse = " ")
    ),
    sprintf("$SIGMA %s", if (residual == "combined") "0.04 0.01" else "0.04")
  )
  list(
    lines = lines,
    advan = advan,
    values = stats::setNames(values, names),
    lag = lag,
    bio = bio
  )
}

test_that("random closed-form models round-trip through every format", {
  withr::local_seed(20261112)
  writers <- list(
    rxode2 = pmx_write_rxode2,
    nonmem = pmx_write_nonmem,
    mrgsolve = pmx_write_mrgsolve
  )

  for (k in 1:40) {
    spec <- random_closed_model()
    m <- read_ctl(spec$lines)
    for (to in names(writers)) {
      label <- sprintf("model %d (ADVAN%d) -> %s", k, spec$advan, to)
      w <- writers[[to]](m)
      expect_faithful(w, label = label)
      back <- pmx_write_nonmem(text_model(w$text, to))
      expect_faithful(back, label = paste(label, "-> nonmem"))
    }
  }
})

test_that("random closed-form models round-trip through JSON", {
  skip_if_no_engine("zujson")
  withr::local_seed(20261113)

  for (k in 1:40) {
    m <- read_ctl(random_closed_model()$lines)
    file <- withr::local_tempfile(fileext = ".json")
    pmx_write_json(m, file)
    expect_identical(pmx_read_json(file), m, label = paste("model", k))
  }
})

test_that("random closed-form models simulate the same on both engines", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  withr::local_seed(20261114)
  design <- pmx_design(n = 2, seed = 3)

  for (k in 1:6) {
    spec <- random_closed_model()
    m <- read_ctl(spec$lines)
    label <- sprintf("model %d (ADVAN%d)", k, spec$advan)
    expect_true(pmx_compare(m, pmx_write_rxode2(m), design)$pass, label = label)
    expect_true(
      pmx_compare(m, pmx_write_mrgsolve(m), design)$pass,
      label = label
    )
  }
})

test_that("one- and two-compartment typical predictions are the closed forms", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  withr::local_seed(20261115)
  design <- pmx_design(n = 0, regimens = "single")
  checked <- 0

  while (checked < 4) {
    spec <- random_closed_model()
    if (!spec$advan %in% c(1L, 2L)) {
      next
    }
    checked <- checked + 1
    m <- read_ctl(spec$lines)
    cmp <- pmx_compare(m, pmx_write_mrgsolve(m), design)
    t <- cmp$predictions$time
    p <- spec$values
    k <- p[["CL"]] / p[["V"]]
    expected <- if (spec$advan == 1L) {
      100 / p[["V"]] * exp(-k * t)
    } else {
      lag <- if (is.na(spec$lag)) 0 else spec$lag
      f <- if (is.na(spec$bio)) 1 else spec$bio
      ka <- p[["KA"]]
      s <- pmax(t - lag, 0)
      f * 100 * ka / (p[["V"]] * (ka - k)) * (exp(-k * s) - exp(-ka * s))
    }
    expect_equal(cmp$predictions$a, expected, tolerance = 1e-7)
    expect_equal(cmp$predictions$b, expected, tolerance = 1e-7)
  }
})
