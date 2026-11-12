# Property tests (design §9): random closed-form models, written as NONMEM
# control streams, must round-trip through every format and simulate the
# same on both engines.

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
