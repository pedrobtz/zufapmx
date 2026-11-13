# Engine-free tests mock the engines (helper-compare.R); tests that
# simulate check against closed forms or the corpus's expected.csv.

test_that("both engines match closed forms on every regimen and subject", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1())
  d <- pmx_design(n = 2, seed = 11)

  cmp <- pmx_compare(m, pmx_write_mrgsolve(m), d)

  expect_true(cmp$pass)
  expect_equal(cmp$engines$engine, c("rxode2", "mrgsolve"))
  # The draws, made independently: subjects 2 and 3 take z * sd.
  withr::local_seed(11, .rng_kind = "Mersenne-Twister")
  z <- rbind(c(0, 0), matrix(stats::rnorm(4), 2, 2))
  eta <- sweep(z, 2, sqrt(c(0.09, 0.04)), `*`)
  p <- cmp$predictions
  expected <- numeric(nrow(p))
  for (i in seq_len(nrow(p))) {
    expected[[i]] <- advan1_conc(
      p$regimen[[i]],
      p$time[[i]],
      cl = 4 * exp(eta[p$id[[i]], 1]),
      v = 50 * exp(eta[p$id[[i]], 2])
    )
  }
  expect_equal(p$a, expected, tolerance = 1e-8)
  expect_equal(p$b, expected, tolerance = 1e-8)
  expect_gt(length(unique(round(p$a[p$time == 1], 6))), 3)
})

test_that("the typical subject reproduces every corpus entry's expected.csv", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  corpus <- zupmxio_corpus()
  # The design's single dose is the corpus's: 100 into compartment 1 at the
  # same times. The infusion entry's corpus dose lasts 1 unit, not 2.
  corpus <- corpus[corpus$regimen != "infusion", ]
  expect_gt(nrow(corpus), 0)

  for (i in seq_len(nrow(corpus))) {
    m <- pmx_read_nonmem(file.path(corpus$path[[i]], "model.ctl"))
    cmp <- pmx_compare(
      m,
      pmx_write_rxode2(m),
      pmx_design(n = 0, regimens = "single")
    )
    expected <- corpus_expected(corpus$path[[i]])$pred
    expect_equal(cmp$predictions$a, expected, tolerance = 1e-6)
    expect_equal(cmp$predictions$b, expected, tolerance = 1e-6)
  }
})

test_that("a modelled duration doses every regimen at it, and skips infusion", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1(pk = "D1 = 2"))

  cmp <- pmx_compare(m, pmx_write_mrgsolve(m), pmx_design(n = 0))

  expect_true(cmp$pass)
  expect_equal(
    cmp$regimens$skipped[cmp$regimens$regimen == "infusion"],
    "a model sets the dose's rate or duration"
  )
  # A single dose over 2 units is the infusion's closed form.
  single <- cmp$predictions[cmp$predictions$regimen == "single", ]
  expected <- advan1_conc("infusion", single$time, cl = 4, v = 50)
  expect_equal(single$a, expected, tolerance = 1e-8)
  expect_equal(single$b, expected, tolerance = 1e-8)
})

test_that("covariates take the design's values, matched without case", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1(
    cl = "CL = THETA(1) * EXP(ETA(1)) * (WT / 70)**0.75"
  ))

  cmp <- pmx_compare(
    m,
    pmx_write_mrgsolve(m),
    pmx_design(n = 0, regimens = "single", covariates = list(wt = 80))
  )

  expect_true(cmp$pass)
  expected <- advan1_conc(
    "single",
    cmp$predictions$time,
    cl = 4 * (80 / 70)^0.75,
    v = 50
  )
  expect_equal(cmp$predictions$a, expected, tolerance = 1e-8)
  expect_equal(cmp$predictions$b, expected, tolerance = 1e-8)
})

test_that("a covariate the design lacks is the caller's error", {
  local_fake_engines()
  m <- read_ctl(ctl_advan1(
    cl = "CL = THETA(1) * EXP(ETA(1)) * (WT / 70)**0.75"
  ))

  expect_error(
    pmx_compare(m, m, pmx_design(n = 0)),
    class = "zupmxio_model_error"
  )
})

test_that("a wrong translation fails as a value, with where it differs", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1())
  w <- pmx_write_rxode2(m)
  w$text <- sub(
    "THETA1 <- c(0, 4)",
    "THETA1 <- c(0, 4.1)",
    w$text,
    fixed = TRUE
  )

  cmp <- pmx_compare(m, w, pmx_design(n = 1))

  expect_false(cmp$pass)
  expect_null(cmp$reason)
  expect_gt(cmp$worst$rel, 1e-3)
  expect_true(all(cmp$regimens$failed > 0))
})

test_that("each point agrees within tol or atol, at the boundary too", {
  obs <- data.frame(id = 1L, time = 1:4)

  # Relative deviation exactly tol, and just over it (exact in binary).
  p <- compare_points(
    "single",
    obs[1:2, ],
    c(1, 1),
    c(0.75, 0.7499),
    0.25,
    0
  )
  expect_equal(p$pass, c(TRUE, FALSE))
  # Absolute deviation at atol rescues a large relative deviation near zero.
  p <- compare_points(
    "single",
    obs[1:2, ],
    c(1e-12, 1e-12),
    c(0, 0),
    1e-6,
    1e-12
  )
  expect_equal(p$pass, c(TRUE, TRUE))
  p <- compare_points("single", obs[1, ], 2e-12, 0, 1e-6, 1e-12)
  expect_false(p$pass)
  # Missing and non-finite predictions never agree, even with each other.
  p <- compare_points(
    "single",
    obs,
    c(NA, Inf, NaN, 0),
    c(NA, Inf, 1, 0),
    1,
    1
  )
  expect_equal(p$pass, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(p$rel, c(Inf, Inf, Inf, 0))
})

test_that("a short or missing engine table fails, never passes", {
  local_fake_engines()
  testthat::local_mocked_bindings(
    engine_mrgsolve = function(text, data, etas) {
      obs <- data[data$evid == 0, ]
      data.frame(id = obs$id, time = obs$time, Y = obs$time)[-1, ]
    }
  )
  m <- read_ctl(ctl_advan1())

  cmp <- pmx_compare(m, m, pmx_design(n = 0), engines = c("rxode2", "mrgsolve"))

  expect_false(cmp$pass)
  expect_true(is.na(cmp$worst$b))
  expect_equal(sum(!cmp$predictions$pass), length(cmp$design$regimens))

  testthat::local_mocked_bindings(
    engine_mrgsolve = function(text, data, etas) data.frame(id = 1, time = 1)
  )
  cmp <- pmx_compare(m, m, pmx_design(n = 0), engines = c("rxode2", "mrgsolve"))
  expect_false(cmp$pass)
  expect_true(all(is.na(cmp$predictions$b)))
})

test_that("models with different numbers of random effects fail with a reason", {
  local_fake_engines()
  a <- read_ctl(ctl_advan1())
  lines <- sub("THETA(2) * EXP(ETA(2))", "THETA(2)", ctl_advan1(), fixed = TRUE)
  b <- read_ctl(sub("$OMEGA 0.09 0.04", "$OMEGA 0.09", lines, fixed = TRUE))

  cmp <- pmx_compare(a, b, pmx_design(n = 0))

  expect_false(cmp$pass)
  expect_match(cmp$reason, "2 and 1")
  expect_null(cmp$predictions)
  expect_null(cmp$worst)
})

test_that("a comparison never runs on the engine of the writer under test", {
  local_fake_engines()
  nm <- read_ctl(ctl_advan1())
  mrg <- pmx_read_mrgsolve(corpus_file("onecmt-oral", "model.cpp"))
  rx <- corpus_rxode2_model("onecmt-oral")
  engines <- function(a, b, ...) {
    pmx_compare(
      a,
      b,
      pmx_design(n = 0, regimens = "single"),
      ...
    )$engines$engine
  }

  expect_equal(engines(nm, pmx_write_rxode2(nm)), c("mrgsolve", "rxode2"))
  expect_equal(engines(nm, pmx_write_mrgsolve(nm)), c("rxode2", "mrgsolve"))
  expect_equal(engines(pmx_write_mrgsolve(nm), nm), c("mrgsolve", "rxode2"))
  expect_equal(engines(rx, pmx_write_rxode2(rx)), c("mrgsolve", "rxode2"))
  expect_equal(engines(mrg, pmx_write_mrgsolve(mrg)), c("rxode2", "mrgsolve"))
  # Models without a writer under test run on their own engine, NONMEM on
  # rxode2; so does a written NONMEM model.
  expect_equal(engines(mrg, rx), c("mrgsolve", "rxode2"))
  expect_equal(engines(nm, pmx_write_nonmem(nm)), c("rxode2", "rxode2"))
  # Unless the user asks.
  expect_equal(
    engines(nm, pmx_write_rxode2(nm), engines = c("rxode2", NA)),
    c("rxode2", "rxode2")
  )
  expect_equal(
    engines(pmx_write_rxode2(nm), nm, engines = c("mrgsolve", "mrgsolve")),
    c("mrgsolve", "mrgsolve")
  )
})

test_that("a written model runs as written, not rewritten", {
  local_fake_engines()
  m <- read_ctl(ctl_advan1())
  w <- pmx_write_rxode2(m)
  w$text <- sub("THETA1 <- c(0, 4)", "THETA1 <- c(0, 5)", w$text, fixed = TRUE)

  cmp <- pmx_compare(m, w, pmx_design(n = 0, regimens = "single"))

  expect_identical(cmp$sides$b$text, w$text)
  expect_match(cmp$sides$b$origin, "as written")
  expect_match(cmp$sides$a$origin, "written to mrgsolve by zupmxio")
})

test_that("a missing engine is an engine error", {
  local_fake_engines()
  testthat::local_mocked_bindings(
    engine_available = function(engine) engine != "mrgsolve"
  )
  m <- read_ctl(ctl_advan1())
  d <- pmx_design(n = 0)

  # D4 leaves a only mrgsolve.
  expect_error(
    pmx_compare(m, pmx_write_rxode2(m), d),
    class = "zupmxio_engine_error"
  )
  # A written mrgsolve model needs mrgsolve.
  expect_error(
    pmx_compare(pmx_write_mrgsolve(m), m, d),
    class = "zupmxio_engine_error"
  )
  # A model with a choice falls back to the engine that is installed.
  mrg <- pmx_read_mrgsolve(corpus_file("onecmt-oral", "model.cpp"))
  expect_equal(pmx_compare(mrg, m, d)$engines$engine, c("rxode2", "rxode2"))

  err <- tryCatch(
    pmx_compare(m, pmx_write_rxode2(m), d),
    zupmxio_engine_error = function(e) e
  )
  expect_equal(err$engine, "mrgsolve")
})

test_that("an engine that fails is an engine error carrying its message", {
  local_fake_engines()
  testthat::local_mocked_bindings(
    engine_mrgsolve = function(text, data, etas) stop("solver blew up")
  )
  m <- read_ctl(ctl_advan1())

  err <- tryCatch(
    pmx_compare(m, pmx_write_rxode2(m), pmx_design(n = 0)),
    error = function(e) e
  )

  expect_s3_class(err, "zupmxio_engine_error")
  expect_equal(err$engine, "mrgsolve")
  expect_equal(conditionMessage(err$parent), "solver blew up")
})

test_that("a writer's refusal moves a model to the next engine", {
  local_fake_engines()
  # rxode2 cannot spell the record's time in individual statements (D15).
  m <- read_ctl(ctl_advan1(pk = "TAD = TIME"))
  expect_error(pmx_write_rxode2(m), class = "zupmxio_write_error")

  cmp <- pmx_compare(m, m, pmx_design(n = 0, regimens = "single"))

  expect_equal(cmp$engines$engine, c("mrgsolve", "mrgsolve"))
  # With no engine left, the refusal is the error.
  expect_error(
    pmx_compare(m, pmx_write_mrgsolve(m), pmx_design(n = 0)),
    class = "zupmxio_write_error"
  )
})

test_that("the design's dose compartment must exist in both models", {
  local_fake_engines()
  m <- read_ctl(ctl_advan1())

  expect_error(
    pmx_compare(m, m, pmx_design(n = 0, cmt = 2)),
    class = "zupmxio_model_error"
  )
})

test_that("each engine gets the dose compartment in its own terms", {
  m <- unclass(pmx_read_mrgsolve(corpus_file("onecmt-oral-mm", "model.cpp")))
  events <- pmx_design(n = 0, regimens = "single", cmt = 2)$events

  expect_equal(unique(side_events(m, events, 2, "rxode2")$cmt), "CENT")
  expect_equal(unique(side_events(m, events, 2, "mrgsolve")$cmt), 2)
  expect_false("regimen" %in% names(side_events(m, events, 2, "rxode2")))
  lag <- unclass(read_ctl(ctl_advan1(pk = "R1 = 50")))
  data <- side_events(lag, events, 1, "mrgsolve")
  expect_equal(data$rate[data$evid == 1], -1)
})

test_that("both sides draw the same etas from the seed, and leave the RNG alone", {
  omega <- diag(c(0.09, 0.04))
  dimnames(omega) <- list(c("ETA1", "ETA2"), c("ETA1", "ETA2"))
  d <- pmx_design(n = 3, seed = 5)

  withr::local_seed(99)
  before <- .Random.seed
  etas <- compare_etas(list(a = omega, b = omega * 4), d)
  expect_identical(.Random.seed, before)

  expect_equal(etas$a[1, ], c(ETA1 = 0, ETA2 = 0))
  expect_equal(etas$b, etas$a * 2)
  expect_equal(
    compare_etas(list(a = omega, b = omega), d),
    compare_etas(list(a = omega, b = omega), d)
  )
  expect_false(isTRUE(all.equal(
    compare_etas(list(a = omega, b = omega), pmx_design(n = 3, seed = 6))$a,
    etas$a
  )))
})

test_that("the eta draw does not depend on the caller's RNG kind", {
  omega <- matrix(0.09, 1, 1, dimnames = list("ETA1", "ETA1"))
  d <- pmx_design(n = 3, seed = 5)
  ref <- compare_etas(list(a = omega, b = omega), d)

  withr::local_seed(
    1,
    .rng_kind = "Knuth-TAOCP-2002",
    .rng_normal_kind = "Box-Muller"
  )
  kind <- RNGkind()
  expect_equal(compare_etas(list(a = omega, b = omega), d), ref)
  expect_equal(RNGkind(), kind)
})

test_that("omega's square root holds semi-definite matrices, refuses others", {
  zero <- matrix(c(0.09, 0, 0, 0), 2)
  expect_equal(omega_root(zero) %*% omega_root(zero), zero)
  full <- matrix(c(0.09, 0.03, 0.03, 0.04), 2)
  expect_equal(omega_root(full) %*% omega_root(full), full)
  expect_error(
    omega_root(matrix(c(0.09, 0.2, 0.2, 0.04), 2)),
    class = "zupmxio_model_error"
  )
})

test_that("pmx_compare() refuses malformed arguments", {
  local_fake_engines()
  m <- read_ctl(ctl_advan1())
  d <- pmx_design(n = 0)
  bad <- list(
    list(a = "model.ctl", b = m),
    list(a = m, b = list()),
    list(a = m, b = m, design = list()),
    list(a = m, b = m, tol = 0),
    list(a = m, b = m, tol = -1),
    list(a = m, b = m, tol = NA),
    list(a = m, b = m, tol = Inf),
    list(a = m, b = m, tol = c(1e-6, 1e-6)),
    list(a = m, b = m, tol = "1e-6"),
    list(a = m, b = m, atol = -1),
    list(a = m, b = m, atol = NaN),
    list(a = m, b = m, engines = "rxode2"),
    list(a = m, b = m, engines = c("rxode2", "nonmem")),
    list(a = m, b = m, engines = c(1, 2))
  )
  for (args in bad) {
    if (is.null(args$design)) {
      args$design <- d
    }
    expect_error(
      do.call(pmx_compare, args),
      class = "zupmxio_model_error",
      label = deparse(args[setdiff(names(args), c("a", "b", "design"))])
    )
  }
  expect_true(pmx_compare(m, m, d, atol = 0)$pass)
  expect_true(pmx_compare(m, m, d, engines = c(NA, NA))$pass)
})

test_that("a comparison prints its verdict, engines and worst point", {
  local_fake_engines(mrgsolve = function(time, id) time * (1 + 1e-3))
  m <- read_ctl(ctl_advan1())

  expect_snapshot({
    pmx_compare(m, pmx_write_rxode2(m), pmx_design(n = 1))
    pmx_compare(m, m, pmx_design(n = 0, regimens = "single"))
  })
  lines <- sub("THETA(2) * EXP(ETA(2))", "THETA(2)", ctl_advan1(), fixed = TRUE)
  b <- read_ctl(sub("$OMEGA 0.09 0.04", "$OMEGA 0.09", lines, fixed = TRUE))
  expect_snapshot(pmx_compare(m, b, pmx_design(n = 0)))
})

test_that("every corpus model round-trips through every pair of formats", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  writers <- list(
    rxode2 = pmx_write_rxode2,
    nonmem = pmx_write_nonmem,
    mrgsolve = pmx_write_mrgsolve
  )
  corpus <- zupmxio_corpus()
  design <- pmx_design(n = 2)

  for (i in seq_len(nrow(corpus))) {
    models <- corpus_models(corpus$path[[i]], corpus$id[[i]])
    for (from in names(models)) {
      for (to in names(writers)) {
        label <- sprintf("%s: %s -> %s", corpus$id[[i]], from, to)
        w <- writers[[to]](models[[from]])
        cmp <- pmx_compare(models[[from]], w, design)
        expect_true(cmp$pass, label = label)
        # And back: X -> Y -> X against the original.
        back <- writers[[from]](text_model(w$text, to))
        cmp <- pmx_compare(models[[from]], back, design)
        expect_true(cmp$pass, label = paste(label, "->", from))
      }
    }
  }
})

test_that("a model agrees with NONMEM tables printed from its predictions", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  corpus <- zupmxio_corpus()
  corpus <- corpus[corpus$regimen != "infusion", ]

  for (i in seq_len(nrow(corpus))) {
    expected <- corpus_expected(corpus$path[[i]])
    # What NONMEM would print: 5 significant digits.
    table <- local_nm_table(table_single_dose(expected$time, expected$pred))
    tables <- pmx_read_tables(table)
    models <- corpus_models(corpus$path[[i]], corpus$id[[i]])
    for (from in names(models)) {
      cmp <- pmx_compare(tables, models[[from]])
      label <- paste(corpus$id[[i]], from)
      expect_true(cmp$pass, label = label)
      expect_equal(cmp$engines$engine[[1]], "table", label = label)
      expect_equal(nrow(cmp$predictions), nrow(expected), label = label)
    }
  }
})

test_that("a table value off in its fourth digit fails", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  expected <- corpus_expected(corpus_file("onecmt-oral", ""))
  pred <- expected$pred
  pred[[3]] <- pred[[3]] * 1.001
  tables <- pmx_read_tables(local_nm_table(table_single_dose(
    expected$time,
    pred
  )))

  cmp <- pmx_compare(tables, corpus_rxode2_model("onecmt-oral"))

  expect_false(cmp$pass)
  expect_equal(which(!cmp$predictions$pass), 3L)
  expect_equal(cmp$worst$time, expected$time[[3]])
})

test_that("tables carry their subjects, covariates and repeated times", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  m <- read_ctl(ctl_advan1(
    cl = "CL = THETA(1) * EXP(ETA(1)) * (WT / 70)**0.75"
  ))
  times <- c(0.5, 2, 2, 8)
  subject <- function(id, wt) {
    df <- table_single_dose(
      times,
      advan1_conc("single", times, cl = 4 * (wt / 70)^0.75, v = 50),
      id = id
    )
    df$WT <- wt
    df
  }
  tables <- pmx_read_tables(local_nm_table(rbind(
    subject(101, 60),
    subject(205, 90)
  )))

  cmp <- pmx_compare(pmx_write_mrgsolve(m), tables)

  expect_true(cmp$pass)
  expect_equal(unique(cmp$predictions$id), c(101, 205))
  expect_equal(cmp$engines$engine, c("mrgsolve", "table"))
  expect_equal(nrow(cmp$predictions), 8)
})

test_that("a point agrees when the model rounds to the table's value", {
  expect_equal(
    table_resolution(c("1.2345E+00", "-1.2345E-02", "0.0000E+00", "12", "NaN")),
    c(5e-5, 5e-7, 5e-5, 0.5, NA)
  )
  obs <- data.frame(id = 1L, time = 1:3)

  p <- compare_points(
    "table",
    obs,
    c(1.2345, 1.2345, 1.2345),
    c(1.23455, 1.2344501, 1.234551),
    1e-7,
    0,
    resolution = table_resolution(rep("1.2345E+00", 3)) * (1 + 1e-9)
  )

  expect_equal(p$pass, c(TRUE, TRUE, FALSE))
})

test_that("tables are compared with a model, never with tables", {
  local_fake_engines()
  tables <- pmx_read_tables(local_nm_table(table_single_dose(1, 1)))
  m <- read_ctl(ctl_advan1())

  expect_error(pmx_compare(tables, tables), class = "zupmxio_model_error")
  expect_s3_class(pmx_compare(tables, m), "zupmxio_comparison")
  expect_null(pmx_compare(m, tables)$design)
})

test_that("tables must hold the model's covariates and compartments", {
  local_fake_engines()
  tables <- pmx_read_tables(local_nm_table(table_single_dose(1, 1)))
  m <- read_ctl(ctl_advan1(
    cl = "CL = THETA(1) * EXP(ETA(1)) * (WT / 70)**0.75"
  ))
  expect_error(pmx_compare(tables, m), class = "zupmxio_model_error")

  df <- table_single_dose(1, 1)
  df$CMT <- 2
  tables <- pmx_read_tables(local_nm_table(df))
  expect_error(
    pmx_compare(tables, read_ctl(ctl_advan1())),
    class = "zupmxio_model_error"
  )
})

test_that("a comparison with tables prints and reports its source", {
  local_fake_engines(rxode2 = function(time, id) time * (1 + 1e-3))
  path <- file.path(withr::local_tempdir(), "sdtab1")
  writeLines(nm_table_lines(table_single_dose(c(1, 2), c(1, 2))), path)
  model <- file.path(withr::local_tempdir(), "advan1.ctl")
  writeLines(ctl_advan1(), model)
  cmp <- pmx_compare(pmx_read_tables(path), pmx_read_nonmem(model))

  expect_snapshot({
    cmp
    writeLines(utils::head(pmx_report(cmp), 32))
  })
})

test_that("a fixed-format small value does not loosen the other points (#147)", {
  local_fake_engines(rxode2 = function(time, id) {
    c(1.2070368, 1.5582794, 1.5387683, 2e-7)[time]
  })
  file <- withr::local_tempfile(fileext = ".tab")
  writeLines(
    c(
      "ID TIME AMT EVID PRED",
      "1 0 100 1 0",
      "1 1 0 0 1.6070368",
      "1 2 0 0 1.9582794",
      "1 3 0 0 1.9387683",
      "1 4 0 0 0.0000002"
    ),
    file
  )

  cmp <- pmx_compare(pmx_read_tables(file), read_ctl(ctl_advan1()))

  expect_false(cmp$pass)
  expect_equal(cmp$predictions$pass, c(FALSE, FALSE, FALSE, TRUE))
})

test_that("the endpoint column is matched exactly (#152)", {
  local_fake_engines()
  testthat::local_mocked_bindings(
    engine_rxode2 = function(text, data, etas) {
      obs <- data[data$evid == 0, ]
      data.frame(id = obs$id, time = obs$time, y = 1000, Y = obs$time)
    }
  )
  m <- read_ctl(ctl_advan1())

  cmp <- pmx_compare(m, m, pmx_design(n = 0, regimens = "single"))

  expect_true(cmp$pass)
  expect_equal(cmp$predictions$a, cmp$predictions$time)
})

test_that("tables with no observation records fail with a reason (#156)", {
  local_fake_engines()
  df <- table_single_dose(numeric(), numeric())
  tables <- pmx_read_tables(local_nm_table(df))

  cmp <- expect_no_warning(pmx_compare(tables, read_ctl(ctl_advan1())))

  expect_false(cmp$pass)
  expect_match(cmp$reason, "no observation")
})

test_that("a passing comparison reports its largest relative deviation as worst (#161)", {
  local_fake_engines(
    mrgsolve = function(time, id) {
      time * (1 + c(2e-7, 5e-7, 1e-7)[(time %% 3) + 1])
    }
  )
  m <- read_ctl(ctl_advan1())

  cmp <- pmx_compare(
    m,
    pmx_write_rxode2(m),
    pmx_design(n = 0, regimens = "single")
  )

  expect_true(cmp$pass)
  expect_true(all(cmp$predictions$abs > cmp$atol))
  expect_equal(cmp$worst$rel, max(cmp$predictions$rel))
})

test_that("pmx_compare() leaves the caller's random number stream as it was (#162)", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1())
  w <- pmx_write_mrgsolve(m)
  withr::local_seed(42)
  before <- .Random.seed

  pmx_compare(m, w, pmx_design(n = 2, regimens = "single"))

  expect_identical(.Random.seed, before)
})

test_that("EVID 2 records carry time-varying covariates to the engines (#169)", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1(
    cl = "CL = THETA(1) * EXP(ETA(1)) * (WT / 70)**0.75"
  ))
  k <- function(wt) 4 * (wt / 70)^0.75 / 50
  # NONMEM integrates each interval with the next record's values: WT 140
  # on (2, 3] from the EVID 2 record, then WT 70 on (3, 4].
  c2 <- 2 * exp(-2 * k(70))
  c4 <- c2 * exp(-k(140)) * exp(-k(70))
  df <- data.frame(
    ID = 1,
    TIME = c(0, 2, 3, 4),
    AMT = c(100, 0, 0, 0),
    EVID = c(1, 0, 2, 0),
    WT = c(70, 70, 140, 70),
    PRED = c(0, c2, 0, c4)
  )
  tables <- pmx_read_tables(local_nm_table(df, digits = 9))
  expect_equal(tables$records$evid, c(1L, 0L, 2L, 0L))

  expect_true(pmx_compare(tables, m)$pass)
  expect_true(pmx_compare(tables, pmx_write_mrgsolve(m))$pass)
})

test_that("a translation off by ten times tol fails through the engines (#165)", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- read_ctl(ctl_advan1())
  w <- pmx_write_rxode2(m)
  # V 50 to 50.0005: predictions move by about 1e-5 relative.
  w$text <- sub(
    "THETA2 <- c(0, 50)",
    "THETA2 <- c(0, 50.0005)",
    w$text,
    fixed = TRUE
  )

  cmp <- pmx_compare(m, w, pmx_design(n = 0, regimens = "single"))

  expect_false(cmp$pass)
  expect_lt(cmp$worst$rel, 1e-4)
  expect_gt(cmp$worst$rel, 1e-6)
  expect_true(
    pmx_compare(m, w, pmx_design(n = 0, regimens = "single"), tol = 1e-4)$pass
  )
})

test_that("a model runs its own source when its engine may run it (#170)", {
  local_fake_engines()
  file <- corpus_file("onecmt-oral", "model.cpp")
  mrg <- pmx_read_mrgsolve(file)
  rx <- corpus_rxode2_model("onecmt-oral")
  d <- pmx_design(n = 0, regimens = "single")

  cmp <- pmx_compare(mrg, pmx_write_rxode2(mrg), d)
  expect_equal(cmp$sides$a$engine, "mrgsolve")
  expect_match(cmp$sides$a$origin, "mrgsolve source, simulated as read")
  expect_identical(cmp$sides$a$text, readLines(file))

  cmp <- pmx_compare(rx, pmx_write_mrgsolve(rx), d)
  expect_match(cmp$sides$a$origin, "rxode2 source, simulated as read")

  # D4: not on the engine of the writer under test.
  cmp <- pmx_compare(mrg, pmx_write_mrgsolve(mrg), d)
  expect_equal(cmp$sides$a$engine, "rxode2")
  expect_match(cmp$sides$a$origin, "written to rxode2 by zupmxio")

  # An edited model is not its source any more.
  edited <- unclass(mrg)
  edited$parameters$init[[1]] <- edited$parameters$init[[1]] * 2
  edited <- do.call(new_model, edited)
  cmp <- pmx_compare(edited, pmx_write_rxode2(edited), d)
  expect_match(cmp$sides$a$origin, "written to mrgsolve by zupmxio")
})

test_that("a table's subject IDs need not be integers (#173)", {
  local_fake_engines(rxode2 = function(time, id) time * 2)
  tables <- pmx_read_tables(local_nm_table(table_single_dose(1, 1, id = 1.5)))
  cmp <- pmx_compare(tables, read_ctl(ctl_advan1()))

  expect_false(cmp$pass)
  expect_output(print(cmp), "subject 1.5")
  expect_true(any(grepl("subject 1.5", pmx_report(cmp))))
})

test_that("a NONMEM model with estimates compares with its translations (#182)", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run2.ctl"))
  d <- pmx_design(n = 2, regimens = c("single", "ss"))

  expect_true(pmx_compare(m, pmx_write_mrgsolve(m), d)$pass)
  expect_true(pmx_compare(m, pmx_write_rxode2(m), d)$pass)
})
