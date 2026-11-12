test_that("the default design holds every regimen for 21 subjects", {
  d <- pmx_design()

  expect_s3_class(d, "zufapmx_design")
  expect_equal(d$regimens, c("single", "infusion", "multiple", "ss"))
  expect_equal(d$n, 20L)
  expect_equal(d$cmt, 1L)
  expect_equal(sort(unique(d$events$id)), 1:21)
  # One dose and 8, 8, 12 and 7 observations per subject.
  expect_equal(nrow(d$events), 21 * (9 + 9 + 13 + 8))
  expect_named(
    d$events,
    c("regimen", "id", "time", "evid", "amt", "rate", "ii", "addl", "ss")
  )
})

test_that("each regimen's dose record is the one documented", {
  d <- pmx_design(n = 0)
  dose <- d$events[d$events$evid == 1, ]
  rownames(dose) <- NULL

  expect_equal(dose$regimen, c("single", "infusion", "multiple", "ss"))
  expect_equal(dose$time, c(0, 0, 0, 0))
  expect_equal(dose$amt, c(100, 100, 100, 100))
  expect_equal(dose$rate, c(0, 50, 0, 0))
  expect_equal(dose$ii, c(0, 0, 12, 12))
  expect_equal(dose$addl, c(0, 0, 4, 0))
  expect_equal(dose$ss, c(0, 0, 0, 1))
})

test_that("no observation falls on a dose time", {
  d <- pmx_design(n = 0)
  obs <- d$events[d$events$evid == 0, ]

  expect_false(any(obs$time[obs$regimen == "multiple"] %in% seq(0, 48, 12)))
  expect_false(any(obs$time == 0))
  expect_true(all(obs$time[obs$regimen == "ss"] < 12))
})

test_that("n counts the subjects besides the typical one, up to 10000", {
  expect_equal(unique(pmx_design(n = 0)$events$id), 1L)
  expect_equal(pmx_design(n = 9999, regimens = "ss")$n, 9999L)
  expect_equal(pmx_design(n = 10000, regimens = "ss")$n, 10000L)
  expect_error(pmx_design(n = 10001), class = "zufapmx_model_error")
})

test_that("pmx_design() refuses malformed arguments", {
  bad <- list(
    list(n = -1),
    list(n = 1.5),
    list(n = NA),
    list(n = "2"),
    list(n = Inf),
    list(n = c(1, 2)),
    list(n = .Machine$integer.max + 1),
    list(seed = NA),
    list(seed = 1.5),
    list(seed = "1"),
    list(seed = Inf),
    list(seed = .Machine$integer.max + 1),
    list(regimens = "bolus"),
    list(regimens = character()),
    list(regimens = c("ss", "ss")),
    list(regimens = NA_character_),
    list(regimens = 1),
    list(cmt = 0),
    list(cmt = 1001),
    list(cmt = NA),
    list(covariates = list(70)),
    list(covariates = c(70)),
    list(covariates = list(WT = NA)),
    list(covariates = list(WT = c(70, 80))),
    list(covariates = list(WT = "70")),
    list(covariates = list(WT = 70, wt = 80)),
    list(covariates = "WT")
  )
  for (args in bad) {
    expect_error(
      do.call(pmx_design, args),
      class = "zufapmx_model_error",
      label = deparse(args)
    )
  }
})

test_that("cmt reaches 1000 and seed the integer range", {
  expect_equal(pmx_design(cmt = 1000)$cmt, 1000L)
  expect_equal(
    pmx_design(seed = .Machine$integer.max)$seed,
    .Machine$integer.max
  )
  expect_equal(pmx_design(seed = -3)$seed, -3)
})

test_that("covariates are a named list or a named numeric vector", {
  expect_equal(
    pmx_design(covariates = c(WT = 70, AGE = 40))$covariates,
    list(WT = 70, AGE = 40)
  )
  expect_equal(
    pmx_design(covariates = list(wt = 70L))$covariates,
    list(wt = 70)
  )
})

test_that("a design prints its regimens, subjects and covariates", {
  expect_snapshot({
    pmx_design()
    pmx_design(n = 3, seed = 7, regimens = "ss", covariates = list(WT = 70))
  })
})
