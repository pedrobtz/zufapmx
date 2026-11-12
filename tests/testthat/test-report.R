test_that("a report holds the verdict, engines, design and both texts", {
  local_fake_engines()
  m <- read_ctl(ctl_advan1())
  w <- pmx_write_rxode2(m)
  cmp <- pmx_compare(m, w, pmx_design(n = 1, covariates = list(WT = 70)))
  file <- withr::local_tempfile(fileext = ".md")

  lines <- pmx_report(cmp, file)

  expect_identical(readLines(file), lines)
  expect_true(
    "**Result: pass.** Every prediction agrees within the tolerances." %in%
      lines
  )
  expect_true(any(grepl("seed 1", lines, fixed = TRUE)))
  expect_true(any(grepl("Covariates: WT = 70.", lines, fixed = TRUE)))
  expect_true(any(grepl("| rxode2 0.0.0 |", lines, fixed = TRUE)))
  # The text each engine ran, whole and in order.
  b <- match("## Model b", lines)
  expect_identical(lines[b + 3:(2 + length(w$text))], w$text)
  expect_identical(lines[[b + 2]], "```r")
  a <- match("## Model a", lines)
  expect_identical(lines[[a + 2]], "```cpp")
  expect_identical(
    lines[a + 3:(2 + length(cmp$sides$a$text))],
    cmp$sides$a$text
  )
})

test_that("a failing report lists the points that disagree, up to max_points", {
  local_fake_engines(mrgsolve = function(time, id) time * 1.01)
  m <- read_ctl(ctl_advan1())
  cmp <- pmx_compare(m, pmx_write_rxode2(m), pmx_design(n = 0))
  n <- nrow(cmp$predictions)

  lines <- pmx_report(cmp, max_points = 3)

  expect_true(any(grepl(sprintf("%d of %d predictions disagree", n, n), lines)))
  table <- match("### Points that disagree", lines)
  expect_equal(
    sum(grepl(
      "^\\| (single|infusion|multiple|ss) \\|",
      lines[-seq_len(table)]
    )),
    3
  )
  expect_true(sprintf("And %d more.", n - 3) %in% lines)

  lines <- pmx_report(cmp, max_points = 0)
  expect_true(sprintf("And %d more.", n) %in% lines)
  lines <- pmx_report(cmp, max_points = n)
  expect_false(any(grepl("^And ", lines)))
})

test_that("a comparison that could not run point by point reports why", {
  local_fake_engines()
  a <- read_ctl(ctl_advan1())
  lines <- sub("THETA(2) * EXP(ETA(2))", "THETA(2)", ctl_advan1(), fixed = TRUE)
  b <- read_ctl(sub("$OMEGA 0.09 0.04", "$OMEGA 0.09", lines, fixed = TRUE))

  report <- pmx_report(pmx_compare(a, b, pmx_design(n = 0)))

  expect_true(
    "**Result: FAIL.** The models have 2 and 1 random effects." %in% report
  )
  expect_false("## Results" %in% report)
})

test_that("a text holding backticks gets a longer fence", {
  side <- list(engine = "rxode2", text = c("x <- 1 # ```", "y <- 2"))

  out <- report_text(side)

  expect_identical(out[[1]], "````r")
  expect_identical(out[[length(out)]], "````")
})

test_that("table cells escape the pipe", {
  expect_identical(
    md_table(data.frame(a = "x | y", b = 1)),
    c("| a | b |", "| --- | --- |", "| x \\| y | 1 |")
  )
})

test_that("pmx_report() refuses malformed arguments", {
  local_fake_engines()
  m <- read_ctl(ctl_advan1())
  cmp <- pmx_compare(m, m, pmx_design(n = 0, regimens = "single"))

  expect_error(pmx_report(list()), class = "zupmxio_model_error")
  expect_error(pmx_report(cmp, file = 1), class = "zupmxio_model_error")
  expect_error(
    pmx_report(cmp, file = NA_character_),
    class = "zupmxio_model_error"
  )
  expect_error(pmx_report(cmp, file = ""), class = "zupmxio_model_error")
  expect_error(
    pmx_report(cmp, file = c("a", "b")),
    class = "zupmxio_model_error"
  )
  expect_error(pmx_report(cmp, max_points = -1), class = "zupmxio_model_error")
  expect_error(pmx_report(cmp, max_points = 1.5), class = "zupmxio_model_error")
  expect_error(pmx_report(cmp, max_points = Inf), class = "zupmxio_model_error")
  expect_error(
    pmx_report(cmp, max_points = .Machine$integer.max + 1),
    class = "zupmxio_model_error"
  )
  expect_error(pmx_report(cmp, max_points = NA), class = "zupmxio_model_error")
})

test_that("a report reads as it did", {
  local_fake_engines(mrgsolve = function(time, id) time * (1 + 1e-3))
  # A fixed file name: the report names the source file.
  path <- file.path(withr::local_tempdir(), "advan1.ctl")
  writeLines(ctl_advan1(), path)
  m <- pmx_read_nonmem(path)
  cmp <- pmx_compare(
    m,
    pmx_write_rxode2(m),
    pmx_design(n = 0, regimens = c("single", "ss"))
  )

  expect_snapshot(writeLines(pmx_report(cmp, max_points = 2)))
})

test_that("a report does not depend on the caller's options or locale", {
  local_fake_engines(mrgsolve = function(time, id) time * (1 + 1.23456789e-4))
  path <- file.path(withr::local_tempdir(), "advan1.ctl")
  writeLines(ctl_advan1(), path)
  m <- pmx_read_nonmem(path)
  cmp <- pmx_compare(
    m,
    pmx_write_rxode2(m),
    pmx_design(n = 0, regimens = "single", covariates = list(WT = 72.5)),
    tol = 1.5e-7
  )
  ref <- pmx_report(cmp)

  withr::local_options(OutDec = ",", scipen = -5, digits = 3)
  expect_identical(pmx_report(cmp), ref)
  withr::local_locale(c(LC_NUMERIC = "C", LC_COLLATE = "C"))
  expect_identical(pmx_report(cmp), ref)
  # Predictions keep 10 significant digits.
  expect_true(any(grepl("| 0.5000617284 |", ref, fixed = TRUE)))
  expect_true(any(grepl("at most 1.5e-07", ref, fixed = TRUE)))
})
