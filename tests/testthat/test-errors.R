test_that("parse errors carry the file, position and tokens", {
  err <- tryCatch(
    parse_error(
      "run1.ctl",
      12L,
      5L,
      seen = "ELSEIF",
      expected = c("ELSE", "ENDIF")
    ),
    error = identity
  )

  expect_s3_class(err, c("zupkpmx_parse_error", "zupkpmx_error", "error"))
  expect_equal(err$file, "run1.ctl")
  expect_equal(err$line, 12L)
  expect_equal(err$column, 5L)
  expect_equal(err$seen, "ELSEIF")
  expect_equal(err$expected, c("ELSE", "ENDIF"))
  expect_snapshot(cat(conditionMessage(err)))
})

test_that("a parse error can explain itself instead of listing tokens", {
  err <- tryCatch(
    parse_error(
      "run1.ctl",
      40L,
      1L,
      seen = "VERBATIM",
      message = "not translated: verbatim Fortran at line 40"
    ),
    error = identity
  )

  expect_s3_class(err, "zupkpmx_parse_error")
  expect_equal(
    conditionMessage(err),
    "run1.ctl:40:1: not translated: verbatim Fortran at line 40"
  )
})

test_that("write errors carry the construct, location and formats", {
  err <- tryCatch(
    write_error(
      "inter-occasion variability",
      "random_effects$iov",
      formats = character()
    ),
    error = identity
  )

  expect_s3_class(err, c("zupkpmx_write_error", "zupkpmx_error", "error"))
  expect_equal(err$construct, "inter-occasion variability")
  expect_equal(err$location, "random_effects$iov")
  expect_equal(err$formats, character())
  expect_snapshot(cat(conditionMessage(err)))
})

test_that("engine errors carry the engine and its own error", {
  parent <- simpleError("compilation failed")
  err <- tryCatch(engine_error("mrgsolve", parent), error = identity)

  expect_s3_class(err, c("zupkpmx_engine_error", "zupkpmx_error", "error"))
  expect_equal(err$engine, "mrgsolve")
  expect_identical(err$parent, parent)
  expect_snapshot(cat(conditionMessage(err)))
})

test_that("an engine error without a parent says the engine is missing", {
  err <- tryCatch(engine_error("rxode2"), error = identity)

  expect_s3_class(err, "zupkpmx_engine_error")
  expect_null(err$parent)
  expect_snapshot(cat(conditionMessage(err)))
})
