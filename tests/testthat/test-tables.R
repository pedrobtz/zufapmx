test_that("a NONMEM table reads into records, with its precision", {
  file <- withr::local_tempfile(fileext = ".tab")
  # NONMEM's layout, trailing spaces and all.
  writeLines(
    c(
      "TABLE NO.  1",
      " ID          TIME        AMT         EVID        CMT         WT          PRED        DV          ",
      "  1.0000E+00  0.0000E+00  1.0000E+02  1.0000E+00  1.0000E+00  7.0000E+01  0.0000E+00  0.0000E+00",
      "  1.0000E+00  5.0000E-01  0.0000E+00  0.0000E+00  1.0000E+00  7.0000E+01  1.2345E+00  1.3000E+00",
      "  2.0000E+00  0.0000E+00  1.0000E+02  1.0000E+00  1.0000E+00  8.0000E+01  0.0000E+00  0.0000E+00",
      "  2.0000E+00  5.0000E-01  0.0000E+00  0.0000E+00  1.0000E+00  8.0000E+01  1.1111E-01  1.0000E-01"
    ),
    file
  )

  t <- pmx_read_tables(file)

  expect_s3_class(t, "zupmxio_tables")
  expect_equal(t$digits, 5)
  expect_equal(t$prediction, "PRED")
  r <- t$records
  expect_named(
    r,
    c(
      "id",
      "time",
      "evid",
      "amt",
      "cmt",
      "rate",
      "ii",
      "addl",
      "ss",
      "pred",
      "WT",
      "DV"
    )
  )
  expect_equal(r$id, c(1, 1, 2, 2))
  expect_equal(r$evid, c(1L, 0L, 1L, 0L))
  expect_equal(r$pred, c(0, 1.2345, 0, 0.11111))
  expect_equal(r$WT, c(70, 70, 80, 80))
  expect_equal(r$rate, c(0, 0, 0, 0))
})

test_that("zucsv and base R read the same table", {
  skip_if_no_engine("zucsv")
  df <- table_single_dose(c(0.5, 1, 2.25), c(1.5, 2.123456789, 3e-7))
  df$WT <- 72.5
  file <- local_nm_table(df, digits = 9)

  with_zucsv <- pmx_read_tables(file)
  testthat::local_mocked_bindings(
    engine_available = function(engine) engine != "zucsv"
  )
  without <- pmx_read_tables(file)

  expect_identical(without, with_zucsv)
  expect_equal(with_zucsv$digits, 9)
  expect_equal(with_zucsv$records$pred[[3]], 2.12345679)
})

test_that("comma-separated tables and NOTITLE tables read too", {
  file <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("ID,TIME,AMT,PRED", "1,0,100,0", "1,1.5,0,2.50"), file)

  t <- pmx_read_tables(file)

  expect_equal(t$records$time, c(0, 1.5))
  expect_equal(t$records$evid, c(1L, 0L))
  # "2.50" has 3 significant digits; "0" has none to count.
  expect_equal(t$digits, 3)
})

test_that("without EVID, AMT and MDV say what a record is", {
  df <- data.frame(
    ID = 1,
    TIME = c(0, 1, 2, 3),
    AMT = c(100, 0, 0, 0),
    MDV = c(1, 0, 1, 0),
    PRED = c(0, 1, 9, 2)
  )

  r <- pmx_read_tables(local_nm_table(df))$records

  # The MDV 1 record without a dose is neither, and is dropped.
  expect_equal(r$time, c(0, 1, 3))
  expect_equal(r$evid, c(1L, 0L, 0L))
  expect_equal(r$pred, c(0, 1, 2))
})

test_that("EVID 2 records are dropped and resets refused", {
  df <- table_single_dose(c(1, 2), c(1, 2))
  df$EVID[[2]] <- 2
  expect_equal(pmx_read_tables(local_nm_table(df))$records$time, c(0, 2))

  df$EVID[[2]] <- 3
  expect_error(
    pmx_read_tables(local_nm_table(df)),
    class = "zupmxio_parse_error"
  )
  df$EVID[[2]] <- 4
  expect_error(
    pmx_read_tables(local_nm_table(df)),
    class = "zupmxio_parse_error"
  )
  df$EVID[[2]] <- 5
  expect_error(
    pmx_read_tables(local_nm_table(df)),
    class = "zupmxio_parse_error"
  )
})

test_that("tables from one run read side by side", {
  df <- table_single_dose(c(1, 2), c(1, 2))
  sd <- local_nm_table(df[c("ID", "TIME", "AMT", "EVID", "PRED")])
  pa <- local_nm_table(data.frame(ID = df$ID, CL = 4, V = 50))

  t <- pmx_read_tables(c(sd, pa))

  expect_equal(t$records$CL, c(4, 4, 4))
  expect_equal(t$files, normalizePath(c(sd, pa)))

  short <- local_nm_table(data.frame(ID = 1, CL = 4))
  expect_error(pmx_read_tables(c(sd, short)), class = "zupmxio_parse_error")
  other <- local_nm_table(data.frame(ID = c(1, 1, 2), CL = 4))
  expect_error(pmx_read_tables(c(sd, other)), class = "zupmxio_parse_error")
})

test_that("the prediction column matches without regard to case", {
  df <- table_single_dose(1, 2)
  names(df)[names(df) == "PRED"] <- "pred"
  df$IPRED <- c(0, 3)
  file <- local_nm_table(df)

  expect_equal(pmx_read_tables(file)$records$pred, c(0, 2))
  expect_equal(pmx_read_tables(file, "IPRED")$records$pred, c(0, 3))
})

test_that("malformed tables are parse errors at their line", {
  lines <- nm_table_lines(table_single_dose(c(1, 2), c(1, 2)))
  read_lines <- function(x) {
    file <- withr::local_tempfile(fileext = ".tab")
    writeLines(x, file)
    pmx_read_tables(file)
  }
  refused <- function(x, line = NULL) {
    err <- expect_error(read_lines(x), class = "zupmxio_parse_error")
    if (!is.null(line)) {
      expect_equal(err$line, line)
    }
  }

  refused(c(lines, lines[[1]]), line = 6L) # a second table
  refused(lines[-2], line = 2L) # NOHEADER: the first line is numbers
  refused(c(lines[1:3], paste(lines[[4]], "1.0E+00")), line = 4L)
  refused(c(lines[1:2], sub("1.0000E+00", "x", lines[[3]], fixed = TRUE)))
  refused(c(lines[1:2], sub("1.0000E+00", "NaN", lines[[3]], fixed = TRUE)))
  refused(character()) # empty
  refused("TABLE NO.  1")
  refused(sub("CMT", "AMT", lines[1:4]), line = 2L) # a column named twice
  refused(sub("ID ", "XX ", lines))
  refused(sub("TIME", "TTTT", lines))
  refused(sub("PRED", "IPRE", lines))
})

test_that("records out of NONMEM's order are refused", {
  df <- rbind(
    table_single_dose(1, 1, id = 1),
    table_single_dose(1, 1, id = 2),
    table_single_dose(1, 1, id = 1)
  )
  expect_error(
    pmx_read_tables(local_nm_table(df)),
    class = "zupmxio_parse_error"
  )

  df <- table_single_dose(c(2, 1), c(1, 1))
  expect_error(
    pmx_read_tables(local_nm_table(df)),
    class = "zupmxio_parse_error"
  )
})

test_that("only the prediction may be missing", {
  df <- table_single_dose(c(1, 2), c(1, 2))
  lines <- nm_table_lines(df)
  file <- withr::local_tempfile(fileext = ".tab")
  writeLines(sub("2\\.0000E\\+00$", "NaN", lines), file)
  expect_true(is.na(pmx_read_tables(file)$records$pred[[3]]))
})

test_that("pmx_read_tables() refuses malformed arguments", {
  file <- local_nm_table(table_single_dose(1, 1))
  for (bad in list(1, NA_character_, character(), "no-such.tab", tempdir())) {
    expect_error(pmx_read_tables(bad), class = "zupmxio_model_error")
  }
  for (bad in list(NA_character_, "", c("PRED", "DV"), 1)) {
    expect_error(pmx_read_tables(file, bad), class = "zupmxio_model_error")
  }
})

test_that("a table prints its subjects, records and precision", {
  path <- file.path(withr::local_tempdir(), "sdtab1")
  writeLines(nm_table_lines(table_single_dose(c(1, 2), c(1, 2))), path)
  expect_snapshot(pmx_read_tables(path))
})
