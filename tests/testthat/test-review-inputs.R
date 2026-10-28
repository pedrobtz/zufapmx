# Inputs the critical review found raising bare or wrongly classed errors,
# or being accepted when they should be refused.

test_that("initial values need one to three values in bound order", {
  ini_model <- function(value) {
    f <- function() NULL
    body(f) <- call(
      "{",
      call("ini", call("{", call("<-", quote(tcl), value), quote(sd <- 0.1))),
      quote(model({
        cl <- tcl
        v <- 1
        cp <- linCmt()
        cp ~ add(sd)
      }))
    )
    f
  }
  expect_error(
    pmx_read_rxode2(ini_model(quote(c(0, 4, 10, 99)))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(ini_model(quote(c(10, 5)))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(ini_model(quote(c()))),
    class = "zupkpmx_parse_error"
  )

  expect_error(
    read_ctl(sub(
      "^\\$THETA .*",
      "$THETA (10, 5) (0, 4) (0, 50)",
      ctl_onecmt()
    )),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(c(
      "$PARAM @annotated",
      "TVCL : 5 : [10, 20]",
      "TVV : 50 : [0, Inf]",
      cpp_onecmt()[-1]
    )),
    class = "zupkpmx_parse_error"
  )
})

test_that("residual error parameters must be ini() parameters", {
  expect_error(
    pmx_read_rxode2(function() {
      ini({
        tcl <- 1
      })
      model({
        cl <- tcl
        v <- 1
        cp <- linCmt()
        cp ~ prop(prop.sd)
      })
    }),
    class = "zupkpmx_parse_error"
  )
})

test_that("annotated parameter names must be names", {
  expect_error(
    read_cpp(c(
      "$PARAM @annotated",
      "q <- 1; message('x'); TVCL : 1 : x",
      cpp_onecmt()[-1]
    )),
    class = "zupkpmx_parse_error"
  )
})

test_that("a writer whose text does not read back raises a write error", {
  m <- model_twocmt_oral()
  p <- pmx_parameters(m)
  p$name[[1]] <- "if"
  ind <- unclass(m)$individual
  ind[[1]] <- quote(ka <- `if`)

  expect_error(
    pmx_write_rxode2(model_with(m, parameters = p, individual = ind)),
    class = "zupkpmx_write_error"
  )
})

test_that("malformed input is a parse error, never a bare R error", {
  expect_error(read_ctl(character()), class = "zupkpmx_parse_error")
  expect_error(read_cpp(character()), class = "zupkpmx_parse_error")
  expect_error(
    read_ctl(ctl_onecmt(pk = "S0 = V")),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_ctl(ctl_onecmt(pk = "DADT(1) = 1")),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      "$PKMODEL ncmt = 4",
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED + EPS(1);"
    )),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      "$PKMODEL ncmt = 0",
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED + EPS(1);"
    )),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = exp(CL, V);")),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(c("$PARAM TVCL = 1", cpp_onecmt())),
    class = "zupkpmx_parse_error"
  )

  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_error(
    read_ctl(c(lines, "$DES", "DADT(3) = 1")),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_ctl(sub("DADT\\(2\\)", "DADT(3)", lines)),
    class = "zupkpmx_parse_error"
  )

  dir <- withr::local_tempdir()
  file.copy(test_path("fixtures", "nonmem", "run1.ctl"), dir)
  writeLines(character(), file.path(dir, "run1.ext"))
  expect_error(
    pmx_read_nonmem(file.path(dir, "run1.ctl"), "ext"),
    class = "zupkpmx_parse_error"
  )
})

test_that("estimates are found next to a control stream in a dotted directory", {
  dir <- file.path(withr::local_tempdir(), "runs.v2")
  dir.create(dir)
  file.copy(test_path("fixtures", "nonmem", c("run1.ctl", "run1.ext")), dir)
  file.rename(file.path(dir, "run1.ctl"), file.path(dir, "run1"))

  m <- pmx_read_nonmem(file.path(dir, "run1"), "ext")
  expect_equal(basename(unclass(m)$provenance$estimates), "run1.ext")
})

test_that("C++ integer division and int declarations are refused", {
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = pow(CL / 70.0, 3 / 4);")),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = (1 + 2) / 4;")),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "int N = 2;")),
    class = "zupkpmx_parse_error"
  )

  m <- read_cpp(cpp_onecmt(
    main = c("double X = 3.0 / 4;", "double Z = 2 * CL;")
  ))
  expect_equal(unclass(m)$individual[[3]], quote(X <- 3 / 4))
  expect_identical(unclass(m)$individual[[4]][[3]][[2]], 2)
})
