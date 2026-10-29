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
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(ini_model(quote(c(10, 5)))),
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(ini_model(quote(c()))),
    class = "zufapmx_parse_error"
  )

  expect_error(
    read_ctl(sub(
      "^\\$THETA .*",
      "$THETA (10, 5) (0, 4) (0, 50)",
      ctl_onecmt()
    )),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(c(
      "$PARAM @annotated",
      "TVCL : 5 : [10, 20]",
      "TVV : 50 : [0, Inf]",
      cpp_onecmt()[-1]
    )),
    class = "zufapmx_parse_error"
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
    class = "zufapmx_parse_error"
  )
})

test_that("annotated parameter names must be names", {
  expect_error(
    read_cpp(c(
      "$PARAM @annotated",
      "q <- 1; message('x'); TVCL : 1 : x",
      cpp_onecmt()[-1]
    )),
    class = "zufapmx_parse_error"
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
    class = "zufapmx_write_error"
  )
})

test_that("malformed input is a parse error, never a bare R error", {
  expect_error(read_ctl(character()), class = "zufapmx_parse_error")
  expect_error(read_cpp(character()), class = "zufapmx_parse_error")
  expect_error(
    read_ctl(ctl_onecmt(pk = "S0 = V")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_ctl(ctl_onecmt(pk = "DADT(1) = 1")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      "$PKMODEL ncmt = 4",
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED + EPS(1);"
    )),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      "$PKMODEL ncmt = 0",
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED + EPS(1);"
    )),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = exp(CL, V);")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(c("$PARAM TVCL = 1", cpp_onecmt())),
    class = "zufapmx_parse_error"
  )

  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_error(
    read_ctl(c(lines, "$DES", "DADT(3) = 1")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_ctl(sub("DADT\\(2\\)", "DADT(3)", lines)),
    class = "zufapmx_parse_error"
  )

  dir <- withr::local_tempdir()
  file.copy(test_path("fixtures", "nonmem", "run1.ctl"), dir)
  writeLines(character(), file.path(dir, "run1.ext"))
  expect_error(
    pmx_read_nonmem(file.path(dir, "run1.ctl"), "ext"),
    class = "zufapmx_parse_error"
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
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = (1 + 2) / 4;")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "int N = 2;")),
    class = "zufapmx_parse_error"
  )

  m <- read_cpp(cpp_onecmt(
    main = c("double X = 3.0 / 4;", "double Z = 2 * CL;")
  ))
  expect_equal(unclass(m)$individual[[3]], quote(X <- 3 / 4))
  expect_identical(unclass(m)$individual[[4]][[3]][[2]], 2)
})

test_that("undeclared or fractional THETA, ETA and EPS indices are refused", {
  expect_error(
    read_ctl(ctl_onecmt(pk = "X = THETA(9)")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_ctl(ctl_onecmt(pk = "X = ETA(5)")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_ctl(ctl_onecmt(pk = "X = THETA(1.5)")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = ETA(4);")),
    class = "zufapmx_parse_error"
  )
})

test_that("other free symbols are recorded as covariates", {
  m <- read_ctl(ctl_onecmt(pk = "X = WT / 70"))
  expect_equal(unclass(m)$provenance$covariates, "WT")
})

test_that("time is one symbol, spelled for each format and block (D15)", {
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  lines <- sub(
    "VM = THETA(2) * EXP(ETA(1))",
    "VM = THETA(2) * EXP(ETA(1)) * (1 + 0.01 * TIME)",
    lines,
    fixed = TRUE
  )
  lines <- sub(
    "DADT(1) = -KA * A(1)",
    "DADT(1) = -KA * A(1) * EXP(-0.1 * T)",
    lines,
    fixed = TRUE
  )
  m <- read_ctl(lines)

  expect_true("t" %in% all.vars(unclass(m)$individual[[2]]))
  expect_true("t" %in% all.vars(pmx_ode(m)$DEPOT))
  expect_equal(pmx_fidelity(pmx_write_nonmem(m)), "exact")
  nm <- pmx_write_nonmem(m)$text
  expect_true(any(grepl("(1.0 + 0.01 * TIME)", nm, fixed = TRUE)))
  expect_true(any(grepl("EXP(-0.1 * T)", nm, fixed = TRUE)))
  mrg <- pmx_write_mrgsolve(m)$text
  expect_true(any(grepl("0.01 * TIME", mrg, fixed = TRUE)))
  expect_true(any(grepl("exp(-0.1 * SOLVERTIME)", mrg, fixed = TRUE)))
  expect_error(pmx_write_rxode2(m), class = "zufapmx_write_error")
})

test_that("rxode2 statements that read time belong to the ODE structure", {
  m <- pmx_read_rxode2(function() {
    ini({
      tk <- 0.1
      sd <- 0.1
    })
    model({
      k <- tk * exp(-0.1 * time)
      d / dt(central) <- -k * central
      cp <- central
      cp ~ add(sd)
    })
  })
  expect_equal(
    unclass(m)$structure$statements,
    list(quote(k <- tk * exp(-0.1 * t)))
  )
  expect_true(any(grepl("SOLVERTIME", pmx_write_mrgsolve(m)$text)))
  expect_true(any(grepl("$DES", pmx_write_nonmem(m)$text, fixed = TRUE)))
})
