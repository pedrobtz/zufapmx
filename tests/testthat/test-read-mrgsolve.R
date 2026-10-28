test_that("every corpus mrgsolve model reads", {
  for (path in zupkpmx_corpus()$path) {
    expect_s3_class(
      pmx_read_mrgsolve(file.path(path, "model.cpp")),
      "zupkpmx_model"
    )
  }
})

test_that("closed-form and ODE models read into their structures", {
  corpus <- zupkpmx_corpus()
  closed <- pmx_read_mrgsolve(file.path(
    corpus$path[corpus$id == "twocmt-oral"],
    "model.cpp"
  ))
  ode <- pmx_read_mrgsolve(file.path(
    corpus$path[corpus$id == "onecmt-oral-mm"],
    "model.cpp"
  ))

  expect_equal(
    unclass(closed)$structure$parameters,
    c(cl = "CL", v = "V2", q = "Q", vp = "V3", ka = "KA")
  )
  expect_equal(unclass(closed)$observations$Y$prediction, quote(central / V2))
  expect_equal(names(pmx_ode(ode)), c("DEPOT", "CENT"))
  expect_equal(unclass(ode)$structure$statements, list(quote(C2 <- CENT / V)))
  expect_snapshot(print(closed))
  expect_snapshot(print(ode))
})

test_that("corpus mrgsolve models predict their expected.csv", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  corpus <- zupkpmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    model <- pmx_read_mrgsolve(file.path(corpus$path[[i]], "model.cpp"))
    got <- corpus_solve_rxode2(
      as_rxode2_function(model),
      corpus$regimen[[i]],
      "Y"
    )
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("annotated parameters carry bounds and fixed flags", {
  m <- read_cpp(c(
    "$PARAM @annotated",
    "TVCL : 4 : [0, Inf] fixed",
    "TVV : 50 : [1, 100]",
    "TVK : log(2) : rate constant",
    cpp_onecmt()[-1]
  ))
  p <- pmx_parameters(m)

  expect_equal(p$init, c(4, 50, log(2)))
  expect_equal(p$lower, c(0, 1, -Inf))
  expect_equal(p$upper, c(Inf, 100, Inf))
  expect_equal(p$fixed, c(TRUE, FALSE, FALSE))
  expect_true("parameter descriptions" %in% unclass(m)$provenance$losses)
})

test_that("C++ statements, macros and blocks read", {
  m <- read_cpp(cpp_onecmt(
    main = c(
      "double WTF = 0.0;",
      "if (WT > 70 && SEX == 1) {",
      "  WTF = pow(WT / 70.0, 0.75);",
      "} else if (WT > 50) WTF = 1.1;",
      "else {",
      "  WTF = fmax(fabs(WT), 1.0f);",
      "}",
      "F_CENT = 0.8; // bioavailability",
      "/* lag */ ALAG_CENT = 0.5;",
      "CENT_0 = 10;"
    ),
    extra = c("$OMEGA @block", "0.1 0.01 0.2")
  ))
  ind <- unclass(m)$individual

  expect_equal(ind[[4]][[2]], quote(WT > 70 & SEX == 1))
  expect_equal(
    ind[[4]][[3]][[2]],
    call("<-", quote(WTF), call("^", quote(WT / 70), 0.75))
  )
  expect_equal(
    unclass(m)$dosing$CENT,
    list(bioavailability = 0.8, lag = 0.5, initial = 10)
  )
  expect_equal(pmx_omega(m)["ETA2", "ETA3"], 0.01)
})

test_that("residual models are recognised in $TABLE", {
  residual <- function(table) {
    unclass(read_cpp(cpp_onecmt(
      table = table,
      extra = "$SIGMA 0.01"
    )))$observations$Y$residual
  }

  expect_equal(
    residual(c("double IPRED = CENT / V;", "double Y = IPRED + EPS(1);"))$type,
    "additive"
  )
  expect_equal(
    residual(c(
      "double IPRED = CENT / V;",
      "double Y = IPRED * (1 + EPS(1)) + EPS(2);"
    )),
    list(type = "combined", parameters = c("EPS2", "EPS1"))
  )
  expect_equal(
    residual(c("double CP = CENT / V;", "double Y = CP * exp(EPS(1));"))$type,
    "exponential"
  )
})

test_that("$NMEXT reads a NONMEM run's estimates as parameters", {
  ext <- normalizePath(test_path("fixtures", "nonmem", "run1.ext"))
  m <- read_cpp(c(
    sprintf("$NMEXT path = \"%s\"", ext),
    "$PKMODEL advan = 4, trans = 4, cmt = \"DEPOT CENT PERIPH\"",
    "$MAIN",
    "double KA = THETA1;",
    "double CL = THETA2 * exp(ETA(1));",
    "double V2 = THETA3 * exp(ETA(2));",
    "double Q = THETA4;",
    "double V3 = THETA5;",
    "$TABLE",
    "double IPRED = CENT / V2;",
    "double Y = IPRED * (1 + EPS(1));"
  ))

  expect_equal(pmx_parameters(m)$estimate, c(1.12, 4.25, 21.5, 2.15, 41.5))
  expect_equal(diag(pmx_omega(m)), c(ETA1 = 0.084, ETA2 = 0.096))
  expect_equal(unclass(m)$provenance$estimates, ext)
})

test_that("code outside mrgsolve's documented C++ is refused", {
  refused <- function(lines) {
    expect_error(read_cpp(lines), class = "zupkpmx_parse_error")
  }
  base <- cpp_onecmt()

  refused(c("$GLOBAL", "double z = 1;", base))
  refused(c("$PLUGIN nm-vars", base))
  refused(cpp_onecmt(main = "double X = sin(CL);"))
  refused(cpp_onecmt(main = "for (int i = 0; i < 2; i++) CL = CL;"))
  refused(cpp_onecmt(main = "double X = CL > 1 ? 1 : 2;"))
  refused(cpp_onecmt(table = "double IPRED = CENT / V;"))
  refused(base[base != "dxdt_CENT = -CL / V * CENT;"])
  refused(c(base, "$OMEGA @correlation", "0.1 0.5 0.1"))
  refused(c(
    "$PARAM CL = 1, V1 = 2, Q = 1, V2 = 3",
    "$PKMODEL ncmt = 2, cmt = \"CENT PERIPH\"",
    "$TABLE",
    "double IPRED = CENT / V1;",
    "double Y = IPRED + EPS(1);"
  ))
})
