test_that("every corpus mrgsolve model reads", {
  for (path in zufapmx_corpus()$path) {
    expect_s3_class(
      pmx_read_mrgsolve(file.path(path, "model.cpp")),
      "zufapmx_model"
    )
  }
})

test_that("closed-form and ODE models read into their structures", {
  corpus <- zufapmx_corpus()
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
  skip_if_no_engine("rxode2")
  corpus <- zufapmx_corpus()

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
  ext <- normalizePath(
    test_path("fixtures", "nonmem", "run1.ext"),
    winslash = "/"
  )
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
  expect_equal(
    normalizePath(unclass(m)$provenance$estimates, winslash = "/"),
    ext
  )
})

test_that("code outside mrgsolve's documented C++ is refused", {
  base <- cpp_onecmt()

  expect_refused(read_cpp(c("$GLOBAL", "double z = 1;", base)), "$GLOBAL", 1)
  expect_refused(read_cpp(c("$PLUGIN nm-vars", base)), "nm-vars", 1)
  expect_refused(read_cpp(cpp_onecmt(main = "double X = sin(CL);")), "sin", 6)
  expect_refused(
    read_cpp(cpp_onecmt(main = "for (int i = 0; i < 2; i++) CL = CL;")),
    "for",
    6
  )
  expect_refused(
    read_cpp(cpp_onecmt(main = "double X = CL > 1 ? 1 : 2;")),
    "?",
    6
  )
  expect_refused(
    read_cpp(cpp_onecmt(table = "double IPRED = CENT / V;")),
    "$TABLE",
    10
  )
  expect_refused(
    read_cpp(base[base != "dxdt_CENT = -CL / V * CENT;"]),
    "$ODE",
    6
  )
  expect_refused(
    read_cpp(c(base, "$OMEGA @correlation", "0.1 0.5 0.1")),
    "@correlation",
    13
  )
  expect_refused(
    read_cpp(c(
      "$PARAM CL = 1, V1 = 2, Q = 1, V2 = 3",
      "$PKMODEL ncmt = 2, cmt = \"CENT PERIPH\"",
      "$TABLE",
      "double IPRED = CENT / V1;",
      "double Y = IPRED + EPS(1);"
    )),
    "ncmt = 2, cmt = \"CENT PERIPH\"",
    2
  )
})

test_that("dosing macros under $PKMODEL use the file's compartment names", {
  m <- pmx_read_mrgsolve(corpus_file("onecmt-oral-lag", "model.cpp"))

  expect_equal(
    unclass(m)$dosing,
    list(depot = list(lag = quote(TVLAG), bioavailability = quote(TVF)))
  )
  expect_false(any(
    c("ALAG_GUT", "F_GUT") %in% stmt_targets(unclass(m)$individual)
  ))
})

test_that("dosing macros inside if, or read as variables, are refused", {
  # Statements point at their record, $MAIN on line 3.
  expect_refused(
    read_cpp(cpp_onecmt(main = "if (CL > 1) F_CENT = 0.5;")),
    "F_CENT",
    3
  )
  expect_refused(
    read_cpp(cpp_onecmt(main = c("F_CENT = 0.5;", "double X = F_CENT * 2.0;"))),
    "F_CENT",
    3
  )
})

test_that("the $TABLE prediction is the term in F's place, fully expanded", {
  m <- read_cpp(cpp_onecmt(
    table = c(
      "double CP = CENT / V;",
      "double IPRED = CP;",
      "double Y = IPRED * (1 + EPS(1));"
    )
  ))
  expect_equal(unclass(m)$observations$Y$prediction, quote(CENT / V))

  expect_error(
    read_cpp(cpp_onecmt(
      table = c(
        "double IPRED = CENT / V;",
        "double W = 0.5;",
        "double Y = W * EPS(1) + IPRED;"
      )
    )),
    class = "zufapmx_parse_error"
  )
})

test_that("$PKMODEL parameters can come straight from $PARAM", {
  lines <- c(
    "$PARAM CL = 4, V = 50, KA = 1",
    "$PKMODEL ncmt = 1, depot = TRUE, cmt = \"GUT CENT\"",
    "$SIGMA 0.04",
    "$TABLE",
    "double IPRED = CENT / V;",
    "double Y = IPRED * (1 + EPS(1));"
  )
  m <- read_cpp(lines)

  expect_equal(
    unclass(m)$structure$parameters,
    c(cl = "CL", v = "V", ka = "KA")
  )
  expect_equal(pmx_fidelity(pmx_write_mrgsolve(m)), "exact")
  expect_true(all(
    c("CL = THETA(1)", "V = THETA(2)", "KA = THETA(3)") %in%
      pmx_write_nonmem(m)$text
  ))
  expect_faithful(pmx_write_rxode2(m))

  skip_on_cran()
  skip_if_no_engine("rxode2")
  got <- corpus_solve_rxode2(as_rxode2_function(m), "oral", "Y")
  expected <- corpus_expected(
    corpus_file("onecmt-oral", "expected.csv") |> dirname()
  )$pred
  expect_lte(corpus_max_rel_dev(got, expected), 1e-6)
})

test_that("Y is matched exactly in $TABLE", {
  expect_error(
    read_cpp(cpp_onecmt(
      table = c(
        "double IPRED = CENT / V;",
        "double YOBS = IPRED * (1 + EPS(1));"
      )
    )),
    class = "zufapmx_parse_error"
  )
})

test_that("$NMEXT resolves paths as mrgsolve does and numbers etas in file order", {
  dir <- withr::local_tempdir()
  file.copy(test_path("fixtures", "nonmem", "run1.ext"), dir)
  lines <- c(
    "$OMEGA 0.5",
    "$NMEXT path = \"run1.ext\", root = \"cppfile\"",
    "$PKMODEL advan = 4, trans = 4, cmt = \"DEPOT CENT PERIPH\"",
    "$MAIN",
    "double KA = THETA1 * exp(ETA(1));",
    "double CL = THETA2 * exp(ETA(2));",
    "double V2 = THETA3 * exp(ETA(3));",
    "double Q = THETA4;",
    "double V3 = THETA5;",
    "$TABLE",
    "double IPRED = CENT / V2;",
    "double Y = IPRED * (1 + EPS(1));"
  )
  path <- file.path(dir, "model.cpp")
  writeLines(lines, path)
  m <- pmx_read_mrgsolve(path)

  expect_equal(diag(pmx_omega(m)), c(ETA1 = 0.5, ETA2 = 0.084, ETA3 = 0.096))
  expect_equal(unclass(m)$random_effects$attach$parameter, c("KA", "CL", "V2"))

  withr::local_dir(dir)
  writeLines(sub(", root = \"cppfile\"", "", lines, fixed = TRUE), "model.cpp")
  expect_equal(
    pmx_parameters(pmx_read_mrgsolve("model.cpp"))$estimate[[1]],
    1.12
  )

  writeLines(c("$THETA 1", lines), "model.cpp")
  expect_error(pmx_read_mrgsolve("model.cpp"), class = "zufapmx_parse_error")
})

test_that("$NMXML needs zuxml", {
  local_mocked_bindings(has_zuxml = function() FALSE)
  xml <- normalizePath(
    test_path("fixtures", "nonmem", "run1.xml"),
    winslash = "/"
  )
  expect_error(
    read_cpp(c(sprintf("$NMXML path = \"%s\"", xml), cpp_onecmt())),
    class = "zufapmx_engine_error"
  )
})

test_that("a directory is not a model file", {
  expect_error(
    pmx_read_mrgsolve(withr::local_tempdir()),
    class = "zufapmx_model_error"
  )
  expect_error(
    pmx_read_nonmem(withr::local_tempdir()),
    class = "zufapmx_model_error"
  )
})

test_that("time is SOLVERTIME in $ODE and TIME elsewhere", {
  err <- expect_error(
    read_cpp(cpp_onecmt(main = "double K = SOLVERTIME;")),
    class = "zufapmx_parse_error"
  )
  expect_equal(err$seen, "SOLVERTIME")

  lines <- cpp_onecmt()
  lines <- sub("-CL / V * CENT", "-CL / V * CENT * TIME", lines, fixed = TRUE)
  err <- expect_error(read_cpp(lines), class = "zufapmx_parse_error")
  expect_equal(err$seen, "TIME")
})

test_that("$ODE beside $PKMODEL is refused", {
  err <- expect_error(
    read_cpp(c(
      "$PARAM CL = 1, V = 20, KA = 1, KE0 = 0.5",
      "$PKMODEL cmt = \"GUT CENT\", depot = TRUE",
      "$CMT EFF",
      "$ODE",
      "dxdt_EFF = KE0 * (CENT / V - EFF);",
      "$OMEGA 0.1",
      "$SIGMA 0.01",
      "$TABLE",
      "double Y = CENT / V * (1 + EPS(1));"
    )),
    class = "zufapmx_parse_error"
  )
  expect_equal(err$line, 4)
})

test_that("$PARAM values written as expressions are evaluated, with a loss", {
  m <- read_cpp(sub(
    "TVCL = 4",
    "TVCL = 4 * log(2)",
    cpp_onecmt(),
    fixed = TRUE
  ))
  expect_equal(pmx_parameters(m)$init[[1]], 4 * log(2))
  expect_true(any(grepl("evaluated", unclass(m)$provenance$losses)))

  expect_length(unclass(read_cpp(cpp_onecmt()))$provenance$losses, 0)
})

test_that("$NMXML and $NMEXT refuse options the reader does not honour", {
  for (option in c("omega = FALSE", "tname = \"x\"", "path = 1")) {
    err <- expect_error(
      read_cpp(c(sprintf("$NMEXT run = 1, %s", option), cpp_onecmt()[-1])),
      class = "zufapmx_parse_error",
      info = option
    )
    expect_equal(err$line, 1, info = option)
  }
  err <- expect_error(
    read_cpp(c("$NMEXT path = \"no-such.ext\"", cpp_onecmt())),
    class = "zufapmx_parse_error"
  )
  expect_equal(err$line, 1)
})

test_that("C++ comparisons are ints, and wide int literals are refused", {
  expect_error(
    read_cpp(cpp_onecmt(main = "double K = TVCL + (1 > 0) / 2;")),
    class = "zufapmx_parse_error"
  )
  expect_s3_class(
    read_cpp(cpp_onecmt(main = "double K = TVCL + (1 > 0) / 2.0;")),
    "zufapmx_model"
  )
  err <- expect_error(
    read_cpp(cpp_onecmt(main = "double K = 3000000000;")),
    class = "zufapmx_parse_error"
  )
  expect_equal(err$seen, "3000000000")
})

test_that("a $PARAM value cannot run code", {
  marker <- withr::local_tempfile()
  lines <- sub(
    "TVCL = 4",
    sprintf(
      "TVCL = file.create(\"%s\")",
      normalizePath(marker, winslash = "/", mustWork = FALSE)
    ),
    cpp_onecmt(),
    fixed = TRUE
  )
  expect_error(read_cpp(lines), class = "zufapmx_parse_error")
  expect_false(file.exists(marker))
})

test_that("$PKMODEL refuses an unknown advan or trans", {
  pkmodel <- function(spec) {
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      sprintf("$PKMODEL %s, cmt = \"CENT\"", spec),
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED + EPS(1);"
    ))
  }
  expect_refused(pkmodel("advan = 7, trans = 2"), "advan = 7", 2)
  expect_refused(pkmodel("advan = 1, trans = 9"), "trans = 9", 2)
})

test_that("a $PARAM item without = is refused", {
  expect_refused(
    read_cpp(sub("TVCL = 4", "TVCL 4", cpp_onecmt(), fixed = TRUE)),
    "TVCL 4",
    1
  )
})

test_that("$PKMODEL takes its compartment names from $CMT", {
  m <- read_cpp(c(
    "$PARAM CL = 1, V = 20, KA = 1",
    "$CMT GUT CENT",
    "$PKMODEL ncmt = 1, depot = TRUE",
    "$TABLE",
    "double IPRED = CENT / V;",
    "double Y = IPRED * (1 + EPS(1));"
  ))
  expect_equal(
    unclass(m)$structure$parameters,
    c(cl = "CL", v = "V", ka = "KA")
  )
  expect_equal(unclass(m)$observations$Y$prediction, quote(central / V))
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

test_that("mrgsolve blocks are refused with their position", {
  base <- cpp_onecmt()

  expect_refused(
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      "$PKMODEL advan = 4, trans = 4, cmt = \"A B\"",
      "$TABLE",
      "double IPRED = B / V;",
      "double Y = IPRED + EPS(1);"
    )),
    "A B",
    2
  )
  expect_refused(read_cpp(base[base != "$CMT CENT"]), "end of file", 11)
  expect_refused(read_cpp(c(base, "$OMEGA @block", "0.1 0.2")), "2 values", 13)
  # The refusal points at the first $ODE block, the record's start.
  expect_refused(read_cpp(c(base, "$ODE", "dxdt_GUT = 1;")), "dxdt_GUT", 6)
})

test_that("mrgsolve $INIT, $SET and annotated $CMT read", {
  m <- read_cpp(c(
    "$PARAM TVCL = 4, TVV = 50",
    "$CMT @annotated",
    "CENT : central amount",
    "$INIT CENT = 10",
    "$SET end = 24",
    cpp_onecmt()[-(1:2)]
  ))

  expect_equal(unclass(m)$dosing$CENT, list(initial = 10))
  expect_true("simulation settings ($SET)" %in% unclass(m)$provenance$losses)
})

test_that("$NMEXT finds a run by run and project", {
  project <- withr::local_tempdir()
  dir.create(file.path(project, "101"))
  file.copy(
    test_path("fixtures", "nonmem", "run1.ext"),
    file.path(project, "101", "101.ext")
  )
  m <- read_cpp(c(
    sprintf(
      "$NMEXT run = 101, project = \"%s\"",
      normalizePath(project, winslash = "/")
    ),
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
  expect_equal(pmx_parameters(m)$estimate[[2]], 4.25)
})

test_that("$PARAM bounds must be in order", {
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

test_that("malformed model files are parse errors, never bare R errors", {
  expect_error(read_cpp(character()), class = "zufapmx_parse_error")
  pkmodel <- function(ncmt) {
    c(
      "$PARAM CL = 1, V = 2",
      sprintf("$PKMODEL ncmt = %d", ncmt),
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED + EPS(1);"
    )
  }
  expect_refused(read_cpp(pkmodel(4)), "ncmt = 4", 2)
  expect_refused(read_cpp(pkmodel(0)), "ncmt = 0", 2)
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = exp(CL, V);")),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_cpp(c("$PARAM TVCL = 1", cpp_onecmt())),
    class = "zufapmx_parse_error"
  )
})

test_that("an undeclared ETA index is refused", {
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = ETA(4);")),
    class = "zufapmx_parse_error"
  )
})
