test_that("every corpus mrgsolve model reads", {
  for (path in zupmxio_corpus()$path) {
    expect_s3_class(
      pmx_read_mrgsolve(file.path(path, "model.cpp")),
      "zupmxio_model"
    )
  }
})

test_that("closed-form and ODE models read into their structures", {
  corpus <- zupmxio_corpus()
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
  corpus <- zupmxio_corpus()

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

test_that("dosing macros under $PKMODEL use the file's compartment names (#24)", {
  m <- pmx_read_mrgsolve(corpus_file("onecmt-oral-lag", "model.cpp"))

  expect_equal(
    unclass(m)$dosing,
    list(depot = list(lag = quote(TVLAG), bioavailability = quote(TVF)))
  )
  expect_false(any(
    c("ALAG_GUT", "F_GUT") %in% stmt_targets(unclass(m)$individual)
  ))
})

test_that("dosing macros inside if, or read as variables, are refused (#25)", {
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

test_that("the $TABLE prediction is the term in F's place, fully expanded (#26)", {
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
    class = "zupmxio_parse_error"
  )
})

test_that("$PKMODEL parameters can come straight from $PARAM (#47)", {
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

test_that("Y is matched exactly in $TABLE (#50)", {
  expect_error(
    read_cpp(cpp_onecmt(
      table = c(
        "double IPRED = CENT / V;",
        "double YOBS = IPRED * (1 + EPS(1));"
      )
    )),
    class = "zupmxio_parse_error"
  )
})

test_that("$NMEXT resolves paths as mrgsolve does and numbers etas in file order (#52)", {
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
  expect_error(pmx_read_mrgsolve("model.cpp"), class = "zupmxio_parse_error")
})

test_that("$NMXML needs zuxml", {
  local_mocked_bindings(has_zuxml = function() FALSE)
  xml <- normalizePath(
    test_path("fixtures", "nonmem", "run1.xml"),
    winslash = "/"
  )
  expect_error(
    read_cpp(c(sprintf("$NMXML path = \"%s\"", xml), cpp_onecmt())),
    class = "zupmxio_engine_error"
  )
})

test_that("a directory is not a model file (#51)", {
  expect_error(
    pmx_read_mrgsolve(withr::local_tempdir()),
    class = "zupmxio_model_error"
  )
  expect_error(
    pmx_read_nonmem(withr::local_tempdir()),
    class = "zupmxio_model_error"
  )
})

test_that("time is SOLVERTIME in $ODE and TIME elsewhere (#83)", {
  err <- expect_error(
    read_cpp(cpp_onecmt(main = "double K = SOLVERTIME;")),
    class = "zupmxio_parse_error"
  )
  expect_equal(err$seen, "SOLVERTIME")

  lines <- cpp_onecmt()
  lines <- sub("-CL / V * CENT", "-CL / V * CENT * TIME", lines, fixed = TRUE)
  err <- expect_error(read_cpp(lines), class = "zupmxio_parse_error")
  expect_equal(err$seen, "TIME")
})

test_that("$ODE beside $PKMODEL is refused (#120)", {
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
    class = "zupmxio_parse_error"
  )
  expect_equal(err$line, 4)
})

test_that("$PARAM values written as expressions are evaluated, with a loss (#114)", {
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

test_that("$NMXML and $NMEXT refuse options the reader does not honour (#97, #120)", {
  for (option in c("omega = FALSE", "tname = \"x\"", "path = 1")) {
    err <- expect_error(
      read_cpp(c(sprintf("$NMEXT run = 1, %s", option), cpp_onecmt()[-1])),
      class = "zupmxio_parse_error",
      info = option
    )
    expect_equal(err$line, 1, info = option)
  }
  err <- expect_error(
    read_cpp(c("$NMEXT path = \"no-such.ext\"", cpp_onecmt())),
    class = "zupmxio_parse_error"
  )
  expect_equal(err$line, 1)
})

test_that("C++ comparisons are ints, and wide int literals are refused (#101)", {
  expect_error(
    read_cpp(cpp_onecmt(main = "double K = TVCL + (1 > 0) / 2;")),
    class = "zupmxio_parse_error"
  )
  expect_s3_class(
    read_cpp(cpp_onecmt(main = "double K = TVCL + (1 > 0) / 2.0;")),
    "zupmxio_model"
  )
  err <- expect_error(
    read_cpp(cpp_onecmt(main = "double K = 3000000000;")),
    class = "zupmxio_parse_error"
  )
  expect_equal(err$seen, "3000000000")
})

test_that("a $PARAM value cannot run code (#91)", {
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
  expect_error(read_cpp(lines), class = "zupmxio_parse_error")
  expect_false(file.exists(marker))
})

test_that("$PKMODEL refuses an unknown advan or trans (#105)", {
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

test_that("a $PARAM item without = is refused (#105)", {
  expect_refused(
    read_cpp(sub("TVCL = 4", "TVCL 4", cpp_onecmt(), fixed = TRUE)),
    "TVCL 4",
    1
  )
})

test_that("$PKMODEL takes its compartment names from $CMT (#105)", {
  m <- read_cpp(c(
    "$PARAM CL = 1, V = 20, KA = 1",
    "$CMT GUT CENT",
    "$PKMODEL ncmt = 1, depot = TRUE",
    "$SIGMA 0.04",
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

test_that("annotated parameter names must be names (#38)", {
  expect_error(
    read_cpp(c(
      "$PARAM @annotated",
      "q <- 1; message('x'); TVCL : 1 : x",
      cpp_onecmt()[-1]
    )),
    class = "zupmxio_parse_error"
  )
})

test_that("C++ integer division and int declarations are refused (#33)", {
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = pow(CL / 70.0, 3 / 4);")),
    class = "zupmxio_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = (1 + 2) / 4;")),
    class = "zupmxio_parse_error"
  )
  expect_error(
    read_cpp(cpp_onecmt(main = "int N = 2;")),
    class = "zupmxio_parse_error"
  )

  m <- read_cpp(cpp_onecmt(
    main = c("double X = 3.0 / 4;", "double Z = 2 * CL;")
  ))
  expect_equal(unclass(m)$individual[[3]], quote(X <- 3 / 4))
  expect_identical(unclass(m)$individual[[4]][[3]][[2]], 2)
})

test_that("mrgsolve blocks are refused with their position (#45)", {
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
    class = "zupmxio_parse_error"
  )
})

test_that("malformed model files are parse errors, never bare R errors (#34, #35)", {
  expect_error(read_cpp(character()), class = "zupmxio_parse_error")
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
    class = "zupmxio_parse_error"
  )
  expect_error(
    read_cpp(c("$PARAM TVCL = 1", cpp_onecmt())),
    class = "zupmxio_parse_error"
  )
})

test_that("an undeclared ETA index is refused (#30)", {
  expect_error(
    read_cpp(cpp_onecmt(main = "double X = ETA(4);")),
    class = "zupmxio_parse_error"
  )
})

test_that("$INPUT names are covariates, not parameters (#126)", {
  m <- read_cpp(cpp_onecmt(
    main = "double CLI = CL * pow(WT / 70.0, 0.75);",
    extra = "$INPUT WT = NA_real_"
  ))

  expect_equal(unclass(m)$provenance$covariates, "WT")
  expect_false("WT" %in% pmx_parameters(m)$name)
  expect_false(any(grepl("INPUT", unclass(m)$provenance$losses)))
})

test_that("an $INPUT default value is a recorded loss (#126)", {
  m <- read_cpp(cpp_onecmt(
    main = "double CLI = CL * pow(WT / 70.0, 0.75);",
    extra = "$INPUT WT = 70"
  ))

  expect_equal(unclass(m)$provenance$covariates, "WT")
  expect_true(any(grepl("\\$INPUT", unclass(m)$provenance$losses)))
})

test_that("a malformed $INPUT entry is refused (#126)", {
  expect_error(
    read_cpp(cpp_onecmt(extra = "$INPUT WT")),
    class = "zupmxio_parse_error"
  )
})

test_that("C++ tokens keep their order, type and column", {
  tokens <- cpp_tokenize("x+= 1.0f; --y", 2L, "f")

  expect_equal(
    vapply(tokens, function(t) paste(t$type, t$value, t$column), ""),
    c("name x 1", "op += 2", "number 1.0f 5", "op ; 9", "op -- 11", "name y 13")
  )
  err <- expect_error(
    cpp_tokenize("x = 1 @ 2", 5L, "f"),
    class = "zupmxio_parse_error"
  )
  expect_equal(c(err$line, err$column), c(5L, 7L))
})

test_that("a name reassigned after a derivative or dosing macro reads it is refused (#146)", {
  ode <- c(
    "$PARAM CL = 1, V = 10",
    "$CMT CENT",
    "$ODE",
    "double k = CL / V;",
    "dxdt_CENT = -k * CENT;",
    "k = 100;",
    "$SIGMA 0.04",
    "$TABLE",
    "double IPRED = CENT / V;",
    "double Y = IPRED * (1 + EPS(1));"
  )
  expect_refused(read_cpp(ode), seen = "k")
  main <- c(
    "$PARAM CL = 1, V = 10",
    "$CMT CENT",
    "$MAIN",
    "double fb = 0.5;",
    "F_CENT = fb;",
    "fb = 1;",
    "$ODE",
    "dxdt_CENT = -CL / V * CENT;",
    "$SIGMA 0.04",
    "$TABLE",
    "double IPRED = CENT / V;",
    "double Y = IPRED * (1 + EPS(1));"
  )
  expect_refused(read_cpp(main), seen = "fb")
  # Before the derivative or the macro, a reassignment is fine.
  ok <- ode
  ok[5:6] <- ode[6:5]
  expect_s3_class(read_cpp(ok), "zupmxio_model")
})

test_that("$PKMODEL advan and ncmt are whole numbers (#155)", {
  pk <- function(opts) {
    c(
      "$PARAM CL = 4, V = 50, KA = 1",
      sprintf("$PKMODEL %s", opts),
      "$SIGMA 0.04",
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED * (1 + EPS(1));"
    )
  }
  expect_refused(read_cpp(pk("advan = 2.9, trans = 2")), seen = "advan = 2.9")
  expect_refused(read_cpp(pk("ncmt = 1.5, depot = TRUE")), seen = "ncmt = 1.5")
  expect_s3_class(
    read_cpp(pk("advan = 2, trans = 2, cmt = \"DEPOT CENT\"")),
    "zupmxio_model"
  )
})

test_that("mrgsolve's built-in names are refused or mapped, not covariates (#149)", {
  expect_refused(read_cpp(cpp_onecmt(main = "double X = EVID;")), seen = "EVID")
  expect_refused(
    read_cpp(cpp_onecmt(main = "double X = NEWIND;")),
    seen = "NEWIND"
  )
  m <- read_cpp(cpp_onecmt(main = c("double X = 2 * M_PI;", "bool B = true;")))
  expect_length(unclass(m)$provenance$covariates, 0)
  stmts <- unclass(m)$individual
  expect_true(list(call("<-", quote(X), call("*", 2, pi))) %in% stmts)
  expect_true(list(call("<-", quote(B), 1)) %in% stmts)
})

test_that("$INIT declares compartments, in file order (#153)", {
  model <- function(init, odes) {
    c(
      "$PARAM CL = 1, V = 10, KP = 0.1",
      init,
      "$CMT CENT",
      "$ODE",
      odes,
      "dxdt_CENT = -CL / V * CENT;",
      "$SIGMA 0.04",
      "$TABLE",
      "double IPRED = CENT / V;",
      "double Y = IPRED * (1 + EPS(1));"
    )
  }
  m <- read_cpp(model("$INIT PER = 0", "dxdt_PER = -KP * PER;"))
  expect_equal(unclass(m)$structure$compartments, c("PER", "CENT"))
  expect_null(unclass(m)$dosing$PER$initial)

  m <- read_cpp(model("$INIT PER = 5", "dxdt_PER = -KP * PER;"))
  expect_equal(unclass(m)$dosing$PER$initial, 5)
  expect_equal(pmx_fidelity(pmx_write_mrgsolve(m)), "exact")

  # A compartment with no derivative is refused, not dropped.
  expect_error(
    read_cpp(model("$INIT PER = 0", character())),
    class = "zupmxio_parse_error"
  )
})

test_that("unknown block options are refused, and @covariates is read (#154)", {
  m <- read_cpp(cpp_onecmt(
    main = "double CLI = CL * pow(WT / 70.0, 0.75);",
    extra = "$PARAM @covariates WT = 70"
  ))
  expect_equal(unclass(m)$provenance$covariates, "WT")
  expect_false("WT" %in% pmx_parameters(m)$name)
  expect_true(any(grepl("covariate default", unclass(m)$provenance$losses)))

  expect_refused(
    read_cpp(cpp_onecmt(extra = "$PARAM @name extra\nKX = 1")),
    seen = "@name"
  )
  expect_refused(
    read_cpp(sub(
      "$OMEGA 0.09",
      "$OMEGA @labels ECL\n0.09",
      cpp_onecmt(),
      fixed = TRUE
    )),
    seen = "@labels"
  )
  # The options the reader honours still read.
  expect_s3_class(
    read_cpp(sub(
      "$OMEGA 0.09",
      "$OMEGA @block\n0.09",
      cpp_onecmt(),
      fixed = TRUE
    )),
    "zupmxio_model"
  )
})

test_that("a multi-line comment keeps the lines after it (#68)", {
  err <- expect_error(
    read_cpp(c(
      "$PARAM CL = 4, V = 50",
      "/* one",
      "   two",
      "   three */",
      "$CMT CENT",
      "$MAIN",
      "double X = 5 $ 2;"
    )),
    class = "zupmxio_parse_error"
  )
  expect_equal(err$line, 7L)
})

test_that("a conditional Y in $TABLE is refused (#79)", {
  expect_error(
    read_cpp(cpp_onecmt(
      table = c(
        "double IPRED = CENT / V;",
        "double Y = IPRED * (1 + EPS(1));",
        "if (CL > 100) Y = 0;"
      )
    )),
    class = "zupmxio_parse_error"
  )
})
