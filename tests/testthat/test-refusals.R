# Every refusal branch of the readers, writers and validators (design §8,
# D6): each is reached once, asserted by class and, where the design
# promises one, by the construct seen and its line.

test_that("NONMEM records are refused with their position", {
  base <- ctl_onecmt()

  err <- expect_error(
    read_ctl(c("CL = 1", base)),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$line, 1L)
  err <- expect_error(
    read_ctl(c(base, "$PK", "X = 1")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "$PK")
  expect_error(
    read_ctl(base[
      !startsWith(base, "$ERROR") &
        base != "IPRED = F" &
        !startsWith(base, "Y =")
    ]),
    class = "zupkpmx_parse_error"
  )
  err <- expect_error(
    read_ctl(c(base, "$DES", "DADT(1) = 1")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "$DES")
  expect_error(
    read_ctl(ctl_onecmt(pk = c("S1 = V", "S2 = V"))),
    class = "zupkpmx_parse_error"
  )
  err <- expect_error(
    read_ctl(sub("ADVAN2 TRANS2", "TRANS2", base)),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "TRANS2")
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_error(
    read_ctl(sub("COMP=", "CMP=", lines)),
    class = "zupkpmx_parse_error"
  )
  err <- expect_error(
    read_ctl(ctl_onecmt(pk = "ALAG3 = 1")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "ALAG3")
  expect_error(
    read_ctl(ctl_onecmt(error = c("IF (DV.GT.0) W = 1", "Y = F + W * EPS(1)"))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_ctl(sub("^\\$OMEGA .*", "$OMEGA BLOCK(2) 0.1 0.01", base)),
    class = "zupkpmx_parse_error"
  )
  for (theta in c(
    "$THETA (0, 1 FIX",
    "$THETA (0, 1, 2, 3) (0, 4) (0, 50)",
    "$THETA (0 1) (0, 4) (0, 50)"
  )) {
    expect_error(
      read_ctl(sub("^\\$THETA .*", theta, base)),
      class = "zupkpmx_parse_error"
    )
  }
})

test_that("a combined F + F*EPS + EPS residual is recognised", {
  m <- read_ctl(ctl_onecmt(
    error = "Y = F + F * EPS(1) + EPS(2)",
    extra = "$SIGMA 0.01"
  ))
  expect_equal(
    unclass(m)$observations$Y$residual,
    list(type = "combined", parameters = c("EPS2", "EPS1"))
  )
})

test_that("abbreviated code is refused with its position", {
  err <- expect_error(
    nm_parse_lines(c("X = 1", "ELSE")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$line, 2L)
  err <- expect_error(
    nm_parse_lines("X = THETA(Y)"),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "THETA")
  err <- expect_error(
    nm_parse_lines("FOO(1) = 2"),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "FOO")
  err <- expect_error(
    read_ctl(ctl_onecmt(pk = "X = A(5)")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(c(err$line, err$column), c(8L, 5L))
})

test_that("mrgsolve blocks are refused with their position", {
  base <- cpp_onecmt()

  err <- expect_error(
    read_cpp(c(
      "$PARAM CL = 1, V = 2",
      "$PKMODEL advan = 4, trans = 4, cmt = \"A B\"",
      "$TABLE",
      "double IPRED = B / V;",
      "double Y = IPRED + EPS(1);"
    )),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$line, 2L)
  expect_error(
    read_cpp(base[base != "$CMT CENT"]),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    read_cpp(c(base, "$OMEGA @block", "0.1 0.2")),
    class = "zupkpmx_parse_error"
  )
  err <- expect_error(
    read_cpp(c(base, "$ODE", "dxdt_GUT = 1;")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "dxdt_GUT")
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

test_that("rxode2 models are refused for the constructs they lack", {
  model <- function(stmts, ini = list(quote(sd <- 0.1))) {
    f <- function() NULL
    body(f) <- call(
      "{",
      call("ini", as.call(c(as.symbol("{"), ini))),
      call("model", as.call(c(as.symbol("{"), stmts)))
    )
    f
  }
  expect_error(
    pmx_read_rxode2(model(list(
      quote(cl <- 1),
      quote(cp <- linCmt()),
      quote(cp ~ add(sd))
    ))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(model(list(quote(cp <- 1), quote(cp ~ add(sd))))),
    class = "zupkpmx_parse_error"
  )

  lnorm <- pmx_read_rxode2(model(list(
    quote(d / dt(a) <- -a),
    quote(cp <- a),
    quote(cp ~ lnorm(sd))
  )))
  expect_equal(unclass(lnorm)$observations$cp$residual$type, "exponential")
  pow <- pmx_read_rxode2(model(
    list(quote(d / dt(a) <- -a), quote(cp <- a), quote(cp ~ pow(sd, e))),
    ini = list(quote(sd <- 0.1), quote(e <- 1))
  ))
  expect_equal(
    unclass(pow)$observations$cp$residual,
    list(type = "power", parameters = c("sd", "e"))
  )
})

test_that("writers refuse what their format cannot hold", {
  m <- corpus_rxode2_model("twocmt-oral")
  parts <- unclass(m)

  log <- parts
  log$observations$cp$transform <- "log"
  for (write in list(pmx_write_rxode2, pmx_write_nonmem, pmx_write_mrgsolve)) {
    expect_error(write(do.call(new_model, log)), class = "zupkpmx_write_error")
  }

  peripheral <- parts
  peripheral$observations$cp$prediction <- quote(peripheral1 / vp)
  for (write in list(pmx_write_rxode2, pmx_write_nonmem, pmx_write_mrgsolve)) {
    expect_error(
      write(do.call(new_model, peripheral)),
      class = "zupkpmx_write_error"
    )
  }

  ode <- unclass(corpus_rxode2_model("onecmt-oral-mm"))
  ode$observations$cp$prediction <- quote(central / v + 1)
  expect_error(
    pmx_write_nonmem(do.call(new_model, ode)),
    class = "zupkpmx_write_error"
  )

  reused <- parts
  reused$individual[[1]] <- quote(ka <- tka * prop.sd)
  expect_error(
    pmx_write_nonmem(do.call(new_model, reused)),
    class = "zupkpmx_write_error"
  )

  iov <- parts
  iov$random_effects$iov <- list(present = TRUE, occasion = "OCC")
  expect_error(
    pmx_write_mrgsolve(do.call(new_model, iov)),
    class = "zupkpmx_write_error"
  )

  expr <- parts
  expr$observations$cp$residual <- list(
    type = "expression",
    parameters = "prop.sd",
    expression = quote(F + prop.sd)
  )
  expect_error(
    pmx_write_rxode2(do.call(new_model, expr)),
    class = "zupkpmx_write_error"
  )
})

test_that("the validator refuses malformed components", {
  m <- model_twocmt_oral()
  parts <- unclass(m)

  expect_error(
    model_with(m, parameters = list(name = "a")),
    class = "zupkpmx_model_error"
  )
  re <- parts$random_effects
  re$attach$transform[[1]] <- "boxcox"
  expect_error(
    model_with(m, random_effects = re),
    class = "zupkpmx_model_error"
  )
  re <- parts$random_effects
  re$omega[[1]]$estimate <- matrix(1, 1, 1)
  expect_error(
    model_with(m, random_effects = re),
    class = "zupkpmx_model_error"
  )
  s <- parts$structure
  s$parameters[["cl"]] <- "clearance"
  expect_error(model_with(m, structure = s), class = "zupkpmx_model_error")
  s <- parts$structure
  s$type <- "pbpk"
  expect_error(model_with(m, structure = s), class = "zupkpmx_model_error")
})

test_that("the corpus reader refuses an empty root, unknown formats and regimens", {
  expect_error(
    corpus_read(withr::local_tempdir()),
    class = "zupkpmx_corpus_error"
  )
  root <- withr::local_tempdir()
  dir.create(file.path(root, "x"))
  writeLines(
    c(
      "Id: x",
      "Title: t",
      "Structure: s",
      "Regimen: oral",
      "Formats: pumas",
      "Source: s"
    ),
    file.path(root, "x", "entry.dcf")
  )
  expect_error(corpus_read(root), class = "zupkpmx_corpus_error")
  expect_error(corpus_design("subcutaneous"), class = "zupkpmx_corpus_error")
})

test_that("readers refuse a missing file", {
  expect_error(pmx_read_nonmem(tempfile()), class = "zupkpmx_model_error")
  expect_error(pmx_read_mrgsolve(tempfile()), class = "zupkpmx_model_error")
  expect_error(
    pmx_read_nonmem(test_path("fixtures", "nonmem", "run1.ctl"), NA),
    class = "zupkpmx_model_error"
  )
})
