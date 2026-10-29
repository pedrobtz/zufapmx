test_that("every corpus control stream reads", {
  for (path in zufapmx_corpus()$path) {
    expect_s3_class(
      pmx_read_nonmem(file.path(path, "model.ctl")),
      "zufapmx_model"
    )
  }
})

test_that("closed-form and ODE control streams read into their structures", {
  corpus <- zufapmx_corpus()
  closed <- pmx_read_nonmem(file.path(
    corpus$path[corpus$id == "twocmt-oral"],
    "model.ctl"
  ))
  ode <- pmx_read_nonmem(file.path(
    corpus$path[corpus$id == "onecmt-oral-mm"],
    "model.ctl"
  ))

  expect_equal(
    unclass(closed)$structure$parameters,
    c(cl = "CL", v = "V2", q = "Q", vp = "V3", ka = "KA")
  )
  expect_equal(
    pmx_parameters(closed)$role,
    c("ka", "clearance", "volume", "other", "volume")
  )
  expect_equal(unclass(closed)$observations$Y$prediction, quote(central / V2))
  expect_equal(names(pmx_ode(ode)), c("DEPOT", "CENTRAL"))
  expect_equal(
    unclass(ode)$structure$statements,
    list(quote(C2 <- CENTRAL / V))
  )
  expect_snapshot(print(closed))
  expect_snapshot(print(ode))
  expect_snapshot(unclass(ode)$provenance$losses)
})

test_that("corpus control streams predict their expected.csv", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    model <- pmx_read_nonmem(file.path(corpus$path[[i]], "model.ctl"))
    got <- corpus_solve_rxode2(
      as_rxode2_function(model),
      corpus$regimen[[i]],
      "Y"
    )
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("residual models are recognised", {
  residual <- function(error) {
    unclass(read_ctl(ctl_onecmt(
      error = error,
      extra = "$SIGMA 0.01"
    )))$observations$Y$residual
  }

  expect_equal(residual("Y = F + EPS(1)")$type, "additive")
  expect_equal(residual("Y = F * EXP(EPS(1))")$type, "exponential")
  expect_equal(residual("Y = F + F * EPS(1)")$type, "proportional")
  expect_equal(
    residual(c("IPRED = F", "Y = IPRED * (1 + EPS(1)) + EPS(2)")),
    list(type = "combined", parameters = c("EPS2", "EPS1"))
  )
  w <- residual(c(
    "IPRED = F",
    "W = SQRT(THETA(1)**2 + (THETA(2)*IPRED)**2)",
    "Y = IPRED + W * EPS(1)"
  ))
  expect_equal(w$type, "expression")
  expect_equal(w$parameters, "EPS1")
})

test_that("$ERROR outputs Y does not use are losses", {
  m <- read_ctl(ctl_onecmt(
    error = c("IPRED = F", "IRES = DV - IPRED", "Y = IPRED + EPS(1)")
  ))

  expect_true(
    "$ERROR outputs not used by Y: IRES" %in% unclass(m)$provenance$losses
  )
})

test_that("$THETA, $OMEGA and $SIGMA forms read", {
  m <- read_ctl(c(
    ctl_onecmt()[1:7],
    "$ERROR",
    "Y = F + EPS(1)",
    "$THETA (0, 1, 5) (-INF, 4) 50 FIX",
    "$OMEGA BLOCK(2) 0.1 0.01 0.2",
    "$SIGMA 0.04 FIX"
  ))
  p <- pmx_parameters(m)
  re <- unclass(m)$random_effects

  expect_equal(p$lower, c(0, -Inf, -Inf))
  expect_equal(p$upper, c(5, Inf, Inf))
  expect_equal(p$fixed, c(FALSE, FALSE, TRUE))
  expect_equal(pmx_omega(m)["ETA1", "ETA2"], 0.01)
  expect_true(re$sigma[[1]]$fixed)
})

test_that("dosing attributes and IF blocks in $PK read", {
  m <- read_ctl(ctl_onecmt(
    pk = c(
      "ALAG1 = THETA(1) / 10",
      "F1 = 0.8",
      "IF (WT.GT.70) THEN",
      "  WTF = 1.2",
      "ELSE",
      "  WTF = 1",
      "ENDIF"
    )
  ))

  expect_equal(
    unclass(m)$dosing$depot,
    list(lag = quote(THETA1 / 10), bioavailability = 0.8)
  )
  expect_equal(unclass(m)$individual[[4]], quote(WTF <- 0))
  expect_true(is_call(unclass(m)$individual[[5]], "if"))
  expect_contains(
    unclass(m)$provenance$losses,
    "values kept from earlier records by variables assigned only inside IF (D14)"
  )
})

test_that("unsupported control streams are refused with a position", {
  base <- ctl_onecmt()

  expect_refused(
    read_ctl(c(base[1:4], "\"  CALL USER", base[5:length(base)])),
    "\"",
    5
  )
  expect_refused(read_ctl(c(base, "$INFN", "CALL SUPP")), "$INFN", 14)
  expect_refused(
    read_ctl(sub("ADVAN2 TRANS2", "ADVAN2 TRANS2 OTHER=user.f90", base)),
    "OTHER=",
    2
  )
  expect_refused(read_ctl(sub("ADVAN2 TRANS2", "ADVAN5", base)), "ADVAN5", 2)
  expect_refused(
    read_ctl(sub("ADVAN2 TRANS2", "ADVAN2 TRANS1", base)),
    "ADVAN2 TRANS1",
    2
  )
  expect_refused(
    read_ctl(sub("^\\$OMEGA .*", "$OMEGA BLOCK(1) SAME", base)),
    "SAME",
    12
  )
  expect_refused(
    read_ctl(sub("^\\$OMEGA .*", "$OMEGA SD 0.3 0.3", base)),
    "SD",
    12
  )
  # Refusals of statements point at their record: $ERROR, $PK.
  expect_refused(read_ctl(ctl_onecmt(error = "IPRED = F")), "$ERROR", 8)
  expect_refused(read_ctl(ctl_onecmt(pk = "X = SIN(T)")), "SIN", 8)
  expect_refused(read_ctl(ctl_onecmt(pk = "X = A(5)")), "A(5)", 8)
  expect_refused(read_ctl(base[base != "V = THETA(3) * EXP(ETA(2))"]), "$PK", 3)
  expect_refused(read_ctl(c(base, "$FOO")), "$FOO", 14)
})

test_that("an ODE model needs a derivative for every compartment", {
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_error(
    read_ctl(lines[!grepl("DADT\\(2\\)", lines)]),
    class = "zufapmx_parse_error"
  )
})

test_that("estimates come from run.xml, .ext or .lst", {
  ctl <- test_path("fixtures", "nonmem", "run1.ctl")

  ext <- pmx_read_nonmem(ctl, "ext")
  expect_equal(pmx_parameters(ext)$estimate, c(1.12, 4.25, 21.5, 2.15, 41.5))
  expect_equal(diag(pmx_omega(ext)), c(ETA1 = 0.084, ETA2 = 0.096))
  expect_equal(diag(pmx_omega(ext, "init")), c(ETA1 = 0.09, ETA2 = 0.09))
  expect_equal(basename(unclass(ext)$provenance$estimates), "run1.ext")

  lst <- pmx_read_nonmem(ctl, "lst")
  expect_equal(pmx_parameters(lst)$estimate, c(1.14, 4.27, 21.7, 2.17, 41.7))
  expect_equal(pmx_omega(lst)["ETA1", "ETA2"], 0)
  expect_equal(unclass(lst)$random_effects$sigma[[1]]$estimate[[1]], 0.036)

  expect_true(all(is.na(pmx_parameters(pmx_read_nonmem(ctl, "none"))$estimate)))
})

test_that("auto prefers run.xml through zuxml", {
  skip_if_no_engine("zuxml")
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run1.ctl"))

  expect_equal(pmx_parameters(m)$estimate, c(1.13, 4.26, 21.6, 2.16, 41.6))
  expect_equal(basename(unclass(m)$provenance$estimates), "run1.xml")
})

test_that("auto falls back to .ext without zuxml", {
  local_mocked_bindings(has_zuxml = function() FALSE)
  ctl <- test_path("fixtures", "nonmem", "run1.ctl")

  m <- pmx_read_nonmem(ctl)
  expect_equal(basename(unclass(m)$provenance$estimates), "run1.ext")
  expect_error(pmx_read_nonmem(ctl, "xml"), class = "zufapmx_engine_error")
})

test_that("auto reads no estimates when there are no output files", {
  m <- pmx_read_nonmem(corpus_file("onecmt-oral", "model.ctl"))

  expect_true(all(is.na(pmx_parameters(m)$estimate)))
  expect_error(
    pmx_read_nonmem(corpus_file("onecmt-oral", "model.ctl"), "ext"),
    class = "zufapmx_model_error"
  )
})

test_that("dosing and scale assignments inside IF, or read as variables, are refused", {
  refused <- function(pk, seen) {
    # Statements point at their record, $PK on line 3.
    expect_refused(read_ctl(ctl_onecmt(pk = pk)), seen, 3)
  }

  refused("IF (KA.GT.0) F1 = 0.5", "F1")
  refused(c("IF (V.GT.0) THEN", "S2 = V", "ENDIF"), "S2")
  refused(c("F1 = THETA(1)", "X = F1 * 2"), "F1")
  refused("A_0(5) = 1", "A_0(5)")
})

test_that("$ERROR uses the definitions in force where Y is assigned", {
  m <- read_ctl(ctl_onecmt(
    error = c("IPRED = F", "Y = IPRED * (1 + EPS(1))", "IPRED = 0")
  ))

  expect_equal(
    unclass(m)$observations$Y$residual,
    list(type = "proportional", parameters = "EPS1")
  )
})

test_that("$SIZES and $ERROR (ONLY OBSERVATIONS) are read as losses", {
  lines <- ctl_onecmt()
  lines <- c(
    "$SIZES PD=50",
    sub("^\\$ERROR$", "$ERROR (ONLY OBSERVATIONS)", lines)
  )
  m <- read_ctl(lines)
  losses <- unclass(m)$provenance$losses

  expect_true("array sizes ($SIZES)" %in% losses)
  expect_true("record options ($ERROR (ONLY OBSERVATIONS))" %in% losses)
})

# run2's three output files agree, with distinct off-diagonal elements.
expect_run2_estimates <- function(source) {
  omega <- matrix(
    c(0.11, 0.012, 0.023, 0.012, 0.21, 0.034, 0.023, 0.034, 0.31),
    3,
    dimnames = list(paste0("ETA", 1:3), paste0("ETA", 1:3))
  )
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run2.ctl"), source)
  re <- unclass(m)$random_effects

  expect_equal(pmx_parameters(m)$estimate, c(1.21, 4.31, 51.1), label = source)
  expect_equal(pmx_omega(m), omega, label = source)
  expect_equal(
    diag(pmx_omega(m, "init")),
    c(ETA1 = 0.1, ETA2 = 0.2, ETA3 = 0.3),
    label = source
  )
  expect_equal(
    c(re$sigma[[1]]$estimate[[1]], re$sigma[[2]]$estimate[[1]]),
    c(0.031, 0.41),
    label = source
  )
}

test_that("estimate matrices keep their orientation from .ext and .lst", {
  expect_run2_estimates("ext")
  expect_run2_estimates("lst")
})

test_that("estimate matrices keep their orientation from run.xml", {
  skip_if_no_engine("zuxml")
  expect_run2_estimates("xml")
})

test_that("Y is assigned once, outside IF", {
  expect_error(
    read_ctl(ctl_onecmt(
      error = c("IPRED = F", "Y = IPRED * (1 + EPS(1))", "IF (KA.GT.100) Y = 0")
    )),
    class = "zufapmx_parse_error"
  )
  expect_error(
    read_ctl(ctl_onecmt(
      error = c("Y = F * (1 + EPS(1))", "Y = Y + 1")
    )),
    class = "zufapmx_parse_error"
  )
})

test_that("a Y that does not use F names its own prediction", {
  lines <- ctl_onecmt(error = c("IPRED = A(2) / V", "Y = IPRED * (1 + EPS(1))"))
  m <- read_ctl(lines[lines != "S2 = V"])
  y <- unclass(m)$observations$Y
  expect_equal(y$prediction, quote(central / V))
  expect_equal(y$residual$type, "proportional")

  expect_error(
    read_ctl(ctl_onecmt(error = c("Y = A(2) / V + EXP(EPS(1))"))),
    class = "zufapmx_parse_error"
  )
})

test_that("scale and dosing names are assigned once", {
  for (pk in list(
    "S2 = S2 / 1000",
    c("ALAG1 = 1", "ALAG1 = 2"),
    c("A_0(2) = 1", "A_0(2) = 2")
  )) {
    expect_error(
      read_ctl(ctl_onecmt(pk = pk)),
      class = "zufapmx_parse_error",
      info = pk[[length(pk)]]
    )
  }
})

test_that("TSCALE, XSCALE and INITIALOFF are refused", {
  for (pk in c("TSCALE = 60", "XSCALE = 1000")) {
    expect_refused(read_ctl(ctl_onecmt(pk = pk)), sub(" .*", "", pk), info = pk)
  }
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  lines <- sub(
    "(DEPOT, DEFDOSE)",
    "(DEPOT, DEFDOSE, INITIALOFF)",
    lines,
    fixed = TRUE
  )
  err <- expect_error(read_ctl(lines), class = "zufapmx_parse_error")
  expect_equal(err$seen, "INITIALOFF")
})

test_that("time is T in $DES and TIME elsewhere", {
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  des <- sub("C2 = A(2) / V", "C2 = A(2) / V * TIME", lines, fixed = TRUE)
  err <- expect_error(read_ctl(des), class = "zufapmx_parse_error")
  expect_equal(err$seen, "TIME")

  m <- read_ctl(ctl_onecmt(pk = c("T = 2", "CL = CL * T")))
  expect_true("T" %in% stmt_targets(unclass(m)$individual))
  expect_false("t" %in% all.vars(unclass(m)$individual))
})

test_that("a derivative inside IF, or a scale of no compartment, is refused", {
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_refused(
    read_ctl(sub(
      "DADT(1) = -KA * A(1)",
      "IF (T.GT.1) DADT(1) = -KA * A(1)",
      lines,
      fixed = TRUE
    )),
    "DADT(1)",
    14
  )
  expect_refused(
    read_ctl(sub("S2 = V", "S5 = V", lines, fixed = TRUE)),
    "S5",
    8
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

test_that("NONMEM records are refused with their position", {
  base <- ctl_onecmt()

  expect_refused(read_ctl(c("CL = 1", base)), "CL = 1", 1)
  expect_refused(read_ctl(c(base, "$PK", "X = 1")), "$PK", 14)
  expect_refused(
    read_ctl(base[
      !startsWith(base, "$ERROR") &
        base != "IPRED = F" &
        !startsWith(base, "Y =")
    ]),
    "end of file",
    10
  )
  expect_refused(read_ctl(c(base, "$DES", "DADT(1) = 1")), "$DES", 14)
  expect_refused(read_ctl(ctl_onecmt(pk = c("S1 = V", "S2 = V"))), "S2", 3)
  expect_refused(read_ctl(sub("ADVAN2 TRANS2", "TRANS2", base)), "TRANS2", 2)
  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_refused(
    read_ctl(sub("COMP=", "CMP=", lines)),
    "CMP=(DEPOT, DEFDOSE)   CMP=(CENTRAL, DEFOBS)",
    5
  )
  expect_refused(read_ctl(ctl_onecmt(pk = "ALAG3 = 1")), "ALAG3", 3)
  expect_refused(
    read_ctl(ctl_onecmt(error = c("IF (DV.GT.0) W = 1", "Y = F + W * EPS(1)"))),
    "W",
    8
  )
  expect_refused(
    read_ctl(sub("^\\$OMEGA .*", "$OMEGA BLOCK(2) 0.1 0.01", base)),
    "2 values",
    12
  )
})

test_that("malformed $THETA items are refused at their line", {
  base <- ctl_onecmt()
  thetas <- list(
    list("$THETA (0, 1 FIX", "end of record"),
    list("$THETA (0, 1, 2, 3) (0, 4) (0, 50)", "("),
    list("$THETA (0 1) (0, 4) (0, 50)", "1")
  )
  for (theta in thetas) {
    expect_refused(
      read_ctl(sub("^\\$THETA .*", theta[[1]], base)),
      theta[[2]],
      11,
      info = theta[[1]]
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

test_that("$THETA values must be in bound order", {
  expect_refused(
    read_ctl(sub(
      "^\\$THETA .*",
      "$THETA (10, 5) (0, 4) (0, 50)",
      ctl_onecmt()
    )),
    "10, 5"
  )
})

test_that("malformed control streams are parse errors, never bare R errors", {
  expect_error(read_ctl(character()), class = "zufapmx_parse_error")
  expect_refused(read_ctl(ctl_onecmt(pk = "S0 = V")), "S0", 3)
  expect_refused(read_ctl(ctl_onecmt(pk = "DADT(1) = 1")), "DADT(1)", 3)

  lines <- readLines(corpus_file("onecmt-oral-mm", "model.ctl"))
  expect_error(
    read_ctl(c(lines, "$DES", "DADT(3) = 1")),
    class = "zufapmx_parse_error"
  )
  expect_refused(read_ctl(sub("DADT\\(2\\)", "DADT(3)", lines)), "DADT(3)", 14)
})

test_that("undeclared or fractional THETA and ETA indices are refused", {
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
})
