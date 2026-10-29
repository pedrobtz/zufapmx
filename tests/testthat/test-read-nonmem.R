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
  skip_if_not_installed("rxode2")
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
  expect_true(any(grepl("D14", unclass(m)$provenance$losses)))
})

test_that("unsupported control streams are refused with a position", {
  refused <- function(lines) {
    expect_error(read_ctl(lines), class = "zufapmx_parse_error")
  }
  base <- ctl_onecmt()

  err <- refused(c(base[1:4], "\"  CALL USER", base[5:length(base)]))
  expect_equal(err$line, 5L)
  refused(c(base, "$INFN", "CALL SUPP"))
  refused(sub("ADVAN2 TRANS2", "ADVAN2 TRANS2 OTHER=user.f90", base))
  refused(sub("ADVAN2 TRANS2", "ADVAN5", base))
  refused(sub("ADVAN2 TRANS2", "ADVAN2 TRANS1", base))
  refused(sub("^\\$OMEGA .*", "$OMEGA BLOCK(1) SAME", base))
  refused(sub("^\\$OMEGA .*", "$OMEGA SD 0.3 0.3", base))
  refused(ctl_onecmt(error = "IPRED = F"))
  refused(ctl_onecmt(pk = "X = SIN(T)"))
  refused(ctl_onecmt(pk = "X = A(5)"))
  refused(base[base != "V = THETA(3) * EXP(ETA(2))"])
  refused(c(base, "$FOO"))
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
  skip_if_not_installed("zuxml")
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
  refused <- function(pk) {
    expect_error(read_ctl(ctl_onecmt(pk = pk)), class = "zufapmx_parse_error")
  }

  refused("IF (KA.GT.0) F1 = 0.5")
  refused(c("IF (V.GT.0) THEN", "S2 = V", "ENDIF"))
  refused(c("F1 = THETA(1)", "X = F1 * 2"))
  refused("A_0(5) = 1")
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

test_that("estimate matrices keep their orientation from every source", {
  omega <- matrix(
    c(0.11, 0.012, 0.023, 0.012, 0.21, 0.034, 0.023, 0.034, 0.31),
    3,
    dimnames = list(paste0("ETA", 1:3), paste0("ETA", 1:3))
  )
  sigma <- matrix(c(0.031, 0, 0, 0.41), 2)
  sources <- c(
    "ext",
    "lst",
    if (requireNamespace("zuxml", quietly = TRUE)) "xml"
  )

  for (source in sources) {
    m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run2.ctl"), source)
    re <- unclass(m)$random_effects

    expect_equal(
      pmx_parameters(m)$estimate,
      c(1.21, 4.31, 51.1),
      label = source
    )
    expect_equal(pmx_omega(m), omega, label = source)
    expect_equal(
      diag(pmx_omega(m, "init")),
      c(ETA1 = 0.1, ETA2 = 0.2, ETA3 = 0.3),
      label = source
    )
    expect_equal(
      c(re$sigma[[1]]$estimate[[1]], re$sigma[[2]]$estimate[[1]]),
      diag(sigma),
      label = source
    )
  }
})
