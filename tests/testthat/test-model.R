test_that("a closed-form model prints its summary", {
  expect_snapshot(print(model_twocmt_oral()))
  expect_snapshot(print(model_twocmt_oral(estimates = TRUE)))
})

test_that("an ODE model prints its summary", {
  expect_snapshot(print(model_onecmt_mm()))
})

test_that("the representation's structure is stable", {
  expect_snapshot(str_full(unclass(model_twocmt_oral())))
  expect_snapshot(str_full(unclass(model_onecmt_mm())))
})

test_that("pmx_parameters() returns the fixed effects", {
  params <- pmx_parameters(model_twocmt_oral())

  expect_s3_class(params, "data.frame")
  expect_equal(params$name, c("tka", "tcl", "tv", "tq", "tvp"))
  expect_equal(params$init, c(1, 4, 20, 2, 40))
})

test_that("pmx_omega() returns the block-diagonal omega matrix", {
  m <- model_twocmt_oral()
  blocks <- unclass(m)$random_effects$omega
  blocks <- c(
    blocks,
    list(list(
      names = "eta.ka",
      matrix = matrix(0.2, dimnames = list("eta.ka", "eta.ka")),
      fixed = TRUE
    ))
  )
  re <- unclass(m)$random_effects
  re$omega <- blocks
  re$attach <- rbind(
    re$attach,
    data.frame(eta = "eta.ka", parameter = "ka", transform = "exp")
  )
  omega <- pmx_omega(model_with(m, random_effects = re))

  expect_equal(
    dimnames(omega),
    list(c("eta.cl", "eta.v", "eta.ka"), c("eta.cl", "eta.v", "eta.ka"))
  )
  expect_equal(diag(omega), c(eta.cl = 0.09, eta.v = 0.09, eta.ka = 0.2))
  expect_equal(omega["eta.cl", "eta.ka"], 0)
})

test_that("pmx_ode() returns the derivatives, or NULL for a closed form", {
  expect_null(pmx_ode(model_twocmt_oral()))
  expect_equal(names(pmx_ode(model_onecmt_mm())), c("depot", "central"))
})

test_that("accessors reject anything but a model", {
  expect_error(pmx_parameters(list()), class = "zufapmx_model_error")
  expect_error(pmx_omega(1), class = "zufapmx_model_error")
  expect_error(pmx_ode("m"), class = "zufapmx_model_error")
})

test_that("an unknown parameter role is an error", {
  m <- model_twocmt_oral()
  params <- pmx_parameters(m)
  params$role[[1]] <- "absorption"

  expect_error(
    model_with(m, parameters = params),
    class = "zufapmx_model_error"
  )
})

test_that("duplicate parameter names are an error", {
  m <- model_twocmt_oral()
  params <- pmx_parameters(m)
  params$name[[2]] <- "tka"

  expect_error(
    model_with(m, parameters = params),
    class = "zufapmx_model_error"
  )
})

test_that("a non-symmetric omega block is an error", {
  m <- model_twocmt_oral()
  re <- unclass(m)$random_effects
  re$omega[[1]]$matrix[1, 2] <- 0.01

  expect_error(
    model_with(m, random_effects = re),
    class = "zufapmx_model_error"
  )
})

test_that("a non-square omega block is an error", {
  m <- model_twocmt_oral()
  re <- unclass(m)$random_effects
  re$omega[[1]]$matrix <- matrix(0.09, 2, 3)

  expect_error(
    model_with(m, random_effects = re),
    class = "zufapmx_model_error"
  )
})

test_that("a random effect attached to an unknown parameter is an error", {
  m <- model_twocmt_oral()
  re <- unclass(m)$random_effects
  re$attach$parameter[[1]] <- "clearance"

  expect_error(
    model_with(m, random_effects = re),
    class = "zufapmx_model_error"
  )
})

test_that("an attached eta missing from omega is an error", {
  m <- model_twocmt_oral()
  re <- unclass(m)$random_effects
  re$attach$eta[[1]] <- "eta.ka"

  expect_error(
    model_with(m, random_effects = re),
    class = "zufapmx_model_error"
  )
})

test_that("a closed form outside one to three compartments is an error", {
  m <- model_twocmt_oral()
  structure <- unclass(m)$structure
  structure$ncmt <- 4L

  expect_error(
    model_with(m, structure = structure),
    class = "zufapmx_model_error"
  )
})

test_that("a closed form naming an unknown canonical parameter is an error", {
  m <- model_twocmt_oral()
  structure <- unclass(m)$structure
  names(structure$parameters)[[1]] <- "k12"

  expect_error(
    model_with(m, structure = structure),
    class = "zufapmx_model_error"
  )
})

test_that("an ODE model needs one derivative per compartment", {
  m <- model_onecmt_mm()
  structure <- unclass(m)$structure
  structure$derivatives$central <- NULL

  expect_error(
    model_with(m, structure = structure),
    class = "zufapmx_model_error"
  )
})

test_that("an invalid expression anywhere in the model is an error", {
  m <- model_onecmt_mm()
  expect_error(
    model_with(m, individual = list(quote(cl <- sin(x)))),
    class = "zufapmx_model_error"
  )

  structure <- unclass(m)$structure
  structure$derivatives$depot <- quote(foo(depot))
  expect_error(
    model_with(m, structure = structure),
    class = "zufapmx_model_error"
  )
})

test_that("dosing on an unknown compartment or attribute is an error", {
  m <- model_onecmt_mm()
  expect_error(
    model_with(m, dosing = list(gut = list(lag = quote(1)))),
    class = "zufapmx_model_error"
  )
  expect_error(
    model_with(m, dosing = list(depot = list(delay = quote(1)))),
    class = "zufapmx_model_error"
  )
})

test_that("an unknown residual model or transform is an error", {
  m <- model_twocmt_oral()
  obs <- unclass(m)$observations
  obs$cp$residual$type <- "student"
  expect_error(model_with(m, observations = obs), class = "zufapmx_model_error")

  obs <- unclass(m)$observations
  obs$cp$transform <- "logit"
  expect_error(model_with(m, observations = obs), class = "zufapmx_model_error")
})

test_that("a missing component is an error", {
  parts <- unclass(model_twocmt_oral())
  parts["dosing"] <- list(NULL)

  expect_error(do.call(new_model, parts), class = "zufapmx_model_error")
})

test_that("parameters, random effects, compartments and time are not assigned", {
  m <- model_onecmt_mm()
  ind <- unclass(m)$individual
  for (target in c("tka", "eta.vm", "eps.prop", "central", "t")) {
    stmts <- c(ind, list(call("<-", as.symbol(target), 1)))
    expect_error(
      model_with(m, individual = stmts),
      class = "zufapmx_model_error"
    )
  }
})

test_that("ODE code may not reassign an individual name", {
  m <- model_onecmt_mm()
  s <- unclass(m)$structure
  s$statements <- c(s$statements, list(quote(vm <- vm / 2)))
  expect_error(model_with(m, structure = s), class = "zufapmx_model_error")
})

test_that("individual statements may not read compartment amounts (D18)", {
  m <- model_onecmt_mm()
  ind <- c(unclass(m)$individual, list(quote(k <- central / 100)))
  expect_error(model_with(m, individual = ind), class = "zufapmx_model_error")
})

test_that("a name assigned inside if needs a value before it (D14)", {
  m <- model_onecmt_mm()
  ind <- c(
    unclass(m)$individual,
    list(quote(
      if (WT > 70) {
        f <- 2
      }
    ))
  )
  expect_error(model_with(m, individual = ind), class = "zufapmx_model_error")

  ind <- c(unclass(m)$individual, list(quote(f <- 1)), ind[length(ind)])
  expect_s3_class(model_with(m, individual = ind), "zufapmx_model")
})

test_that("a choice argument outside its documented strings is a model error", {
  ctl <- corpus_file("onecmt-oral", "model.ctl")
  m <- model_twocmt_oral()
  for (bad in list("EXT", "bogus", NA, c("final", "initial", "x"), 1)) {
    expect_error(pmx_read_nonmem(ctl, bad), class = "zufapmx_model_error")
    expect_error(pmx_write_nonmem(m, bad), class = "zufapmx_model_error")
    expect_error(pmx_omega(m, bad), class = "zufapmx_model_error")
  }
  expect_equal(pmx_omega(m, "init"), pmx_omega(m))
})

test_that("a file argument must name one existing file", {
  for (bad in list(NA_character_, character(), c("a", "b"), tempdir(), 1)) {
    expect_error(pmx_read_nonmem(bad), class = "zufapmx_model_error")
    expect_error(pmx_read_mrgsolve(bad), class = "zufapmx_model_error")
  }
})

test_that("an edited model is validated again before use", {
  m <- model_twocmt_oral()
  edits <- list(
    function(x) {
      x$parameters$fixed[1] <- NA
      x
    },
    function(x) {
      x$parameters$init[1] <- "1"
      x
    },
    function(x) {
      x$parameters$init[1] <- NA
      x
    },
    function(x) structure(list(), class = "zufapmx_model")
  )
  for (edit in edits) {
    bad <- edit(m)
    expect_error(pmx_write_nonmem(bad), class = "zufapmx_model_error")
    expect_error(pmx_write_rxode2(bad), class = "zufapmx_model_error")
    expect_error(pmx_write_mrgsolve(bad), class = "zufapmx_model_error")
    expect_error(pmx_parameters(bad), class = "zufapmx_model_error")
    expect_error(format(bad), class = "zufapmx_model_error")
  }
})

test_that("a model without endpoints is valid but not written to one-endpoint formats", {
  m <- model_with(model_twocmt_oral(), observations = list())
  err <- expect_error(pmx_write_nonmem(m), class = "zufapmx_write_error")
  expect_equal(err$location, "observations")
})
