test_that("only names assigned once are substituted as aliases (#95)", {
  m <- model_twocmt_oral()
  alias <- model_with(
    m,
    individual = c(
      unclass(m)$individual,
      list(quote(a <- b), quote(b <- 3), quote(y <- a))
    )
  )
  direct <- model_with(
    m,
    individual = c(
      unclass(m)$individual,
      list(quote(b <- 3), quote(y <- b))
    )
  )
  expect_false(identical(normalise_model(alias), normalise_model(direct)))

  once <- model_with(
    m,
    individual = c(
      unclass(m)$individual,
      list(quote(b <- 3), quote(a <- b), quote(y <- a))
    )
  )
  direct <- model_with(
    m,
    individual = c(unclass(m)$individual, list(quote(b <- 3), quote(y <- b)))
  )
  expect_true(same_content(normalise_model(once), normalise_model(direct)))
})

test_that("covariates that differ only in case are not folded together (#96)", {
  m <- model_twocmt_oral()
  with_covariates <- function(first, second) {
    model_with(
      m,
      individual = c(
        unclass(m)$individual,
        list(call("<-", quote(k), call("/", first, second)))
      )
    )
  }
  swapped <- normalise_model(with_covariates(quote(WT), quote(wt)))
  original <- normalise_model(with_covariates(quote(wt), quote(WT)))
  expect_false(same_content(swapped, original))

  upper <- normalise_model(with_covariates(quote(WT), quote(AGE)))
  lower <- normalise_model(with_covariates(quote(wt), quote(age)))
  expect_true(same_content(upper, lower))
})

test_that("a writer whose text does not read back raises a write error (#38)", {
  m <- model_twocmt_oral()
  p <- pmx_parameters(m)
  p$name[[1]] <- "if"
  ind <- unclass(m)$individual
  ind[[1]] <- quote(ka <- `if`)

  expect_error(
    pmx_write_rxode2(model_with(m, parameters = p, individual = ind)),
    class = "zupmxio_write_error"
  )
})

test_that("same_content() compares attributes as a set, like identical() (#128)", {
  a <- data.frame(x = c(1, 2), y = c("a", "b"))
  b <- structure(
    list(x = c(1, 2), y = c("a", "b")),
    row.names = c(NA, -2L),
    class = "data.frame"
  )
  expect_identical(a, b)
  expect_false(identical(names(attributes(a)), names(attributes(b))))

  expect_true(same_content(a, b))
  expect_true(same_content(list(p = a), list(p = b)))
  b$x[[2]] <- 3
  expect_false(same_content(a, b))
})

test_that("a closed-form parameter aliased to a covariate writes (#130)", {
  m <- pmx_read_rxode2(function() {
    ini({
      tcl <- c(0, 4)
      tv <- c(0, 10)
      tq <- c(0, 2)
      eta.cl ~ 0.09
      prop.sd <- 0.2
    })
    model({
      cl <- tcl * exp(eta.cl)
      v <- tv
      q <- tq
      vp <- vpcov
      cp <- linCmt()
      cp ~ prop(prop.sd)
    })
  })
  expect_equal(unclass(m)$provenance$covariates, "vpcov")

  for (writer in list(pmx_write_nonmem, pmx_write_rxode2, pmx_write_mrgsolve)) {
    expect_faithful(writer(m))
  }
})

test_that("an error in a writer's read-back that is not a refusal propagates (#113)", {
  testthat::local_mocked_bindings(mrg_read = function(file) stop("reader bug"))
  m <- read_ctl(ctl_onecmt())

  err <- tryCatch(pmx_write_mrgsolve(m), error = function(e) e)

  expect_false(inherits(err, "zupmxio_write_error"))
  expect_equal(conditionMessage(err), "reader bug")
})

test_that("an accessor's error names the function the user called (#171)", {
  err <- tryCatch(pmx_fidelity(list()), error = function(e) e)
  expect_s3_class(err, "zupmxio_model_error")
  expect_identical(conditionCall(err)[[1]], quote(pmx_fidelity))
})

test_that("same_content() tolerates 4 ulps and no more (#140)", {
  ulps <- function(x, k) x * (1 + k * .Machine$double.eps)
  for (x in c(0.1, 3, 2^10, 1e-300)) {
    expect_true(same_content(x, ulps(x, 3)), label = paste(x, "3 ulps"))
    expect_true(same_content(x, ulps(x, 4)), label = paste(x, "4 ulps"))
    expect_false(same_content(x, ulps(x, 6)), label = paste(x, "6 ulps"))
  }
  # Every path that reaches the comparison: a list, and a call.
  expect_true(same_content(list(a = 3), list(a = ulps(3, 4))))
  expect_false(same_content(list(a = 3), list(a = ulps(3, 6))))
  expect_true(same_content(
    call("+", quote(x), 3),
    call("+", quote(x), ulps(3, 4))
  ))
  expect_false(same_content(
    call("+", quote(x), 3),
    call("+", quote(x), ulps(3, 6))
  ))
})
