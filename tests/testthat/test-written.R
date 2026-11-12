test_that("only names assigned once are substituted as aliases", {
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

test_that("covariates that differ only in case are not folded together", {
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

test_that("same_content() compares attributes as a set, like identical()", {
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
