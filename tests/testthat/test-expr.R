test_that("constants, names and table calls are valid expressions", {
  expect_silent(expr_check(quote(1.5)))
  expect_silent(expr_check(quote(cl)))
  expect_silent(expr_check(quote(tcl * exp(eta.cl))))
  expect_silent(expr_check(quote(-(a + b) / sqrt(c)^2)))
  expect_silent(expr_check(quote(log10(x) >= 0 & !(y == 1))))
  expect_silent(expr_check(quote(max(a, min(b, 2)))))
})

test_that("a call outside the function table is an error", {
  expect_error(expr_check(quote(sin(x))), class = "zupkpmx_model_error")
  expect_error(expr_check(quote(foo(x, 1))), class = "zupkpmx_model_error")
  expect_error(expr_check(quote(a %% b)), class = "zupkpmx_model_error")
})

test_that("non-numeric constants are errors", {
  expect_error(expr_check("a"), class = "zupkpmx_model_error")
  expect_error(expr_check(quote(TRUE)), class = "zupkpmx_model_error")
  expect_error(expr_check(c(1, 2)), class = "zupkpmx_model_error")
  expect_error(expr_check(NULL), class = "zupkpmx_model_error")
})

test_that("a table function with the wrong number of arguments is an error", {
  expect_error(expr_check(quote(exp(a, b))), class = "zupkpmx_model_error")
  expect_error(expr_check(quote(max(a))), class = "zupkpmx_model_error")
})

test_that("assignments and if/else are valid statements", {
  expect_silent(stmt_check(quote(cl <- tcl * exp(eta.cl))))
  expect_silent(stmt_check(quote(
    if (wt > 70) {
      v <- 2
    } else {
      v <- 1
    }
  )))
  expect_silent(stmt_check(quote(
    if (sex == 1) {
      f <- 0.8
    }
  )))
})

test_that("a statement must assign to a name", {
  expect_error(stmt_check(quote(cl)), class = "zupkpmx_model_error")
  expect_error(stmt_check(quote(f(x) <- 1)), class = "zupkpmx_model_error")
  expect_error(stmt_check(str2lang("cl = 1")), class = "zupkpmx_model_error")
  expect_error(stmt_check(quote(cl <- sin(x))), class = "zupkpmx_model_error")
})

test_that("stmt_targets() lists every name a statement list assigns", {
  stmts <- list(
    quote(cl <- tcl),
    quote(
      if (wt > 70) {
        v <- 2
      } else {
        v <- 1
        k <- cl / v
      }
    )
  )
  expect_equal(stmt_targets(stmts), c("cl", "v", "k"))
})
