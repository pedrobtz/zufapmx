test_that("every corpus model round-trips exactly", {
  for (id in zupkpmx_corpus()$id) {
    m <- corpus_rxode2_model(id)
    w <- pmx_write_rxode2(m)
    back <- pmx_read_rxode2(eval(parse(text = w$text)))

    expect_equal(pmx_fidelity(w), "exact", label = id)
    expect_equal(pmx_losses(w), character(), label = id)
    expect_true(same_content(model_content(back), model_content(m)), label = id)
  }
})

test_that("written corpus models simulate to their expected predictions", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  corpus <- zupkpmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    w <- pmx_write_rxode2(corpus_rxode2_model(corpus$id[[i]]))
    fun <- eval(parse(text = w$text))
    cmt <- corpus_design(corpus$regimen[[i]])$cmt[[1]]
    got <- corpus_solve_rxode2(fun, corpus$regimen[[i]], "cp", cmt = cmt)
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("written text is stable", {
  expect_snapshot(print(pmx_write_rxode2(corpus_rxode2_model("twocmt-oral"))))
  expect_snapshot(print(pmx_write_rxode2(corpus_rxode2_model(
    "onecmt-oral-mm"
  ))))
})

test_that("a model with estimates is written with them and loses its initial values", {
  m <- model_twocmt_oral(estimates = TRUE)
  re <- unclass(m)$random_effects
  re$sigma <- list()
  m <- model_with(
    m,
    random_effects = re,
    observations = list(
      cp = list(
        prediction = quote(central / v),
        residual = list(type = "proportional", parameters = "tvp"),
        transform = "none"
      )
    )
  )
  w <- pmx_write_rxode2(m)

  expect_equal(pmx_fidelity(w), "lossy")
  expect_length(pmx_losses(w), 1)
  expect_true(any(grepl("tcl <- c(0, 4.4)", w$text, fixed = TRUE)))
})

test_that("bounds, fixed values, blocks and dosing round-trip", {
  m <- pmx_read_rxode2(function() {
    ini({
      lcl <- fix(1)
      lv <- c(0, 3, 10)
      lka <- c(-Inf, 0.5, 2)
      eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
      eta.ka ~ fix(0.3)
      sd <- 0.123456789012345678
    })
    model({
      ka <- exp(lka + eta.ka)
      cl <- exp(lcl + eta.cl)
      v <- exp(lv + eta.v)
      if (cl > 2) {
        k <- cl / v
      } else {
        k <- 1 / 3
      }
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - k * central
      alag(depot) <- 0.5
      depot(0) <- 0
      cp <- central / v
      cp ~ add(sd) + prop(sd)
    })
  })
  w <- pmx_write_rxode2(m)

  expect_equal(pmx_fidelity(w), "exact")
})

test_that("a fixed omega block written by the writer parses in rxode2", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  m <- pmx_read_rxode2(function() {
    ini({
      lcl <- 1
      lv <- 3
      eta.cl + eta.v ~ fix(0.1, 0.01, 0.2)
      sd <- 0.1
    })
    model({
      cl <- exp(lcl + eta.cl)
      v <- exp(lv + eta.v)
      cp <- linCmt()
      cp ~ add(sd)
    })
  })
  ui <- suppressMessages(rxode2::rxode2(eval(parse(
    text = pmx_write_rxode2(m)$text
  ))))

  expect_true(all(ui$iniDf$fix[!is.na(ui$iniDf$neta1)]))
})

test_that("sigma blocks and inter-occasion variability are refused", {
  expect_error(
    pmx_write_rxode2(model_twocmt_oral()),
    class = "zupkpmx_write_error"
  )

  m <- model_twocmt_oral()
  re <- unclass(m)$random_effects
  re$sigma <- list()
  re$iov <- list(present = TRUE, occasion = "OCC")
  expect_error(
    pmx_write_rxode2(model_with(m, random_effects = re)),
    class = "zupkpmx_write_error"
  )
})

test_that("fidelity accessors reject anything but a written model", {
  expect_error(pmx_fidelity(list()), class = "zupkpmx_model_error")
  expect_error(pmx_losses("x"), class = "zupkpmx_model_error")
})

test_that("fidelity tolerates a parser that is off by one ulp", {
  x <- 0.12345678901234568
  near <- x * (1 + .Machine$double.eps)

  expect_true(same_content(
    list(quote(a <- 0.12345678901234568 * b), x),
    list(call("<-", quote(a), call("*", near, quote(b))), near)
  ))
  expect_false(same_content(list(x), list(x * (1 + 1e-12))))
  expect_false(same_content(list(quote(a <- b)), list(quote(a <- c))))
  expect_false(same_content(data.frame(a = 1), data.frame(b = 1)))
  expect_false(same_content(list(NA_real_), list(1)))
})
