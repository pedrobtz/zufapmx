test_that("every corpus model round-trips exactly", {
  for (id in zupmxio_corpus()$id) {
    m <- corpus_rxode2_model(id)
    w <- pmx_write_rxode2(m)

    expect_equal(pmx_fidelity(w), "exact", label = id)
    expect_equal(pmx_losses(w), character(), label = id)
  }
})

test_that("written corpus models simulate to their expected predictions", {
  skip_on_cran()
  skip_if_no_engine("rxode2")
  corpus <- zupmxio_corpus()

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
  expect_match(pmx_losses(w), "^initial values")
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
  skip_if_no_engine("rxode2")
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

test_that("sigma variances become standard-deviation parameters", {
  m <- model_twocmt_oral()
  w <- pmx_write_rxode2(m)

  expect_equal(pmx_fidelity(w), "equivalent")
  expect_true("eps.prop <- c(0, 0.2)" %in% trimws(w$text))
  expect_true("cp ~ prop(eps.prop)" %in% trimws(w$text))
})

test_that("correlated sigma, expression residuals and IOV are refused", {
  m <- model_twocmt_oral()
  re <- unclass(m)$random_effects
  re$sigma <- list(model_block(c("e1", "e2"), c(0.04, 0.01)))
  obs <- unclass(m)$observations
  obs$cp$residual$parameters <- "e1"
  expect_error(
    pmx_write_rxode2(model_with(m, random_effects = re, observations = obs)),
    class = "zupmxio_write_error"
  )

  obs <- unclass(m)$observations
  obs$cp$residual <- list(
    type = "expression",
    parameters = "eps.prop",
    expression = quote(F + eps.prop)
  )
  expect_error(
    pmx_write_rxode2(model_with(m, observations = obs)),
    class = "zupmxio_write_error"
  )

  re <- unclass(m)$random_effects
  re$iov <- list(present = TRUE, occasion = "OCC")
  expect_error(
    pmx_write_rxode2(model_with(m, random_effects = re)),
    class = "zupmxio_write_error"
  )
})

test_that("fidelity accessors reject anything but a written model", {
  expect_error(pmx_fidelity(list()), class = "zupmxio_model_error")
  expect_error(pmx_losses("x"), class = "zupmxio_model_error")
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

test_that("fidelity is equivalent only through known reparameterisations (#32)", {
  m <- corpus_rxode2_model("twocmt-oral")
  parts <- unclass(m)

  renamed <- parts
  renamed$individual <- c(renamed$individual, list(quote(V2 <- v)))
  renamed$structure$parameters[["v"]] <- "V2"
  renamed$observations$cp$prediction <- quote(central / V2)
  back <- do.call(new_model, renamed)
  expect_equal(written_fidelity(character(), m, back), "equivalent")

  changed <- parts
  changed$individual[[2]] <- quote(cl <- tcl * exp(eta.cl) * 2)
  back <- do.call(new_model, changed)
  expect_error(
    written_fidelity(character(), m, back),
    class = "zupmxio_write_error"
  )
})

test_that("variables linCmt() would read as parameters are renamed (#55)", {
  m <- read_ctl(c(
    "$PROBLEM x",
    "$SUBROUTINES ADVAN3 TRANS4",
    "$PK",
    "CL = THETA(1)",
    "V1 = THETA(2)",
    "Q = THETA(3)",
    "V2 = THETA(4)",
    "VPX = 2",
    "KE = CL / V1",
    "VMAX = 3",
    "S1 = V1",
    "$ERROR",
    "Y = F + EPS(1)",
    "$THETA 4 20 2 40",
    "$SIGMA 0.1"
  ))
  text <- pmx_write_rxode2(m)$text

  expect_true(all(
    c("x_VPX <- 2", "x_KE <- CL/V1", "VMAX <- 3") %in% trimws(text)
  ))
  skip_on_cran()
  skip_if_no_engine("rxode2")
  expect_s3_class(
    suppressMessages(rxode2::rxode2(eval(parse(text = text)))),
    "rxUi"
  )
})

test_that("a sigma variance is written as its standard deviation (#90)", {
  parts <- unclass(model_twocmt_oral())
  parts$random_effects$sigma[[1]]$estimate <- matrix(
    0.0625,
    dimnames = list("eps.prop", "eps.prop")
  )
  text <- pmx_write_rxode2(do.call(new_model, parts))$text
  expect_true("    eps.prop <- c(0, 0.25)" %in% text)
})

test_that("a power residual model round-trips through pow() (#105)", {
  m <- pmx_read_rxode2(function() {
    ini({
      tcl <- 4
      tv <- 50
      sd <- 0.1
      pw <- 0.5
    })
    model({
      cl <- tcl
      v <- tv
      cp <- linCmt()
      cp ~ pow(sd, pw)
    })
  })
  w <- pmx_write_rxode2(m)
  expect_equal(pmx_fidelity(w), "exact")
  expect_true("    cp ~ pow(sd, pw)" %in% w$text)
})

test_that("a combined residual with EPS(1) proportional writes (#129)", {
  m <- read_ctl(ctl_onecmt(
    error = c("IPRED = F", "Y = IPRED * (1 + EPS(1)) + EPS(2)"),
    extra = "$SIGMA 0.01"
  ))

  w <- pmx_write_rxode2(m)

  expect_faithful(w)
  expect_true("    Y ~ add(EPS2) + prop(EPS1)" %in% w$text)
  back <- pmx_write_nonmem(text_model(w$text, "rxode2"))
  expect_faithful(back)
  expect_true("Y = F * (1 + EPS(1)) + EPS(2)" %in% back$text)
})

test_that("ADVAN12 is written under names rxode2's linCmt() reads (#176)", {
  m <- read_ctl(c(
    "$PROBLEM advan12",
    "$SUBROUTINES ADVAN12 TRANS4",
    "$PK",
    "CL = THETA(1) * EXP(ETA(1))",
    "V2 = THETA(2)",
    "Q3 = THETA(3)",
    "V3 = THETA(4)",
    "Q4 = THETA(5)",
    "V4 = THETA(6)",
    "KA = THETA(7)",
    "S2 = V2",
    "$ERROR",
    "Y = F * (1 + EPS(1))",
    "$THETA (0, 5) (0, 1.3) (0, 0.8) (0, 23) (0, 0.9) (0, 2.2) (0, 0.2)",
    "$OMEGA 0.09",
    "$SIGMA 0.04"
  ))

  w <- pmx_write_rxode2(m)

  expect_faithful(w)
  expect_false(any(grepl("Q3|Q4", w$text)))

  skip_on_cran()
  skip_if_no_engine("rxode2")
  skip_if_no_engine("mrgsolve")
  expect_true(pmx_compare(m, w, pmx_design(n = 1))$pass)
})

test_that("omega estimates are written, not initial values (#182)", {
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run2.ctl"))
  est <- pmx_omega(m, "estimate")
  expect_false(isTRUE(all.equal(est, pmx_omega(m, "init"))))

  w <- pmx_write_rxode2(m)

  # The estimates are written, so the initial values are what is lost.
  expect_equal(pmx_fidelity(w), "lossy")
  back <- pmx_read_rxode2(eval(parse(text = w$text)[[1]]))
  expect_equal(unname(pmx_omega(back, "init")), unname(est))
})

test_that("a chained power is written so rxode2 parses it (#187)", {
  m <- read_ctl(ctl_onecmt(pk = "K2 = 2**3**2"))

  w <- pmx_write_rxode2(m)

  expect_faithful(w)
  expect_true(any(grepl("2^(3^2)", w$text, fixed = TRUE)))
  skip_if_no_engine("rxode2")
  expect_no_error(suppressMessages(rxode2::rxode2(eval(parse(text = w$text)[[
    1
  ]]))))
})
