test_that("writers refuse what their format cannot hold, and say who can", {
  writers <- list(
    rxode2 = pmx_write_rxode2,
    nonmem = pmx_write_nonmem,
    mrgsolve = pmx_write_mrgsolve
  )
  # Each refusal names where the construct is, and every format it lists
  # really writes the model.
  expect_refusal <- function(model, formats, location) {
    for (format in formats) {
      err <- expect_error(
        writers[[format]](model),
        class = "zufapmx_write_error"
      )
      expect_equal(err$location, location)
      for (other in err$formats) {
        expect_s3_class(writers[[other]](model), "zufapmx_written")
      }
    }
  }
  parts <- unclass(corpus_rxode2_model("twocmt-oral"))
  edit <- function(f) {
    x <- parts
    do.call(new_model, f(x))
  }

  log <- edit(function(x) {
    x$observations$cp$transform <- "log"
    x
  })
  expect_refusal(log, names(writers), "observations$cp")

  peripheral <- edit(function(x) {
    x$observations$cp$prediction <- quote(peripheral1 / vp)
    x
  })
  expect_refusal(peripheral, names(writers), "observations$cp")

  ode <- unclass(corpus_rxode2_model("onecmt-oral-mm"))
  ode$observations$cp$prediction <- quote(central / v + 1)
  expect_refusal(do.call(new_model, ode), "nonmem", "observations$cp")

  reused <- edit(function(x) {
    x$individual[[1]] <- quote(ka <- tka * prop.sd)
    x
  })
  expect_refusal(reused, c("nonmem", "mrgsolve"), "observations$cp$residual")

  iov <- edit(function(x) {
    x$random_effects$iov <- list(present = TRUE, occasion = "OCC")
    x
  })
  expect_refusal(iov, names(writers), "random_effects$iov")

  over_sd <- edit(function(x) {
    x$observations$cp$residual <- list(
      type = "expression",
      parameters = "prop.sd",
      expression = quote(F + prop.sd)
    )
    x
  })
  expect_refusal(over_sd, names(writers), "observations$cp$residual")

  over_eps <- read_ctl(ctl_onecmt(error = "Y = F + SQRT(F) * EPS(1)"))
  expect_refusal(over_eps, c("rxode2", "mrgsolve"), "observations$Y$residual")

  power <- pmx_read_rxode2(function() {
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
  expect_refusal(power, c("nonmem", "mrgsolve"), "observations$cp$residual")
})
