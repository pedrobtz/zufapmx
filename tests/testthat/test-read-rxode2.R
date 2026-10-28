test_that("a closed-form corpus model reads into its structure", {
  m <- pmx_read_rxode2(corpus_rxode2("twocmt-oral"))
  s <- unclass(m)$structure

  expect_equal(s$type, "closed")
  expect_equal(s$ncmt, 2L)
  expect_true(s$depot)
  expect_equal(
    s$parameters,
    c(cl = "cl", v = "v", q = "q", vp = "vp", ka = "ka")
  )
  expect_equal(c(s$advan, s$trans), c(4L, 4L))
  expect_equal(
    pmx_parameters(m)$role,
    c("ka", "clearance", "volume", "other", "volume", "other")
  )
  expect_equal(pmx_parameters(m)$lower, c(0, 0, 0, 0, 0, -Inf))
  expect_equal(unclass(m)$observations$cp$prediction, quote(central / v))
  expect_snapshot(print(m))
})

test_that("an ODE corpus model reads into states, statements and derivatives", {
  m <- pmx_read_rxode2(corpus_rxode2("onecmt-oral-mm"))
  s <- unclass(m)$structure

  expect_equal(s$type, "ode")
  expect_equal(s$compartments, c("depot", "central"))
  expect_equal(s$statements, list(quote(c2 <- central / v)))
  expect_equal(names(pmx_ode(m)), c("depot", "central"))
  expect_length(unclass(m)$individual, 4)
  expect_equal(unclass(m)$observations$cp$prediction, quote(central / v))
  expect_snapshot(print(m))
})

test_that("every corpus model reads", {
  corpus <- zupkpmx_corpus()
  for (id in corpus$id) {
    expect_s3_class(pmx_read_rxode2(corpus_rxode2(id)), "zupkpmx_model")
  }
})

test_that("etas attach to the statement that reads them", {
  m <- pmx_read_rxode2(function() {
    ini({
      lcl <- 1
      tv <- 3
      tka <- 0.5
      eta.cl ~ 0.1
      eta.v ~ 0.2
      eta.ka ~ 0.3
      sd <- 0.1
    })
    model({
      cl <- exp(lcl + eta.cl)
      v <- tv * exp(eta.v)
      ka <- tka + eta.ka
      cp <- linCmt()
      cp ~ add(sd)
    })
  })
  attach <- unclass(m)$random_effects$attach

  expect_equal(attach$parameter, c("cl", "v", "ka"))
  expect_equal(attach$transform, c("exp", "exp", "additive"))
})

test_that("bounds, fixed values and omega blocks read", {
  m <- pmx_read_rxode2(function() {
    ini({
      lcl <- fix(1)
      lv <- c(0, 3, 10)
      eta.cl + eta.v ~ c(0.1, 0.01, 0.2)
      eta.ka ~ fix(0.3)
      sd <- 0.1
    })
    model({
      ka <- exp(eta.ka)
      cl <- exp(lcl + eta.cl)
      v <- exp(lv + eta.v)
      cp <- linCmt()
      cp ~ add(sd)
    })
  })
  p <- pmx_parameters(m)
  omega <- pmx_omega(m)

  expect_equal(p$fixed, c(TRUE, FALSE, FALSE))
  expect_equal(
    p[p$name == "lv", c("lower", "init", "upper")],
    data.frame(lower = 0, init = 3, upper = 10),
    ignore_attr = TRUE
  )
  expect_equal(omega["eta.cl", "eta.v"], 0.01)
  expect_equal(omega["eta.v", "eta.cl"], 0.01)
  expect_true(unclass(m)$random_effects$omega[[2]]$fixed)
})

test_that("linCmt() accepts vc for the central volume", {
  m <- pmx_read_rxode2(function() {
    ini({
      lcl <- 1
      lvc <- 3
      sd <- 0.1
    })
    model({
      cl <- exp(lcl)
      vc <- exp(lvc)
      Cc <- linCmt()
      Cc ~ add(sd)
    })
  })

  expect_equal(unclass(m)$structure$parameters, c(cl = "cl", v = "vc"))
  expect_equal(unclass(m)$observations$Cc$prediction, quote(central / vc))
})

test_that("dosing attributes read by compartment", {
  m <- pmx_read_rxode2(function() {
    ini({
      tka <- 1
      tcl <- 1
      tv <- 3
      sd <- 0.1
    })
    model({
      ka <- tka
      cl <- tcl
      v <- tv
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      alag(depot) <- 0.5
      f(depot) <- 0.8
      dur(central) <- 2
      depot(0) <- 0
      cp <- central / v
      cp ~ add(sd)
    })
  })

  expect_equal(
    unclass(m)$dosing,
    list(
      depot = list(lag = 0.5, bioavailability = 0.8, initial = 0),
      central = list(duration = 2)
    )
  )
})

test_that("labels and description are dropped as losses", {
  m <- pmx_read_rxode2(function() {
    description <- "One compartment"
    ini({
      lcl <- 1
      label("Clearance")
      lv <- 3
      sd <- 0.1
    })
    model({
      cl <- exp(lcl)
      v <- exp(lv)
      cp <- linCmt()
      cp ~ add(sd)
    })
  })

  expect_equal(unclass(m)$provenance$losses, c("description", "labels"))
  expect_equal(unclass(m)$provenance$fidelity, "lossy")
})

test_that("constructs outside the supported set are refused", {
  base <- function(stmt) {
    f <- function() {
      ini({
        lcl <- 1
        lv <- 3
        sd <- 0.1
      })
      model({
        cl <- exp(lcl)
        v <- exp(lv)
        cp <- linCmt()
        cp ~ add(sd)
      })
    }
    b <- body(f)
    b[[3]][[2]] <- as.call(c(as.list(b[[3]][[2]]), stmt))
    body(f) <- b
    f
  }

  expect_error(
    pmx_read_rxode2(base(quote(x <- sin(cl)))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(x <- ifelse(cl > 1, 1, 2)))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(cp ~ t(sd)))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(y ~ add(sd)))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(alag(gut) <- 1))),
    class = "zupkpmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(d / dt(central) <- -cl))),
    class = "zupkpmx_parse_error"
  )
})

test_that("a function without ini() or model() is refused", {
  expect_error(
    pmx_read_rxode2(function() {
      model({
        cp <- linCmt()
      })
    }),
    class = "zupkpmx_parse_error"
  )
  expect_error(pmx_read_rxode2("one.cmt"), class = "zupkpmx_model_error")
})

test_that("metadata, constant ini expressions and prop() + add() read", {
  m <- pmx_read_rxode2(function() {
    reference <- "Someone et al. 2020"
    units <- list(time = "h", dosing = "mg")
    ini({
      lcl <- log(4)
      lv <- 3
      add.sd <- 0.1
      prop.sd <- 0.2
    })
    model({
      cl <- exp(lcl)
      v <- exp(lv)
      cp <- linCmt()
      cp ~ prop(prop.sd) + add(add.sd)
    })
  })
  m <- unclass(m)

  expect_equal(m$parameters$init[[1]], log(4))
  expect_equal(
    m$observations$cp$residual,
    list(type = "combined", parameters = c("add.sd", "prop.sd"))
  )
  expect_setequal(
    m$provenance$losses,
    c(
      "metadata `reference`",
      "metadata `units`",
      "initial values written as expressions, evaluated"
    )
  )
})

test_that("a compartment can be an endpoint", {
  m <- pmx_read_rxode2(function() {
    ini({
      kg <- 0.1
      sd <- 0.1
    })
    model({
      d / dt(tumor) <- kg * tumor
      tumor(0) <- 1
      tumor ~ prop(sd)
    })
  })

  expect_equal(unclass(m)$observations$tumor$prediction, quote(tumor))
  expect_equal(pmx_fidelity(pmx_write_rxode2(m)), "exact")
})

test_that("a construct outside the table in a derivative is a parse error", {
  expect_error(
    pmx_read_rxode2(function() {
      ini({
        k <- 0.1
        sd <- 0.1
      })
      model({
        d / dt(a) <- ifelse(a > 1, -k * a, 0)
        cp <- a
        cp ~ add(sd)
      })
    }),
    class = "zupkpmx_parse_error"
  )
})
