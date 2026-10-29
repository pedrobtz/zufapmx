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
  corpus <- zufapmx_corpus()
  for (id in corpus$id) {
    expect_s3_class(pmx_read_rxode2(corpus_rxode2(id)), "zufapmx_model")
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
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(x <- ifelse(cl > 1, 1, 2)))),
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(cp ~ t(sd)))),
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(y ~ add(sd)))),
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(alag(gut) <- 1))),
    class = "zufapmx_parse_error"
  )
  expect_error(
    pmx_read_rxode2(base(quote(d / dt(central) <- -cl))),
    class = "zufapmx_parse_error"
  )
})

test_that("a function without ini() or model() is refused", {
  expect_error(
    pmx_read_rxode2(function() {
      model({
        cp <- linCmt()
      })
    }),
    class = "zufapmx_parse_error"
  )
  expect_error(pmx_read_rxode2("one.cmt"), class = "zufapmx_model_error")
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
    class = "zufapmx_parse_error"
  )
})

test_that("linCmt() reads NONMEM's numbered names", {
  lincmt <- function(names) {
    stmts <- lapply(names, function(n) call("<-", as.symbol(n), 1))
    f <- function() NULL
    body(f) <- call(
      "{",
      call(
        "ini",
        quote({
          sd <- 0.1
        })
      ),
      call(
        "model",
        as.call(c(
          as.symbol("{"),
          stmts,
          quote(Y <- linCmt()),
          quote(Y ~ add(sd))
        ))
      )
    )
    unclass(pmx_read_rxode2(f))$structure$parameters
  }

  expect_equal(
    lincmt(c("CL", "V2", "Q", "V3", "KA")),
    c(cl = "CL", v = "V2", q = "Q", vp = "V3", ka = "KA")
  )
  expect_equal(
    lincmt(c("CL", "V1", "Q", "V2")),
    c(cl = "CL", v = "V1", q = "Q", vp = "V2")
  )
  expect_equal(
    lincmt(c("CL", "V1", "Q2", "V2", "Q3", "V3")),
    c(cl = "CL", v = "V1", q = "Q2", vp = "V2", q2 = "Q3", vp2 = "V3")
  )
  expect_equal(
    lincmt(c("cl", "v", "q", "vp", "q2", "vp2")),
    c(cl = "cl", v = "v", q = "q", vp = "vp", q2 = "q2", vp2 = "vp2")
  )
})

test_that("statements reassigning a name the ODE code uses move into it", {
  m <- pmx_read_rxode2(function() {
    ini({
      tcl <- 4
      tv <- 50
      sd <- 0.1
    })
    model({
      cl <- tcl
      v <- tv
      k <- cl / v
      if (time > 10) {
        k <- k / 2
      }
      d / dt(central) <- -k * central
      cp <- central / v
      cp ~ add(sd)
    })
  })
  expect_equal(
    unclass(m)$individual,
    list(quote(cl <- tcl), quote(v <- tv))
  )
  expect_equal(length(unclass(m)$structure$statements), 2)

  m <- pmx_read_rxode2(function() {
    ini({
      tcl <- 4
      tv <- 50
      sd <- 0.1
    })
    model({
      cl <- tcl
      v <- tv
      k <- cl / v
      k <- k / (1 + central / 100)
      d / dt(central) <- -k * central
      cp <- central / v
      cp ~ add(sd)
    })
  })
  expect_equal(
    unclass(m)$structure$statements,
    list(quote(k <- cl / v), quote(k <- k / (1 + central / 100)))
  )
  expect_true(any(grepl("$DES", pmx_write_nonmem(m)$text, fixed = TRUE)))
})

test_that("an endpoint another statement reads stays a statement", {
  m <- pmx_read_rxode2(function() {
    ini({
      tcl <- 4
      tv <- 50
      emax <- 10
      ec50 <- 2
      sd <- 0.1
      sd2 <- 1
    })
    model({
      cl <- tcl
      v <- tv
      d / dt(central) <- -cl / v * central
      cp <- central / v
      eff <- emax * cp / (ec50 + cp)
      cp ~ add(sd)
      eff ~ add(sd2)
    })
  })
  obs <- unclass(m)$observations
  expect_equal(obs$cp$prediction, quote(cp))
  expect_equal(obs$eff$prediction, quote(emax * cp / (ec50 + cp)))
  expect_equal(unclass(m)$structure$statements, list(quote(cp <- central / v)))
  expect_equal(pmx_fidelity(pmx_write_rxode2(m)), "exact")
})

test_that("a parameter reassigned in model() is refused", {
  expect_error(
    pmx_read_rxode2(function() {
      ini({
        cl <- 4
        v <- 50
        eta.cl ~ 0.09
        sd <- 0.1
      })
      model({
        cl <- cl * exp(eta.cl)
        cp <- linCmt()
        cp ~ add(sd)
      })
    }),
    class = "zufapmx_parse_error"
  )
})

test_that("a linCmt() endpoint that is reassigned or read is refused", {
  expect_error(
    pmx_read_rxode2(function() {
      ini({
        tcl <- 4
        tv <- 50
        sd <- 0.1
      })
      model({
        cl <- tcl
        v <- tv
        cp <- linCmt()
        cp <- cp * 2
        cp ~ add(sd)
      })
    }),
    class = "zufapmx_parse_error"
  )
})
