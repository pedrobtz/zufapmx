function() {
  ini({
    tka <- c(0, 1)
    tcl <- c(0, 4)
    tv <- c(0, 50)
    tlag <- c(0, 0.25)
    tf <- c(0, 0.6, 1)
    eta.cl ~ 0.09
    eta.v ~ 0.09
    prop.sd <- 0.2
  })
  model({
    ka <- tka
    cl <- tcl * exp(eta.cl)
    v <- tv * exp(eta.v)
    alag(depot) <- tlag
    f(depot) <- tf
    cp <- linCmt()
    cp ~ prop(prop.sd)
  })
}
