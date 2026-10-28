function() {
  ini({
    tka <- c(0, 1)
    tvm <- c(0, 20)
    tkm <- c(0, 2)
    tv <- c(0, 50)
    eta.vm ~ 0.09
    eta.v ~ 0.09
    prop.sd <- 0.2
  })
  model({
    ka <- tka
    vm <- tvm * exp(eta.vm)
    km <- tkm
    v <- tv * exp(eta.v)
    c2 <- central / v
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - vm * c2 / (km + c2)
    cp <- central / v
    cp ~ prop(prop.sd)
  })
}
