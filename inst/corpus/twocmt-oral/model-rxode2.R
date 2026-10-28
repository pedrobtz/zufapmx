function() {
  ini({
    tka <- c(0, 1)
    tcl <- c(0, 4)
    tv <- c(0, 20)
    tq <- c(0, 2)
    tvp <- c(0, 40)
    eta.cl ~ 0.09
    eta.v ~ 0.09
    prop.sd <- 0.2
  })
  model({
    ka <- tka
    cl <- tcl * exp(eta.cl)
    v <- tv * exp(eta.v)
    q <- tq
    vp <- tvp
    cp <- linCmt()
    cp ~ prop(prop.sd)
  })
}
