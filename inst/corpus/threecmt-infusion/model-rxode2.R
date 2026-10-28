function() {
  ini({
    tcl <- c(0, 4)
    tv <- c(0, 10)
    tq <- c(0, 2)
    tvp <- c(0, 20)
    tq2 <- c(0, 0.5)
    tvp2 <- c(0, 40)
    eta.cl ~ 0.09
    eta.v ~ 0.09
    prop.sd <- 0.2
  })
  model({
    cl <- tcl * exp(eta.cl)
    v <- tv * exp(eta.v)
    q <- tq
    vp <- tvp
    q2 <- tq2
    vp2 <- tvp2
    cp <- linCmt()
    cp ~ prop(prop.sd)
  })
}
