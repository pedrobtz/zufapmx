# written text is stable

    Code
      print(pmx_write_rxode2(corpus_rxode2_model("twocmt-oral")))
    Output
      function() {
        ini({
          tka <- c(0, 1)
          tcl <- c(0, 4)
          tv <- c(0, 20)
          tq <- c(0, 2)
          tvp <- c(0, 40)
          prop.sd <- 0.2
          eta.cl ~ 0.09
          eta.v ~ 0.09
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
      # rxode2, fidelity: exact

---

    Code
      print(pmx_write_rxode2(corpus_rxode2_model("onecmt-oral-mm")))
    Output
      function() {
        ini({
          tka <- c(0, 1)
          tvm <- c(0, 20)
          tkm <- c(0, 2)
          tv <- c(0, 50)
          prop.sd <- 0.2
          eta.vm ~ 0.09
          eta.v ~ 0.09
        })
        model({
          ka <- tka
          vm <- tvm * exp(eta.vm)
          km <- tkm
          v <- tv * exp(eta.v)
          c2 <- central/v
          d/dt(depot) <- -ka * depot
          d/dt(central) <- ka * depot - vm * c2/(km + c2)
          cp <- central/v
          cp ~ prop(prop.sd)
        })
      }
      # rxode2, fidelity: exact

