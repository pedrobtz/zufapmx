# corpus mrgsolve models round-trip exactly

    Code
      print(pmx_write_mrgsolve(corpus_rxode2_model("twocmt-oral")))
    Output
      $PROB zupkpmx model read from rxode2
      
      $PARAM @annotated
      tka : 1 : [0, Inf]
      tcl : 4 : [0, Inf]
      tv : 20 : [0, Inf]
      tq : 2 : [0, Inf]
      tvp : 40 : [0, Inf]
      
      $PKMODEL advan = 4, trans = 4, cmt = "DEPOT CENT PERIPH1"
      
      $MAIN
      double ka = tka;
      double cl = tcl * exp(ETA(1));
      double v = tv * exp(ETA(2));
      double q = tq;
      double vp = tvp;
      double CL = cl;
      double V2 = v;
      double Q = q;
      double V3 = vp;
      double KA = ka;
      
      $OMEGA
      0.09
      
      $OMEGA
      0.09
      
      $SIGMA
      0.04
      
      $TABLE
      double IPRED = CENT / V2;
      double Y = IPRED * (1 + EPS(1));
      
      $CAPTURE IPRED Y
      # mrgsolve, fidelity: equivalent

---

    Code
      print(pmx_write_mrgsolve(corpus_rxode2_model("onecmt-oral-mm")))
    Output
      $PROB zupkpmx model read from rxode2
      
      $PARAM @annotated
      tka : 1 : [0, Inf]
      tvm : 20 : [0, Inf]
      tkm : 2 : [0, Inf]
      tv : 50 : [0, Inf]
      
      $CMT depot central
      
      $MAIN
      double ka = tka;
      double vm = tvm * exp(ETA(1));
      double km = tkm;
      double v = tv * exp(ETA(2));
      
      $ODE
      double c2 = central / v;
      dxdt_depot = -ka * depot;
      dxdt_central = ka * depot - vm * c2 / (km + c2);
      
      $OMEGA
      0.09
      
      $OMEGA
      0.09
      
      $SIGMA
      0.04
      
      $TABLE
      double IPRED = central / v;
      double Y = IPRED * (1 + EPS(1));
      
      $CAPTURE IPRED Y
      # mrgsolve, fidelity: equivalent

