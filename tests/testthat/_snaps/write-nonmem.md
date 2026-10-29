# written text is stable

    Code
      print(pmx_write_nonmem(corpus_rxode2_model("twocmt-oral")))
    Output
      $PROBLEM zupkpmx model read from rxode2
      ; zupkpmx: $INPUT and $DATA are placeholders; point them at your dataset.
      $INPUT ID TIME EVID AMT RATE CMT DV
      $DATA data.csv IGNORE=@
      $SUBROUTINES ADVAN4 TRANS4
      $PK
      KA = THETA(1)
      CL = THETA(2) * EXP(ETA(1))
      V = THETA(3) * EXP(ETA(2))
      Q = THETA(4)
      VP = THETA(5)
      V2 = V
      V3 = VP
      S2 = V2
      $ERROR
      Y = F * (1 + EPS(1))
      $THETA (0, 1) ; tka
      $THETA (0, 4) ; tcl
      $THETA (0, 20) ; tv
      $THETA (0, 2) ; tq
      $THETA (0, 40) ; tvp
      $OMEGA 0.09
      $OMEGA 0.09
      $SIGMA 0.04
      # nonmem, fidelity: equivalent

---

    Code
      print(pmx_write_nonmem(corpus_rxode2_model("onecmt-oral-mm")))
    Output
      $PROBLEM zupkpmx model read from rxode2
      ; zupkpmx: $INPUT and $DATA are placeholders; point them at your dataset.
      $INPUT ID TIME EVID AMT RATE CMT DV
      $DATA data.csv IGNORE=@
      $SUBROUTINES ADVAN13 TOL=9
      $MODEL COMP=(DEPOT) COMP=(CENTRAL)
      $PK
      KA = THETA(1)
      VM = THETA(2) * EXP(ETA(1))
      KM = THETA(3)
      V = THETA(4) * EXP(ETA(2))
      S2 = V
      $DES
      C2 = A(2) / V
      DADT(1) = -KA * A(1)
      DADT(2) = KA * A(1) - VM * C2 / (KM + C2)
      $ERROR
      Y = F * (1 + EPS(1))
      $THETA (0, 1) ; tka
      $THETA (0, 20) ; tvm
      $THETA (0, 2) ; tkm
      $THETA (0, 50) ; tv
      $OMEGA 0.09
      $OMEGA 0.09
      $SIGMA 0.04
      # nonmem, fidelity: equivalent

