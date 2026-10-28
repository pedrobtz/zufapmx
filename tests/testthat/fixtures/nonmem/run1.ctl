$PROBLEM Two-compartment model, first-order absorption
$INPUT ID TIME EVID AMT CMT DV
$DATA data.csv IGNORE=@
$SUBROUTINES ADVAN4 TRANS4
$PK
  KA = THETA(1)
  CL = THETA(2) * EXP(ETA(1))
  V2 = THETA(3) * EXP(ETA(2))
  Q  = THETA(4)
  V3 = THETA(5)
  S2 = V2
$ERROR
  IPRED = F
  Y = IPRED * (1 + EPS(1))
$THETA
  (0, 1)   ; KA
  (0, 4)   ; CL
  (0, 20)  ; V2
  (0, 2)   ; Q
  (0, 40)  ; V3
$OMEGA 0.09 0.09
$SIGMA 0.04
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999
