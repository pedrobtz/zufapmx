$PROBLEM One-compartment model, absorption lag and bioavailability
$INPUT ID TIME EVID AMT CMT DV
$DATA data.csv IGNORE=@
$SUBROUTINES ADVAN2 TRANS2
$PK
  KA = THETA(1)
  CL = THETA(2) * EXP(ETA(1))
  V  = THETA(3) * EXP(ETA(2))
  ALAG1 = THETA(4)
  F1 = THETA(5)
  S2 = V
$ERROR
  IPRED = F
  Y = IPRED * (1 + EPS(1))
$THETA
  (0, 1)     ; KA
  (0, 4)     ; CL
  (0, 50)    ; V
  (0, 0.25)  ; ALAG1
  (0, 0.6, 1) ; F1
$OMEGA 0.09 0.09
$SIGMA 0.04
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999
