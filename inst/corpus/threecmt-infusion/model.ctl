$PROBLEM Three-compartment model, zero-order infusion
$INPUT ID TIME EVID AMT RATE CMT DV
$DATA data.csv IGNORE=@
$SUBROUTINES ADVAN11 TRANS4
$PK
  CL = THETA(1) * EXP(ETA(1))
  V1 = THETA(2) * EXP(ETA(2))
  Q2 = THETA(3)
  V2 = THETA(4)
  Q3 = THETA(5)
  V3 = THETA(6)
  S1 = V1
$ERROR
  IPRED = F
  Y = IPRED * (1 + EPS(1))
$THETA
  (0, 4)    ; CL
  (0, 10)   ; V1
  (0, 2)    ; Q2
  (0, 20)   ; V2
  (0, 0.5)  ; Q3
  (0, 40)   ; V3
$OMEGA 0.09 0.09
$SIGMA 0.04
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999
