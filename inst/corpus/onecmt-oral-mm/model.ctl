$PROBLEM One-compartment model, Michaelis-Menten elimination
$INPUT ID TIME EVID AMT CMT DV
$DATA data.csv IGNORE=@
$SUBROUTINES ADVAN13 TOL=9
$MODEL
  COMP=(DEPOT, DEFDOSE)
  COMP=(CENTRAL, DEFOBS)
$PK
  KA = THETA(1)
  VM = THETA(2) * EXP(ETA(1))
  KM = THETA(3)
  V  = THETA(4) * EXP(ETA(2))
  S2 = V
$DES
  C2 = A(2) / V
  DADT(1) = -KA * A(1)
  DADT(2) = KA * A(1) - VM * C2 / (KM + C2)
$ERROR
  IPRED = F
  Y = IPRED * (1 + EPS(1))
$THETA
  (0, 1)   ; KA
  (0, 20)  ; VM
  (0, 2)   ; KM
  (0, 50)  ; V
$OMEGA 0.09 0.09
$SIGMA 0.04
$ESTIMATION METHOD=1 INTERACTION MAXEVAL=9999
