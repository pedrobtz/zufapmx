$PROBLEM One-compartment model, correlated random effects
$INPUT ID TIME EVID AMT CMT DV
$DATA data.csv IGNORE=@
$SUBROUTINES ADVAN2 TRANS2
$PK
  KA = THETA(1) * EXP(ETA(1))
  CL = THETA(2) * EXP(ETA(2))
  V  = THETA(3) * EXP(ETA(3))
  S2 = V
$ERROR
  IPRED = F
  Y = IPRED * (1 + EPS(1)) + EPS(2)
$THETA (0, 1) (0, 4) (0, 50)
$OMEGA BLOCK(3) 0.1 0.01 0.2 0.02 0.03 0.3
$SIGMA 0.04 0.5
