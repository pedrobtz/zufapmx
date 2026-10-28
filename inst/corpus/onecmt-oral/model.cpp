$PROB One-compartment model, first-order absorption

$PARAM TVKA = 1, TVCL = 4, TVV = 50

$PKMODEL advan = 2, trans = 2, cmt = "DEPOT CENT"

$MAIN
double KA = TVKA;
double CL = TVCL * exp(ETA(1));
double V = TVV * exp(ETA(2));

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
