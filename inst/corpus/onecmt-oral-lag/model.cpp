$PROB One-compartment model, absorption lag and bioavailability

$PARAM TVKA = 1, TVCL = 4, TVV = 50, TVLAG = 0.25, TVF = 0.6

$PKMODEL advan = 2, trans = 2, cmt = "GUT CENT"

$MAIN
double KA = TVKA;
double CL = TVCL * exp(ETA(1));
double V = TVV * exp(ETA(2));
ALAG_GUT = TVLAG;
F_GUT = TVF;

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
