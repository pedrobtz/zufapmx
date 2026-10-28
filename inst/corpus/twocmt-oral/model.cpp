$PROB Two-compartment model, first-order absorption

$PARAM TVKA = 1, TVCL = 4, TVV2 = 20, TVQ = 2, TVV3 = 40

$PKMODEL cmt = "DEPOT CENT PERIPH", depot = TRUE

$MAIN
double KA = TVKA;
double CL = TVCL * exp(ETA(1));
double V2 = TVV2 * exp(ETA(2));
double Q = TVQ;
double V3 = TVV3;

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V2;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
