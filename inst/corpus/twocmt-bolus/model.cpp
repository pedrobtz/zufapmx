$PROB Two-compartment model, intravenous bolus

$PARAM TVCL = 4, TVV1 = 20, TVQ = 2, TVV2 = 40

$PKMODEL advan = 3, trans = 4, cmt = "CENT PERIPH"

$MAIN
double CL = TVCL * exp(ETA(1));
double V1 = TVV1 * exp(ETA(2));
double Q = TVQ;
double V2 = TVV2;

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V1;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
