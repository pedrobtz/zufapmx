$PROB Three-compartment model, zero-order infusion

$PARAM TVCL = 4, TVV1 = 10, TVQ2 = 2, TVV2 = 20, TVQ3 = 0.5, TVV3 = 40

$PKMODEL advan = 11, trans = 4, cmt = "CENT PERIPH1 PERIPH2"

$MAIN
double CL = TVCL * exp(ETA(1));
double V1 = TVV1 * exp(ETA(2));
double Q2 = TVQ2;
double V2 = TVV2;
double Q3 = TVQ3;
double V3 = TVV3;

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V1;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
