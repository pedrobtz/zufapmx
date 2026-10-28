$PROB Three-compartment model, zero-order infusion

$PARAM TVCL = 4, TVV1 = 10, TVQ2 = 2, TVV2 = 20, TVQ3 = 0.5, TVV3 = 40

$CMT CENT PERIPH1 PERIPH2

$MAIN
double CL = TVCL * exp(ETA(1));
double V1 = TVV1 * exp(ETA(2));
double Q2 = TVQ2;
double V2 = TVV2;
double Q3 = TVQ3;
double V3 = TVV3;

$ODE
dxdt_CENT = -(CL + Q2 + Q3) / V1 * CENT + Q2 / V2 * PERIPH1 +
  Q3 / V3 * PERIPH2;
dxdt_PERIPH1 = Q2 / V1 * CENT - Q2 / V2 * PERIPH1;
dxdt_PERIPH2 = Q3 / V1 * CENT - Q3 / V3 * PERIPH2;

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V1;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
