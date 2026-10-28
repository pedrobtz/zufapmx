$PROB One-compartment model, Michaelis-Menten elimination

$PARAM TVKA = 1, TVVM = 20, TVKM = 2, TVV = 50

$CMT DEPOT CENT

$MAIN
double KA = TVKA;
double VM = TVVM * exp(ETA(1));
double KM = TVKM;
double V = TVV * exp(ETA(2));

$ODE
double C2 = CENT / V;
dxdt_DEPOT = -KA * DEPOT;
dxdt_CENT = KA * DEPOT - VM * C2 / (KM + C2);

$OMEGA 0.09 0.09

$SIGMA 0.04

$TABLE
double IPRED = CENT / V;
double Y = IPRED * (1 + EPS(1));

$CAPTURE IPRED Y
