# Fake engines for testing pmx_compare() without rxode2 or mrgsolve. Each
# fake returns, for every observation record, `fun(time, id)` in a column
# named after the side's endpoint. Mocked through the package namespace,
# so only tests that ask for them see them.
local_fake_engines <- function(
  rxode2 = function(time, id) time,
  mrgsolve = function(time, id) time,
  env = parent.frame()
) {
  fake <- function(fun) {
    function(text, data, etas) {
      obs <- data[data$evid == 0, ]
      data.frame(id = obs$id, time = obs$time, Y = fun(obs$time, obs$id))
    }
  }
  testthat::local_mocked_bindings(
    engine_available = function(engine) TRUE,
    engine_version = function(engine) "0.0.0",
    engine_rxode2 = fake(rxode2),
    engine_mrgsolve = fake(mrgsolve),
    .env = env
  )
}

# A one-compartment bolus model (ADVAN1) with etas on CL and V, whose
# predictions have closed forms on every regimen of the design.
ctl_advan1 <- function(pk = character(), cl = "CL = THETA(1) * EXP(ETA(1))") {
  c(
    "$PROBLEM advan1",
    "$SUBROUTINES ADVAN1 TRANS2",
    "$PK",
    cl,
    "V = THETA(2) * EXP(ETA(2))",
    "S1 = V",
    pk,
    "$ERROR",
    "IPRED = F",
    "Y = IPRED * (1 + EPS(1))",
    "$THETA (0, 4) (0, 50)",
    "$OMEGA 0.09 0.04",
    "$SIGMA 0.04"
  )
}

# The concentration of a one-compartment model after the design's
# regimens, by superposition of exponentials.
advan1_conc <- function(regimen, t, cl, v) {
  k <- cl / v
  dose <- 100
  switch(
    regimen,
    single = dose / v * exp(-k * t),
    infusion = {
      rate <- 50
      dur <- 2
      ifelse(
        t <= dur,
        rate / cl * (1 - exp(-k * t)),
        rate / cl * (1 - exp(-k * dur)) * exp(-k * (t - dur))
      )
    },
    multiple = vapply(
      t,
      function(ti) {
        td <- seq(0, 48, by = 12)
        td <- td[td <= ti]
        sum(dose / v * exp(-k * (ti - td)))
      },
      numeric(1)
    ),
    ss = dose / v * exp(-k * t) / (1 - exp(-k * 12))
  )
}
