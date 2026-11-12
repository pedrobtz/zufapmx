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

# A data frame as NONMEM writes a $TABLE: a title line, a header of names
# 12 wide, and values in 1PE11.4 (5 significant digits by default).
nm_table_lines <- function(df, digits = 5, title = TRUE) {
  fmt <- sprintf("%%%d.%dE", digits + 7, digits - 1)
  rows <- apply(as.matrix(df), 1, function(r) {
    paste0(" ", paste(sprintf(fmt, r), collapse = " "))
  })
  c(
    if (title) "TABLE NO.  1",
    paste0(" ", paste(formatC(names(df), width = -11), collapse = " ")),
    rows
  )
}

local_nm_table <- function(df, ..., env = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".tab", .local_envir = env)
  writeLines(nm_table_lines(df, ...), file)
  file
}

# The records of one subject's single dose of 100 into compartment 1 and
# observations at `times`, with `pred` at the observations.
table_single_dose <- function(times, pred, id = 1) {
  data.frame(
    ID = id,
    TIME = c(0, times),
    AMT = c(100, rep(0, length(times))),
    EVID = c(1, rep(0, length(times))),
    CMT = 1,
    PRED = c(0, pred)
  )
}
