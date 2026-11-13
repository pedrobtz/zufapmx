# Random closed-form models for test-property.R (design §9).

# A random closed-form model: an ADVAN, positive parameter values, etas on
# some parameters, a residual model, and for a depot a lag and a
# bioavailability. Returns the control stream and what it holds.
# `advan` and `with_lag` (TRUE for a lag and bioavailability) can be fixed,
# so a test can cover every structure rather than sample them (#159).
random_closed_model <- function(advan = NULL, with_lag = NULL) {
  advan <- advan %||% sample(c(1L, 2L, 3L, 4L, 11L, 12L), 1)
  form <- closed_forms[[as.character(advan)]]
  names <- unname(form$names)
  values <- signif(exp(stats::runif(length(names), log(0.2), log(60))), 4)
  depot <- "KA" %in% names
  eta_on <- names[stats::runif(length(names)) < 0.5]
  if (length(eta_on) == 0) {
    eta_on <- names[[1]]
  }
  pk <- vapply(
    seq_along(names),
    function(i) {
      eta <- match(names[[i]], eta_on)
      sprintf(
        "%s = THETA(%d)%s",
        names[[i]],
        i,
        if (is.na(eta)) "" else sprintf(" * EXP(ETA(%d))", eta)
      )
    },
    character(1)
  )
  thetas <- sprintf("$THETA (0, %s)", format(values))
  dosing <- character()
  lag <- NA
  bio <- NA
  if (depot && (with_lag %||% (stats::runif(1) < 0.5))) {
    lag <- signif(stats::runif(1, 0.1, 1), 3)
    bio <- signif(stats::runif(1, 0.3, 1), 3)
    k <- length(names)
    dosing <- c(
      sprintf("ALAG1 = THETA(%d)", k + 1),
      sprintf("F1 = THETA(%d)", k + 2)
    )
    thetas <- c(
      thetas,
      sprintf("$THETA (0, %s)", lag),
      sprintf("$THETA (0, %s, 1)", bio)
    )
  }
  central <- form$names[["v"]]
  residual <- sample(
    c("additive", "proportional", "combined", "exponential"),
    1
  )
  y <- switch(
    residual,
    additive = "Y = IPRED + EPS(1)",
    proportional = "Y = IPRED * (1 + EPS(1))",
    combined = "Y = IPRED * (1 + EPS(1)) + EPS(2)",
    exponential = "Y = IPRED * EXP(EPS(1))"
  )
  lines <- c(
    "$PROBLEM random closed form",
    sprintf("$SUBROUTINES ADVAN%d TRANS%d", advan, form$trans),
    "$PK",
    pk,
    dosing,
    sprintf("S%d = %s", form$central, central),
    "$ERROR",
    "IPRED = F",
    y,
    thetas,
    sprintf(
      "$OMEGA %s",
      paste(signif(stats::runif(length(eta_on), 0.01, 0.3), 3), collapse = " ")
    ),
    sprintf("$SIGMA %s", if (residual == "combined") "0.04 0.01" else "0.04")
  )
  list(
    lines = lines,
    advan = advan,
    values = stats::setNames(values, names),
    lag = lag,
    bio = bio
  )
}
