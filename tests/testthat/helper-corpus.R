# Typical-subject simulation of corpus entries: etas and residual error at
# zero, on corpus_design(), with tight solver tolerances so that ODE and
# closed-form solutions are comparable at the design's 1e-6. Each function
# returns the predictions at the design's observation times.
# tools/corpus-expected.R and tools/oracle-agreement.R use these too, so the
# files, the script and the tests cannot drift apart.

corpus_atol <- 1e-12
corpus_rtol <- 1e-10

corpus_times <- function(regimen) {
  design <- corpus_design(regimen)
  design$time[design$evid == 0]
}

corpus_expected <- function(path) {
  utils::read.csv(file.path(path, "expected.csv"))
}

# Solve an rxode2 model function or UI. `cmt` is the dose compartment, by
# name or by number; every corpus entry doses into compartment 1 in NONMEM
# numbering.
corpus_solve_rxode2 <- function(model, regimen, output, cmt = 1) {
  dose <- corpus_design(regimen)[1, ]
  events <- if (dose$dur > 0) {
    rxode2::et(amt = dose$amt, dur = dose$dur, cmt = cmt)
  } else {
    rxode2::et(amt = dose$amt, cmt = cmt)
  }
  events <- rxode2::et(events, corpus_times(regimen))

  # Predictions are read, not simulated observations: only etas matter.
  model <- suppressMessages(rxode2::zeroRe(model, which = "omega"))
  sim <- suppressMessages(rxode2::rxSolve(
    model,
    events,
    returnType = "data.frame",
    atol = corpus_atol,
    rtol = corpus_rtol
  ))
  sim[[output]]
}

# An entry's own rxode2 model, which defines expected.csv.
corpus_solve_entry_rxode2 <- function(path, regimen) {
  fun <- eval(parse(file.path(path, "model-rxode2.R"), keep.source = FALSE))
  cmt <- corpus_design(regimen)$cmt[[1]]
  corpus_solve_rxode2(fun, regimen, "cp", cmt = cmt)
}

# Solve an mrgsolve model file, reading the prediction from IPRED.
corpus_solve_mrgsolve <- function(cpp, regimen) {
  # A unique model name per compile: mrgsolve names the shared library after
  # the model, and two models of one name in one session collide.
  dir <- tempfile("mrg")
  dir.create(dir)
  name <- basename(dir)
  file.copy(cpp, file.path(dir, paste0(name, ".cpp")))
  mod <- suppressMessages(mrgsolve::mread(name, project = dir, quiet = TRUE))
  mod <- mrgsolve::zero_re(mod)
  mod <- mrgsolve::update(mod, atol = corpus_atol, rtol = corpus_rtol)

  dose <- corpus_design(regimen)[1, ]
  rate <- if (dose$dur > 0) dose$amt / dose$dur else 0
  ev <- mrgsolve::ev(amt = dose$amt, cmt = 1, rate = rate)
  out <- mrgsolve::mrgsim_e(
    mod,
    ev,
    end = -1,
    add = corpus_times(regimen),
    obsonly = TRUE,
    output = "df"
  )
  out$IPRED
}

# The oracles: translate the entry's model.ctl with a CRAN converter and
# simulate the translation in its target engine.
corpus_oracle_nonmem2rx <- function(path, regimen) {
  ui <- suppressMessages(nonmem2rx::nonmem2rx(
    file.path(path, "model.ctl"),
    validate = FALSE
  ))
  corpus_solve_rxode2(ui, regimen, "ipred")
}

corpus_oracle_amp_sim <- function(path, regimen) {
  out <- tempfile("amp")
  suppressMessages(amp.sim::convert_nonmem(
    file.path(path, "model.ctl"),
    out,
    mod_return = "IPRED",
    type_return = "mrgsolve",
    verbose = FALSE,
    overwrite = TRUE
  ))
  corpus_solve_mrgsolve(paste0(out, ".cpp"), regimen)
}

corpus_max_rel_dev <- function(got, expected) {
  max(abs(got - expected) / abs(expected))
}

# An entry's rxode2 model function, and the model read from it.
corpus_rxode2 <- function(id) {
  eval(parse(system.file("corpus", id, "model-rxode2.R", package = "zupkpmx")))
}

corpus_rxode2_model <- function(id) {
  pmx_read_rxode2(corpus_rxode2(id))
}

# Any model as a simulable rxode2 function, through the rxode2 writer.
as_rxode2_function <- function(model) {
  eval(parse(text = pmx_write_rxode2(model)$text))
}

# Write a control stream to a temporary file and read it.
read_ctl <- function(lines, env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".ctl", .local_envir = env)
  writeLines(lines, path)
  pmx_read_nonmem(path)
}

ctl_onecmt <- function(
  pk = character(),
  error = c("IPRED = F", "Y = IPRED * (1 + EPS(1))"),
  extra = character()
) {
  c(
    "$PROBLEM test",
    "$SUBROUTINES ADVAN2 TRANS2",
    "$PK",
    "KA = THETA(1)",
    "CL = THETA(2) * EXP(ETA(1))",
    "V = THETA(3) * EXP(ETA(2))",
    "S2 = V",
    pk,
    "$ERROR",
    error,
    "$THETA (0, 1) (0, 4) (0, 50)",
    "$OMEGA 0.09 0.09",
    "$SIGMA 0.04",
    extra
  )
}
