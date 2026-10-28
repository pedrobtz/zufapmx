# Typical-subject predictions for one corpus entry through rxode2: etas and
# residual error at zero, on corpus_design(). tools/corpus-expected.R uses
# this too, so expected.csv and its test cannot drift apart.
corpus_solve_rxode2 <- function(path, regimen) {
  design <- corpus_design(regimen)
  fun <- eval(parse(file.path(path, "model-rxode2.R"), keep.source = FALSE))
  model <- suppressMessages(rxode2::zeroRe(fun))

  dose <- design[design$evid == 1, ]
  events <- if (dose$dur > 0) {
    rxode2::et(amt = dose$amt, dur = dose$dur, cmt = dose$cmt)
  } else {
    rxode2::et(amt = dose$amt, cmt = dose$cmt)
  }
  events <- rxode2::et(events, design$time[design$evid == 0])

  sim <- suppressMessages(
    rxode2::rxSolve(model, events, returnType = "data.frame")
  )
  data.frame(
    regimen = regimen,
    time = sim$time,
    endpoint = "cp",
    pred = sim$cp
  )
}

corpus_expected <- function(path) {
  utils::read.csv(file.path(path, "expected.csv"))
}
