# The performance targets of design §10. Run from the package root:
#   Rscript tools/bench/performance.R
# Needs rxode2 and mrgsolve for the comparison overhead.

suppressMessages(devtools::load_all(quiet = TRUE))

median_ms <- function(expr, n = 20) {
  f <- function() force(expr)
  expr <- substitute(expr)
  env <- parent.frame()
  times <- vapply(
    seq_len(n),
    function(i) {
      system.time(eval(expr, env))[["elapsed"]]
    },
    numeric(1)
  )
  1000 * stats::median(times)
}

# A 500-line control stream: an ODE model with a long $PK and $DES.
ctl_500 <- function() {
  n <- 161
  c(
    "$PROBLEM 500 lines",
    "$INPUT ID TIME EVID AMT CMT DV WT",
    "$DATA data.csv IGNORE=@",
    "$SUBROUTINES ADVAN13 TOL=9",
    "$MODEL COMP=(DEPOT) COMP=(CENT)",
    "$PK",
    "KA = THETA(1)",
    "CL = THETA(2) * EXP(ETA(1)) * (WT / 70)**0.75",
    "V = THETA(3) * EXP(ETA(2))",
    sprintf("X%d = CL * %d.5 / (V + %d)", seq_len(n), seq_len(n), seq_len(n)),
    "$DES",
    sprintf("Z%d = A(2) / V * %d", seq_len(n), seq_len(n)),
    "DADT(1) = -KA * A(1)",
    "DADT(2) = KA * A(1) - CL / V * A(2)",
    "$ERROR",
    sprintf("W%d = F * %d", seq_len(n), seq_len(n)),
    "Y = F * (1 + EPS(1))",
    "$THETA (0, 1) (0, 4) (0, 50)",
    "$OMEGA 0.09 0.09",
    "$SIGMA 0.04"
  )
}
path <- tempfile(fileext = ".ctl")
writeLines(ctl_500(), path)
cat(sprintf("control stream: %d lines\n", length(readLines(path))))
cat(sprintf(
  "read a 500-line control stream: %.1f ms\n",
  median_ms(pmx_read_nonmem(path))
))

corpus <- zufapmx_corpus()
read_corpus <- function() {
  for (p in corpus$path) {
    pmx_read_nonmem(file.path(p, "model.ctl"))
    pmx_read_mrgsolve(file.path(p, "model.cpp"))
    pmx_read_rxode2(eval(parse(
      file.path(p, "model-rxode2.R"),
      keep.source = FALSE
    )))
  }
}
cat(sprintf(
  "read the whole corpus (%d models): %.0f ms\n",
  3 * nrow(corpus),
  median_ms(read_corpus(), n = 5)
))

# pmx_compare() overhead: the engines' results are recorded on a first run
# and replayed instantly on the timed runs, so what is timed is everything
# but the simulations.
ns <- asNamespace("zufapmx")
m <- pmx_read_nonmem(file.path(
  corpus$path[corpus$id == "twocmt-oral"],
  "model.ctl"
))
w <- pmx_write_rxode2(m)
design <- pmx_design()
recorded <- list()
record <- function(f) {
  force(f)
  function(...) {
    out <- f(...)
    recorded[[length(recorded) + 1]] <<- out
    out
  }
}
testthat::with_mocked_bindings(
  invisible(pmx_compare(m, w, design)),
  engine_rxode2 = record(ns$engine_rxode2),
  engine_mrgsolve = record(ns$engine_mrgsolve)
)
at <- 0
replay <- function(...) {
  at <<- at %% length(recorded) + 1
  recorded[[at]]
}
overhead <- testthat::with_mocked_bindings(
  vapply(
    seq_len(10),
    function(i) system.time(pmx_compare(m, w, design))[["elapsed"]],
    numeric(1)
  ),
  engine_rxode2 = replay,
  engine_mrgsolve = replay
)
cat(sprintf(
  "pmx_compare() overhead beyond the simulations: %.0f ms (default design, %d points)\n",
  1000 * stats::median(overhead),
  sum(design$events$evid == 0)
))
cat(sprintf(
  "%s, R %s, %s\n",
  Sys.info()[["machine"]],
  getRversion(),
  format(Sys.Date())
))
