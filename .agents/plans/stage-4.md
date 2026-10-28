# Stage 4 — NONMEM reader: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans.

**Goal:** `pmx_read_nonmem(file, estimates = "auto")` reads the §4.2
subset into the representation, with estimates from `run.xml`, `.ext` or
`.lst`.

**Spec:** design §4.2, §6 (estimates), §8, D6, D7, D13.

## Decisions

- Tool names are kept as symbols: `THETA(1)` is `THETA1`, `ETA(1)` is
  `ETA1`, `EPS(1)` is `EPS1`; `A(n)` is the compartment's name from
  `$MODEL` or the `ADVAN`'s defaults (`DEPOT`, `CENTRAL`, `PERIPH`...;
  closed forms use the representation's `depot`, `central`,
  `peripheral1`...). The NONMEM writer (Stage 5) maps them back.
- Closed forms: `ADVAN1`–`4`, `11`, `12` with `TRANS2` (one compartment)
  or `TRANS4`; other `TRANS` are refused. The prediction is
  `central / <S expression>`.
- `$ERROR`: `IPRED = F` is an alias; `Y = ...` defines the residual model,
  recognised as additive, proportional, combined or exponential over
  `EPS`, or kept as `expression` with the expression; other statements
  that `Y` does not use are losses. The endpoint is named `Y`.
- Omega and sigma blocks gain an optional `estimate` matrix.
- `estimates = "auto"`: `run.xml` through zuxml when both exist, else
  `.ext`, else none. `"xml"`, `"ext"`, `"lst"`, `"none"` force a source.
- Refused: verbatim code (`"` lines), `$INFN`, `$PRED`, `$MIX`, `$PRIOR`,
  `SAME`, `SD`/`CORR` options, user subroutines (`OTHER=`), and any
  function or variable outside the table. Each is `zupkpmx_parse_error`
  with file, line and column.
- Losses: `$PROBLEM`, `$INPUT`, `$DATA`, `$ESTIMATION`, `$COVARIANCE`,
  `$TABLE`, comments, the ODE solver (D13), unused `$ERROR` statements.

## Tasks

1. Records, tokenizer and expression/statement parser (`R/nmtran.R`).
2. `$THETA`/`$OMEGA`/`$SIGMA`, `$SUBROUTINES`/`$MODEL`, assembly
   (`R/read-nonmem.R`); every corpus `model.ctl` reads and its typical
   predictions, written through rxode2 (Stage 5's writer is not needed:
   the test simulates the representation through `pmx_write_rxode2()`
   after mapping sigma), match `expected.csv`.
3. Estimates (`R/nonmem-estimates.R`) with fixtures for each source.
