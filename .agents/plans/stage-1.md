# Stage 1 — Corpus skeleton and oracles: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans
> to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax.

**Goal:** A shipped model corpus with typical-subject predictions, a
`zufapmx_corpus()` index, and a recorded comparison of the CRAN converters
against it.

**Architecture:** One directory per entry under `inst/corpus/`, described by
an `entry.dcf` read with base `read.dcf()`. Predictions in `expected.csv`
are produced by a development script with rxode2 and checked by tests that
skip without it. A second development script runs the converters and writes
`.agents/oracle-agreement.md`.

**Tech Stack:** base R; rxode2, mrgsolve, nonmem2rx, amp.sim in `Suggests`;
nonmem2mrgsolve (GitHub) only in the development script, if installed.

**Spec:** `.agents/design.md` §7, §9, D8, D12; `.agents/roadmap.md` Stage 1.

## Global Constraints

- No `Imports` (D8). Every engine and oracle is `Suggests` and every test
  that uses one skips without it.
- Every `Suggests` entry is on CRAN (D11, D12).
- Corpus content is MIT-compatible: written for this package from the
  PREDPP model structures. Nothing copied from GPL libraries (`nlmixr2lib`,
  `modlib()`); those are used at test time from their installed copies.
- Exported functions are `pmx_*`, except `zufapmx_corpus()` (D10).
- Tests assert condition classes, not message text; `withr` for state.

## Review Focus

- An `entry.dcf` missing a required field: `zufapmx_corpus()` must fail
  loudly naming the entry, not return a short data frame. Test in Task 1.
- A format file listed in `entry.dcf` but absent on disk. Test in Task 1.
- `expected.csv` regenerated with a different rxode2 version drifting
  silently: the consistency test uses a relative tolerance of `1e-6`, the
  design's default `tol`. Task 2.
- A converter that errors on one entry must not stop the agreement script;
  the error message is recorded in the table. Task 3.
- Oracles absent on a CI leg (`nosuggests`): every engine test skips. Task 2.

---

### Task 1: Corpus entries and `zufapmx_corpus()`

**Files:**

- Create: `inst/corpus/<id>/{entry.dcf,model.ctl,model-rxode2.R,model.cpp}`
  for ids `onecmt-oral` (ADVAN2 TRANS2), `twocmt-bolus` (ADVAN3 TRANS4),
  `twocmt-oral` (ADVAN4 TRANS4), `threecmt-infusion` (ADVAN11 TRANS4),
  `onecmt-oral-mm` (ADVAN13, Michaelis-Menten elimination in `$DES`).
- Create: `R/corpus.R`, `tests/testthat/test-corpus.R`

**Interfaces:**

- Produces: `zufapmx_corpus()` returning a data frame, one row per entry,
  columns `id`, `title`, `structure`, `regimen`, `formats` (comma-separated
  `nonmem,rxode2,mrgsolve` subset), `source`, `path` (absolute directory).
  Rows ordered by `id`.
- `entry.dcf` fields: `Id`, `Title`, `Structure`, `Regimen` (`bolus`,
  `oral` or `infusion`), `Formats`, `Source`. All required.
- Error class `zufapmx_corpus_error` for a malformed entry.

Each model: one observation endpoint `cp = central / v`, log-normal etas on
clearance and central volume, proportional residual error. Estimates are
the initial values (no `.ext` in Stage 1).

- [ ] Test `zufapmx_corpus()` returns the five ids, the seven columns, and
  existing `path`s; every format listed has its file.
- [ ] Test a temp corpus with an entry missing `Formats` raises
  `zufapmx_corpus_error` (internal `corpus_read(root)` takes the root).
- [ ] Implement `corpus_read(root)` and `zufapmx_corpus()` (calls it on
  `system.file("corpus", package = "zufapmx")`). Roxygen, exported.
- [ ] `devtools::document()`, `devtools::test(filter = "corpus")` green.
- [ ] Commit.

### Task 2: Expected predictions

**Files:**

- Create: `tools/corpus-expected.R`, `inst/corpus/<id>/expected.csv`,
  `tests/testthat/test-corpus-expected.R`
- Modify: `DESCRIPTION` (`Suggests: rxode2`)

**Interfaces:**

- Consumes: `zufapmx_corpus()`.
- Produces: `expected.csv` with columns `regimen`, `time`, `endpoint`,
  `pred`; the standard Stage 1 design is a single 100-unit dose at time 0
  (infusion: over 1 h) and times `0.5, 1, 2, 4, 6, 8, 12, 24`. Typical
  subject: etas zero, residual error off (D5).
- Internal `corpus_design()` returning that event table as a data frame,
  so `pmx_design()` in Stage 7 can start from it.

- [ ] Write `tools/corpus-expected.R`: for each entry, source
  `model-rxode2.R`, `rxode2::zeroRe()`, solve on `corpus_design()`, write
  `expected.csv` with `signif(pred, 10)`.
- [ ] Run it; commit the CSVs.
- [ ] Test (skips without rxode2, `skip_on_cran()`): each entry's rxode2
  model reproduces `expected.csv` within relative `1e-6`.
- [ ] Commit.

### Task 3: Oracle agreement

**Files:**

- Create: `tools/oracle-agreement.R`, `.agents/oracle-agreement.md`
- Modify: `DESCRIPTION` (`Suggests: mrgsolve, nonmem2rx, amp.sim`)

**Interfaces:**

- Consumes: `zufapmx_corpus()`, `corpus_design()`, `expected.csv`.
- Produces: a Markdown table, one row per entry, one column per converter
  (`nonmem2rx`, `amp.sim`, `nonmem2mrgsolve`) plus mrgsolve's own
  `model.cpp`: `agrees (max rel dev x)`, `differs (max rel dev x)` or
  `error: <message>`; installed versions in the header.

- [ ] Write the script: for each entry with `model.ctl`, translate with
  each converter installed, simulate the typical subject on
  `corpus_design()` in the target engine, compare with `expected.csv`.
  `tryCatch` per cell. Also simulate `model.cpp` with mrgsolve.
- [ ] Run it; commit `.agents/oracle-agreement.md`.
- [ ] Commit.

### Task 4: Wiring and records

**Files:**

- Modify: `NEWS.md`, `CLAUDE.md` (oracles: `amp.sim`, not
  `nonmem2mrgsolve`), `.agents/roadmap.md` (Done, D12 and licence
  deviation), `.Rbuildignore` (`^tools$`).

- [ ] `devtools::check()` clean; `devtools::test(shuffle = TRUE)` green.
- [ ] Commit.
