# Stage 3 — rxode2 reader and writer: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans.

**Goal:** `pmx_read_rxode2()` and `pmx_write_rxode2()` with fidelity and
losses; corpus and `nlmixr2lib` models round-trip `exact`.

**Architecture:** The reader walks the body of a model function as R
syntax (no rxode2 needed): `ini({})` gives parameters and omega blocks,
`model({})` is split into individual statements, structure (`linCmt()` or
`d/dt()`), dosing attributes and endpoints with residual models. The
writer prints the representation back as a function; it then reads its own
output and compares the representation (provenance aside) to set the
fidelity.

**Spec:** design §4.2 (rxode2), §4.3, §6, D9.

## Decisions

- Fidelity per writer (§6): a model with estimates written to rxode2 loses
  its initial values (rxode2 `ini()` holds one value); a model without
  estimates loses nothing. The "initial values written" loss of §6 belongs
  to writers whose format holds both (NONMEM, Stage 5).
- Reader losses (labels, `description`, comments) are recorded in
  `provenance$losses`; read fidelity is `lossy` when there are any.
- Residual parameters of rxode2 are fixed effects with role `other`;
  `sigma` is empty for rxode2-read models.
- Role inference: a fixed effect gets the closed-form role of the
  canonical parameter (`cl`, `v`/`vp`/`vp2`, `ka`) whose statement reads
  it as the only fixed effect; `other` otherwise.
- Eta transform: `exp` for `p <- exp(th + eta)` or `p <- th * exp(eta)`;
  `additive` for `p <- th + eta`; `expression` otherwise.
- Anything outside §4.2's list or the function table is a
  `zufapmx_parse_error`; `nlmixr2lib` models that use it are refused.

## Review Focus

- A model function with no `ini()` or no `model()`.
- An endpoint with a residual model but no prediction assignment.
- Two endpoints.
- `linCmt()` with names `vc`, `v1`, `v2`/`v3` variants.
- A residual call outside `add`, `prop`, `combined1/2`, `lnorm`, `pow`.

## Tasks

1. Reader (`R/read-rxode2.R`, `tests/testthat/test-read-rxode2.R`).
2. Writer and fidelity (`R/write-rxode2.R`, `R/written.R`,
   `tests/testthat/test-write-rxode2.R`): corpus round trips `exact`,
   output parses with rxode2 and simulates to `expected.csv`.
3. `nlmixr2lib` sweep (`tests/testthat/test-nlmixr2lib.R`): accepted
   models round-trip `exact`; refused ones raise `zufapmx_parse_error`;
   counts recorded in the roadmap.
