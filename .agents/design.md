# RFC 0046: `zupkpmx`, pharmacometric models read, written and proven equal

Adopted from [RFC 0046](https://github.com/pedrobtz/packages/blob/main/rfcs/started/0046-zupkpmx-pharmacometric-model-interchange.md)
on 2026-10-28. From here on this file is authoritative; the RFC is history.

- **Status:** Proposed. Merging this file adopts the design as the starting
  point for the package's own `.agents/design.md`; each decision in section
  13 stays the maintainer's until the package repository records it.
  D3, that the representation is an R object and not a file standard, and
  D6, the NM-TRAN subset, are the ones most worth a second look.
- **Date:** 2026-10-28.
- **Packages:** `zupkpmx` (not yet created; crandb returns `not_found` and
  there is no `pedrobtz/zupkpmx` repository). Pure R in v0.1.0. No
  `Imports`. `Suggests: testthat, rxode2, nlmixr2, mrgsolve, nonmem2rx,
  nonmem2mrgsolve, zuxml, zucsv, zujson, daffiz`. MIT.
- **Source:** the re-check in
  [`archive/reviews/pharmacometrics-review-2026-10-28.md`](https://github.com/pedrobtz/packages/blob/main/archive/reviews/pharmacometrics-review-2026-10-28.md)
  and the sources it lists; the nlmixr2 team's report of its seminar to
  FDA reviewers (29 April 2026); the `babelmixr2`, `nonmem2rx` and
  `monolix2rx` documentation; Pharmpy 2.2 documentation; the Genmab
  mrgsolve–rxode2 bridge (PAGE 2026, abstract 12003); `nonmem2mrgsolve`
  1.2.0, `nm2mrg` and `amp.sim` 0.1.1; the PharmML paper (CPT:PSP 2015)
  and the DDMoRe Foundation pages; the NONMEM Users Guides for NM-TRAN
  and PREDPP; the mrgsolve user guide; the rxode2 model-syntax articles.
  Nothing has been built.

## 1. What zupkpmx is

`zupkpmx` holds a population PK/PD model as one R object, reads that object
from the formats modellers already have, writes it to the format another
tool wants, and proves that two models are the same model. The proof is
the part no existing tool offers: `pmx_compare()` simulates both models on
a canonical design through the engines each one runs on and reports the
largest deviation against a tolerance, with a report that can be attached
to a submission.

The one-line statement that governs every decision below:

> **A model is the same model in every tool, and `zupkpmx` shows it.**

What it is not:

- **Not a modelling language for people.** PharmML tried to be the format
  everyone writes and nobody adopted it. The representation here is an R
  list with a class, documented as data, serialisable to JSON when
  `zujson` is present (D3). Users keep writing NONMEM, rxode2 or mrgsolve.
- **Not an estimator or a runner.** It never calls NONMEM, Monolix or
  Pumas. It reads their files and writes files for them.
- **Not a replacement for `babelmixr2`, `nonmem2rx` or Pharmpy.** They are
  oracles (§7) and consumers: a `zupkpmx` object converts to an rxode2 model
  function, which is `babelmixr2`'s input.
- **Not a translator of arbitrary code.** Verbatim Fortran, `$INFN`,
  user subroutines and C++ beyond mrgsolve's documented blocks are refused
  with a position, never guessed (§8).

## 2. Scope

In v0.1.0:

- **The representation** (§4.1): parameters, random effects, structural
  model, dosing attributes, observation models, covariates, estimates.
- **Readers** (§4.2): NONMEM control stream plus `.ext` or `.lst`
  estimates; rxode2 and nlmixr2 model functions; mrgsolve model
  specifications.
- **Writers** (§4.3): the same three formats, each returning a fidelity
  level and the list of what was lost (§6).
- **The equivalence harness** (§4.4): `pmx_design()`, `pmx_compare()`,
  a report object with `print()` and a Markdown writer.
- **A corpus** (§7): models in every format with their known-equivalent
  translations, used for tests and shipped as data.

Out of v0.1.0, in the order expected (§14):

- Monolix mlxtran reading and writing. `monolix2rx` covers reading today.
- Pumas, Phoenix PML, PK-Sim and Campsis writers.
- A Stan plus Torsten writer.
- PharmML import, for the DDMoRe model repository.
- Mixture models, `$PRIOR`, inter-occasion variability beyond
  representation (the object holds it; v0.1.0 writers refuse it).

## 3. Position in the `zu*` family

`zupkpmx` is the first package in the family aimed at one scientific
community rather than at a layer of computing. It stays pure R so that
the community can install it anywhere, and it follows the family rules:
no `Imports`, a conformance corpus, a differential oracle, one
behaviour per function.

- **`zuxml`** (CRAN 0.1.0): `Suggests`. Reads NONMEM 7's `run.xml`,
  which carries final estimates, standard errors and the covariance
  matrix in structured form, and is the same file mrgsolve's `$NMXML`
  reads. Preferred estimate source when the file exists (§4.2). Later,
  the PharmML importer (§14) is built on it.
- **`zucsv`** (CRAN 0.1.0): `Suggests`. Reads NONMEM datasets and
  `$TABLE` output, which are large and delimiter-sniffed; `sniff_csv()`
  handles the space-delimited tables with a title line. Base R is the
  fallback when it is not installed.
- **`daffiz`** (0.1.0, not yet on CRAN): `Suggests`. Its value-by-value
  table comparison with casting rules and a diff summary is the core of
  `pmx_compare()` (§4.4), and `expect_table_equal()` is how the corpus
  round-trip tests are written (§9). Q5 says what happens if it is not
  on CRAN in time.
- **`zujson`** (CRAN 0.1.0): `Suggests`. `pmx_write_json()` and
  `pmx_read_json()` serialise the representation when it is installed.
- **`zudiff`** (RFC 0045): not in v0.1.0. It is a sequence diff with
  exact equality, and RFC 0045 §1.1 leaves tables to `daffiz`, which is
  why the numeric comparison in `pmx_compare()` goes to `daffiz` and not
  here. Its place is §14: the statement-level `pmx_diff()` between two
  versions of one model, and the text diff between a model's source and
  its rewritten form in fidelity reports.
- **`zufuzz`**: development only. Fuzzes the three parsers (§9).
- **`zupeg`** (RFC 0039): the intended parser for v0.2.0. The v0.1.0
  readers are hand-written recursive descent so this package does not
  wait on that one (D7).
- **`zupkode`** (RFC 0047): a third engine for `pmx_compare()` that
  belongs to neither side of a translation, and a writer target. `zupkpmx`
  does not depend on it; its corpus becomes `zupkode`'s conformance suite.
- **`zutable`** (RFC 0036): not needed; the harness returns data frames.

## 4. Architecture

### 4.1 The representation

A `zupkpmx_model` is a list of seven components, each documented as a
data structure, none holding code:

1. **Parameters.** Fixed effects with name, initial value, bounds,
   `fixed`, and the final estimate when known. NONMEM `THETA(1)` and
   rxode2 `tcl` are both names for a parameter; the representation keeps
   the tool's name and a canonical role (`clearance`, `volume`, `ka`,
   `other`) inferred only where the structural model makes it certain.
2. **Random effects.** Omega as blocks with the parameter each eta
   attaches to and the transformation (`exp`, `additive`, `logit`,
   expression). Sigma as blocks. Inter-occasion variability as a flag
   plus occasion variable, represented but refused by v0.1.0 writers.
3. **Individual parameters.** Statements that compute individual values
   from parameters, etas and covariates, as expression trees (§4.5).
4. **Structural model.** Either a closed-form linear model with its
   canonical identity (one, two or three compartments; bolus, zero-order
   or first-order input; the NONMEM `ADVAN`/`TRANS` pair, rxode2's
   `linCmt()` parameterisation and mrgsolve's `PKMODEL` all map to it),
   or a system of ODEs over named compartments, as expression trees.
5. **Dosing attributes.** Per compartment: lag, bioavailability, rate or
   duration, initial amount, as expressions.
6. **Observation models.** Per endpoint: the prediction expression, the
   residual model (additive, proportional, combined, exponential, power,
   or an expression over epsilons), and transformation (`log`).
7. **Provenance.** Source format, file, the estimates file, the hash of
   each, and the fidelity of the read.

### 4.2 Readers

Each reader is a tokenizer and a recursive-descent parser in R for one
format, producing the representation or a `zupkpmx_parse_error` naming
file, line and column.

- **NONMEM.** `$PROBLEM`, `$INPUT`, `$DATA`, `$SUBROUTINES`, `$MODEL`,
  `$PK`, `$DES`, `$ERROR`, `$THETA`, `$OMEGA`, `$SIGMA`, `$ESTIMATION`,
  `$TABLE`. Abbreviated code: assignments, `IF`/`THEN`/`ELSE`/`ENDIF`,
  arithmetic, `EXP`, `LOG`, `SQRT`, `A(n)`, `DADT(n)`, `ALAGn`, `Fn`,
  `Rn`, `Dn`, `S n`, `ERR(n)`, `EPS(n)`, `ETA(n)`, `THETA(n)`. Estimates
  from `run.xml` through `zuxml` when the file exists, else from the
  `.ext` last iteration row, else from the `.lst` final estimates.
- **rxode2 and nlmixr2.** The `ini({})` and `model({})` blocks of a model
  function, taken as R expressions, so this reader is a walk over R
  syntax, not a tokenizer. Mu-referencing, `linCmt()`, `d/dt()`,
  `alag()`, `f()`, `rate()`, `dur()`, `cmt()`, `~` residual
  specifications with `add()`, `prop()`, `combined1()`, `combined2()`,
  `lnorm()`, `pow()`.
- **mrgsolve.** `$PARAM`, `$THETA`, `$OMEGA`, `$SIGMA`, `$CMT`, `$INIT`,
  `$MAIN`, `$PK`, `$ODE`, `$DES`, `$TABLE`, `$ERROR`, `$CAPTURE`,
  `$PKMODEL`, `$NMXML` and `$NMEXT` as estimate sources. C++ expressions
  with the documented macros (`F_CENT`, `ALAG_DEPOT`, `D_`, `R_`,
  `dxdt_`, `ETA(n)`, `EPS(n)`); anything else is refused.

### 4.3 Writers

A writer walks the representation and prints one format. Every writer
returns the text plus a `fidelity` (§6). Writers never invent parameter
values: a model read without estimates is written with its initial
values and says so.

### 4.4 The equivalence harness

`pmx_compare(a, b, design = pmx_design(), tol = 1e-6)`:

1. Resolves each model to an engine: rxode2 for rxode2 and NONMEM-read
   models (through the rxode2 writer), mrgsolve for mrgsolve models, or
   any engine the user names. A model cannot be compared through the
   engine it was translated from and to, unless the user asks: that is
   what makes the test independent of the writer being checked.
2. Builds the design: a typical subject (all etas zero) and `n` subjects
   with etas drawn from omega under a fixed seed, over a fixed regimen
   set: single bolus, single first-order dose, infusion, multiple doses
   with interval and additional doses, and steady state. Residual error
   is off; the harness compares predictions, not noise. Covariates take
   the values in the design table.
3. Simulates both, then hands the two prediction tables to `daffiz`,
   which aligns them on subject, time and endpoint and computes
   absolute and relative deviations under the design's tolerances.
4. Returns a `zupkpmx_comparison`: pass or fail, the worst deviation and
   where it occurred, per-regimen summaries, the engines and versions
   used, and both models' provenance. `pmx_report()` writes it as
   Markdown with the design and the two model texts appended.

The harness is the product. A regulator with NONMEM output tables and a
sponsor's nlmixr2 translation can run it on the tables alone through
`pmx_compare(tables, model)`, which skips the first model's engine and
reads the predictions from `$TABLE` output.

### 4.5 Expressions

One expression tree type for all three formats: constants, names,
binary and unary operators, calls from a fixed function table, and
`if`/`else`. Each format has a reader into this tree and a printer from
it. The function table maps names across formats (`EXP`, `exp`, `exp`)
and refuses names outside it. Fortran `IF` without `ELSE` becomes an
`if` with an implicit previous-value branch, which the rxode2 and
mrgsolve printers render as a sequential assignment, exactly as
`nonmem2rx` does.

## 5. Public R API (v0.1.0 shape)

```r
m  <- pmx_read_nonmem("run001.ctl")            # finds run001.ext
m  <- pmx_read_rxode2(one.cmt)                 # a model function
m  <- pmx_read_mrgsolve("pk2cmt.cpp")
m                                               # prints a summary
pmx_parameters(m); pmx_omega(m); pmx_ode(m)     # data frames and lists

w  <- pmx_write_rxode2(m)                      # function text
w  <- pmx_write_nonmem(m, estimates = "final")
w  <- pmx_write_mrgsolve(m)
pmx_fidelity(w)                                 # "exact", "equivalent", "lossy"
pmx_losses(w)                                   # character(), or what was lost

d  <- pmx_design(n = 20, seed = 1, regimens = c("bolus", "oral", "ss"))
cmp <- pmx_compare(m, w, design = d, tol = 1e-6)
cmp                                             # pass/fail, worst deviation
pmx_report(cmp, "run001-translation.md")
pmx_compare(pmx_read_tables("run001.tab"), w)   # against NONMEM output

pmx_write_json(m); pmx_read_json(path)          # with zujson installed
zupkpmx_corpus()                                  # the shipped model corpus
```

## 6. Semantics

**Fidelity** is one of four values, in order:

- `exact`: the written model reads back to an identical representation.
- `equivalent`: a reparameterisation that simulates identically (for
  example `ADVAN4 TRANS4` written as `linCmt()` with `cl`, `v2`, `q`,
  `v3`, `ka`).
- `lossy`: something was dropped, and `pmx_losses()` names every item
  (a `$TABLE` statement, an estimation option, a comment block).
- `refused`: the writer stopped at a construct it cannot express; the
  error names it.

Losses of estimation settings, tables and comments are `lossy`. Losses
that change predictions are never `lossy`; they are `refused`.

**Estimates** attach from `run.xml` when present, from `.ext`
otherwise, from `.lst` when asked, and are absent when none exists. A
model with estimates writes estimates; a model without writes initial
values and sets a loss.

**Comparison** passes when every relative deviation is below `tol` or
every absolute deviation is below `atol` for predictions near zero. The
seed is part of the design and of the report.

## 7. The oracle

Three oracles, all in `Suggests`, all run in CI when installed:

- **`nonmem2rx`**: every NONMEM model in the corpus that it reads must
  simulate identically to `zupkpmx`'s rxode2 writing of the same model.
- **`nonmem2mrgsolve`**: the same, for mrgsolve output.
- **Round trip**: every model read from format X, written to Y, read
  from Y and written back to X must compare as `pass` against the
  original through engines, and as `exact` or `equivalent` in
  representation.

The corpus starts from the `nlmixr2lib` model library, mrgsolve's model
library (`modlib()`), the NONMEM PREDPP examples re-typed from the Users
Guide, and the published models of the Genmab abstract where the
publications permit redistribution. Each corpus entry is one directory
with the model in every format that can hold it and a `expected.csv` of
typical-subject predictions on the standard design.

## 8. Errors

- `zupkpmx_parse_error`: file, line, column, the token seen and the tokens
  expected. Verbatim code and user subroutines raise it with the message
  "not translated: verbatim Fortran at line n".
- `zupkpmx_write_error`: the construct refused, its location in the
  representation, and the formats that could hold it.
- `zupkpmx_engine_error`: the engine missing or failing, with the engine's
  own message attached.
- `pmx_compare()` never raises on a failed comparison; failure is a value.

## 9. Testing

- One test file per reader, per writer and for the harness.
- Every corpus entry is a snapshot test of its representation and a
  round-trip test in every direction available, written with
  `daffiz::expect_table_equal()` on the prediction tables.
- The three parsers have `zufuzz` targets, run in the family's fuzz job.
- Oracle tests skip when the oracle is not installed and are required on
  the r-actions legs that install `Suggests`.
- A property test generates random closed-form models (compartments,
  input type, parameterisation) and checks round trips.
- The three CRAN point converters are run on the corpus in Stage 0 and
  their agreement recorded, so the package knows where it must agree and
  where the oracles disagree with each other.

## 10. Performance targets

Parsing is not the cost; engines are. The targets keep the package out
of the way.

| Operation | Target |
| --- | --- |
| read a 500-line control stream | under 50 ms |
| read the whole corpus | under 2 s |
| `pmx_compare()` overhead beyond the simulations | under 100 ms |
| standard design, 2-cpt, 20 subjects, rxode2 | under 5 s including compile |

## 11. Roadmap

Stages run the quick CI profile unless marked `full`. Each is one pull
request, merged on green.

| Stage | Delivers | Exit criterion |
| --- | --- | --- |
| 0 | repository, CI, corpus skeleton, oracles | §7 agreement recorded |
| 1 | representation, expressions, printing | §4.1 snapshot tests pass |
| 2 | rxode2 reader and writer | `nlmixr2lib` round trips `exact` |
| 3 | NONMEM reader with estimates | corpus parses; §8 refusals tested |
| 4 | NONMEM writer; mrgsolve read and write | `equivalent` round trips |
| 5 | harness, design, report | `nonmem2rx` oracle passes on the corpus |
| 6 | tables comparison, JSON, property test | §9 complete |
| 7 (`full`) | README, article, release preparation | §12 |

Stage 7's article is on translation validation: what `pmx_report()`
shows and how a reviewer reads it.

## 12. Acceptance criteria for v0.1.0

- Every behaviour in §6 and §8 has a passing test.
- Every corpus model round-trips through every pair of v0.1.0 formats
  that can hold it with `pass` from `pmx_compare()`.
- `zupkpmx` agrees with `nonmem2rx` on every corpus model that
  `nonmem2rx` reads; disagreements are documented, not hidden.
- `pmx_report()` output for one corpus model is reviewed by one
  pharmacometrician outside the family and the review is recorded.
- `R CMD check --as-cran` is clean on all r-actions legs; no `Imports`.
- Eight stages. A stage that needs a second pull request moves scope to
  §14, not the release.

## 13. Decisions

Marked **(M)** when the maintainer decided it and **(C)** when this RFC
decides it and the maintainer may overturn it.

- **D1 (C).** The deliverable is translation validation; converters
  exist to feed it. The README leads with `pmx_compare()`.
- **D2 (C).** v0.1.0 covers NONMEM, rxode2 and mrgsolve. These are the
  three formats a regulator and a sponsor are most likely to hold
  between them, and the three with free engines or free readers to test
  against.
- **D3 (C).** The representation is an R object documented as data,
  with optional JSON through `zujson`. It is not a file standard and the
  package never claims to be one. PharmML is the reason.
- **D4 (C).** Comparison runs each model on its own engine and never on
  the engine of the writer under test.
- **D5 (C).** Residual error is excluded from comparison. Predictions
  are deterministic given etas; noise would need distributional tests
  and hides translation errors.
- **D6 (C).** The NM-TRAN subset is the abbreviated-code subset that
  `nonmem2rx` and `nonmem2mrgsolve` translate today, no larger. Verbatim
  code is refused. Growing the subset is a later version.
- **D7 (C).** Readers are hand-written recursive descent in R. `zupeg`
  grammars replace them when `zupeg` exists; the representation does
  not change.
- **D8 (C).** Pure R, no `Imports`. Engines, oracles and the siblings
  `zuxml`, `zucsv`, `zujson` and `daffiz` are `Suggests`. Every reader
  degrades without them: estimates fall back to `.ext`, tables to base
  R, JSON is unavailable, and comparison without `daffiz` is Q5.
- **D9a (M).** Siblings already on CRAN are used wherever they fit
  rather than re-implemented: `zuxml` for `run.xml`, `zucsv` for tables.
- **D9 (C).** Writers never fabricate values and never silently drop a
  construct that changes predictions.
- **D10 (M).** The name is `zu` + `pk` + role. `pk` marks the
  pharmacometrics sub-family and the suffix the package's role, so
  `zupkpmx` and `zupkode` (RFC 0047) read as siblings and a later
  `zupknca` would too. Functions carry the role word only: `pmx_*`, with
  `zupkpmx_corpus()` for the package-level entry point.
- **D11 (M), 2026-10-28.** `daffiz` goes to CRAN before the harness
  stage (§11 stage 5), so it stays in `Suggests` and `pmx_compare()`
  carries no alignment-and-tolerance routine of its own. Resolves Q5.

## 14. Later versions, in the order they are expected

- **Monolix** mlxtran reader and writer, with `monolix2rx` as oracle.
- **A Stan plus Torsten writer**, which is the Bayesian item of the
  original analysis in the form it should take: the representation is
  already a model; Torsten's `pmx_solve_*` functions take NONMEM event
  records. Closed-form models map to `pmx_solve_twocpt()` and friends,
  ODE models to `pmx_solve_rk45()` or `pmx_solve_bdf()`.
- **`zupkode` as an engine** for comparison and as a writer target.
- **Pumas, Phoenix PML, PK-Sim, Campsis** writers, each with a reader
  only when a free engine or free output exists to test against.
- **PharmML import** for the DDMoRe repository.
- **`pmx_diff()`**: a structural diff of two representations, for
  review of model updates within one format, built on `zudiff` (RFC
  0045) for the statement sequences and expression text, with the
  representation supplying the alignment by component.
- **Text diffs in fidelity reports**: `pmx_write_*()` output against the
  source text through `zudiff`, so a reviewer sees what a round trip
  changed rather than only that it was `lossy`.
- **`zupeg` grammars** replacing the hand-written parsers.
- **Inter-occasion variability and mixtures** in writers.

## 15. Open questions

- **Q1.** Should `pmx_compare()` also compare individual predictions
  from estimation output (`IPRED` from `$TABLE` against rxode2's
  individual predictions with the same etas)? This RFC compares
  simulations only and reads tables for the typical subject.
- **Q2.** Is a corpus entry allowed to carry a published model whose
  paper is open access but whose licence is silent? This RFC says no:
  corpus models are from libraries with explicit licences or re-typed
  from documentation.
- **Q3.** Should the NONMEM reader accept `$SUBROUTINES ADVAN13` with
  `$DES` only, or also `ADVAN6`, `ADVAN8` and `ADVAN9`? This RFC accepts
  all general ODE `ADVAN`s and records the solver choice as a loss.
- **Q4.** Does the package submit to CRAN at 0.1.0, or wait until one
  external pharmacometrician has used `pmx_report()` on a real
  translation (§12)? This RFC requires the review before submission.
- **Q5.** `daffiz` is not on CRAN, and a CRAN package may not list an
  unpublished package in `Suggests`. Either `daffiz` is submitted before
  Stage 5, or `pmx_compare()` carries a minimal alignment-and-tolerance
  routine of its own and `daffiz` becomes the richer report when
  installed. This RFC prefers submitting `daffiz` first and makes it a
  Stage 0 check. **Resolved by D11:** `daffiz` goes to CRAN first.
