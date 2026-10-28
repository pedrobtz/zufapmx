# zupkpmx: roadmap

Status: draft 2026-10-28. Implements [design.md](design.md).
Target: v0.1.0, the first CRAN release.
Tracking: [v0.1.0 (#1)](https://github.com/pedrobtz/zupkpmx/issues/1), with
one sub-issue per stage;
[CRAN-submission (#14)](https://github.com/pedrobtz/zupkpmx/issues/14).

The design's §11 has eight stages. This roadmap brackets them with the
portfolio's identity, `Full CI` and readiness stages, and splits its Stage 4
(NONMEM writer plus mrgsolve reader and writer) into two pull requests. The
design's stage 0 is Stage 1 here, and its stage 7 is split between Stage 9
(README and article) and Stage 11 (release preparation).

## Stages

| Stage | Title | CI | Issue | Pull request | Done |
| --- | --- | --- | --- | --- | --- |
| 0 | [Package identity](#stage-0--package-identity) | quick | [#2](https://github.com/pedrobtz/zupkpmx/issues/2) | [#16](https://github.com/pedrobtz/zupkpmx/pull/16) | 2026-10-28 |
| 1 | [Corpus skeleton and oracles](#stage-1--corpus-skeleton-and-oracles) | full | [#3](https://github.com/pedrobtz/zupkpmx/issues/3) | [#18](https://github.com/pedrobtz/zupkpmx/pull/18) | 2026-10-28 |
| 2 | [Representation and expressions](#stage-2--representation-and-expressions) | quick | [#4](https://github.com/pedrobtz/zupkpmx/issues/4) | [#19](https://github.com/pedrobtz/zupkpmx/pull/19) | 2026-10-28 |
| 3 | [rxode2 reader and writer](#stage-3--rxode2-reader-and-writer) | quick | [#5](https://github.com/pedrobtz/zupkpmx/issues/5) | [#20](https://github.com/pedrobtz/zupkpmx/pull/20) | 2026-10-28 |
| 4 | [NONMEM reader](#stage-4--nonmem-reader) | quick | [#6](https://github.com/pedrobtz/zupkpmx/issues/6) | [#21](https://github.com/pedrobtz/zupkpmx/pull/21) | 2026-10-28 |
| 5 | [NONMEM writer](#stage-5--nonmem-writer) | quick | [#7](https://github.com/pedrobtz/zupkpmx/issues/7) | [#22](https://github.com/pedrobtz/zupkpmx/pull/22) | 2026-10-28 |
| 6 | [mrgsolve reader and writer](#stage-6--mrgsolve-reader-and-writer) | quick | [#8](https://github.com/pedrobtz/zupkpmx/issues/8) | | 2026-10-28 |
| 7 | [Equivalence harness](#stage-7--equivalence-harness) | full | [#9](https://github.com/pedrobtz/zupkpmx/issues/9) | | |
| 8 | [Tables, JSON and property tests](#stage-8--tables-json-and-property-tests) | quick | [#10](https://github.com/pedrobtz/zupkpmx/issues/10) | | |
| 9 | [README and article](#stage-9--readme-and-article) | quick | [#11](https://github.com/pedrobtz/zupkpmx/issues/11) | | |
| 10 | [Full CI](#stage-10--full-ci) | full | [#12](https://github.com/pedrobtz/zupkpmx/issues/12) | | |
| 11 | [v0.1.0 readiness](#stage-11--v010-readiness) | full | [#13](https://github.com/pedrobtz/zupkpmx/issues/13) | | |

## Stage 0 — Package identity

**Issue.** [#2](https://github.com/pedrobtz/zupkpmx/issues/2), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** The package says what it is, and the quick CI profile runs on
every pull request in under five minutes.

**Work.**

- `DESCRIPTION`: real Title and Description from design §1, `Authors@R`
  with `cre`, `aut` and `cph`, MIT licence, `Version: 0.0.0.9000`, no
  `Imports`.
- `.Rbuildignore` with every entry the "Working with skills" section of
  `CLAUDE.md` lists (done by `pkg-init`; verify).
- `R-CMD-check` caller: add `{"os": "windows-latest", "r": "devel"}` to
  `runners`.
- `coverage` caller: pin `coverage.yml` to a commit with the tag in a
  comment, as `CLAUDE.md` requires for a workflow with a write token.
- Package `CLAUDE.md` (done by `pkg-init`; committed with the roadmap).
- Replace the placeholder test with one that checks the package loads.

**Exit criteria.**

- `R CMD check` passes on the quick r-actions profile with 0 errors, 0
  warnings and at most the "New submission" note.
- Every quick-profile check on the pull request finishes in under five
  minutes; the timing is recorded in this roadmap.
- `DESCRIPTION` has no placeholder text and no `Imports`.

**Timing (2026-10-28, [#16](https://github.com/pedrobtz/zupkpmx/pull/16)).**
Quick profile, longest check first: pkgdown 2m45s, R CMD check clang23
2m38s, Ubuntu R-release 1m19s, coverage 1m11s. No tuning needed.

**Design.** §1, §3, D8.

## Stage 1 — Corpus skeleton and oracles

**Issue.** [#3](https://github.com/pedrobtz/zupkpmx/issues/3), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `full — first Suggests guards (rxode2, mrgsolve, nonmem2rx,
amp.sim); nosuggests must pass before later stages copy the skip
pattern.`

**Goal.** The corpus layout exists with its first models, the oracles run
on it, and where they agree and disagree is recorded.

**Work.**

- `inst/corpus/<model>/` layout from design §7: the model in every format
  that holds it, and `expected.csv` of typical-subject predictions.
- First entries: one-, two- and three-compartment closed-form models
  (bolus, first-order and zero-order input) and one ODE model, from
  `nlmixr2lib`, mrgsolve's `modlib()` and re-typed PREDPP examples, each
  with its licence recorded.
- `zupkpmx_corpus()` listing the entries.
- `Suggests`: `testthat`, `rxode2`, `nlmixr2`, `mrgsolve`, `nonmem2rx`,
  `nonmem2mrgsolve`. A test helper that skips when an oracle or engine is
  missing.
- A script under `tools/` that runs the CRAN point converters on the corpus
  and writes their agreement to `.agents/oracle-agreement.md` (design §9).
- Check where `daffiz` is on its way to CRAN (D11). It must be accepted
  before Stage 7 starts.

**Exit criteria.**

- `.agents/oracle-agreement.md` exists and covers every corpus entry.
- `zupkpmx_corpus()` returns every entry, tested.
- The full r-actions profile is green on the pull request, including
  `nosuggests`.
- The quick profile, with `Suggests` installed, still finishes in under
  five minutes, or this roadmap records what was tuned.

**Design.** §7, §9, Q2, D11, D12.

**Deviations (2026-10-28).**

- `nonmem2mrgsolve` is not on CRAN, so `amp.sim` replaces it in
  `Suggests` (design D12, the maintainer's decision). `nonmem2mrgsolve` is
  a development-only cross-check in `tools/oracle-agreement.R`.
- The corpus entries are written for this package from the PREDPP model
  structures, not copied from `nlmixr2lib` or `modlib()`. Both are GPL,
  and this package is MIT; shipping their files in `inst/` would change
  its licence. Tests that need them load them from the installed
  packages. Q2's answer (no corpus model without an explicit,
  compatible licence) is applied as written.
- `nlmixr2` is not in `Suggests` yet. Nothing in this stage uses it; the
  rxode2 reader stage adds it if its tests need it.
- `daffiz` is not on CRAN as of 2026-10-28 and has no `CRAN-submission`
  issue. Stage 7 waits on it (D11).

**CI tuning (2026-10-28, [#18](https://github.com/pedrobtz/zupkpmx/pull/18)).**
With the engines in `Suggests`, the R-devel containers took 15 to 17
minutes (`clang23` 17m, `ubuntu-clang` 16m, `ubuntu-gcc16` 15m), because
they compile rxode2 and mrgsolve from source; the binary runners took 3 to
3.5 minutes. `quick-containers` is now `[]`, so the quick profile is
Ubuntu R-release only, and the containers run in every full profile.
`nosuggests` and `nold` are on in the full profile.

## Stage 2 — Representation and expressions

**Issue.** [#4](https://github.com/pedrobtz/zupkpmx/issues/4), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** A `zupkpmx_model` can be built, validated, inspected and printed
without any reader.

**Work.**

- The seven components of design §4.1 as documented data structures, with
  a constructor and validator.
- The expression tree of §4.5: constants, names, operators, the fixed
  function table and `if`/`else`.
- `print()` for the model; `pmx_parameters()`, `pmx_omega()`, `pmx_ode()`.
- The condition classes of §8 (`zupkpmx_parse_error`,
  `zupkpmx_write_error`, `zupkpmx_engine_error`).

**Exit criteria.**

- Snapshot tests of the representation and its printing pass (§4.1).
- Invalid components raise classed errors, tested by class.
- `R CMD check` passes on the quick profile.

**Design.** §4.1, §4.5, §5, §8.

## Stage 3 — rxode2 reader and writer

**Issue.** [#5](https://github.com/pedrobtz/zupkpmx/issues/5), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** rxode2 and nlmixr2 model functions read into the representation
and write back out, with a fidelity and a list of losses.

**Work.**

- `pmx_read_rxode2()`: a walk over the `ini({})` and `model({})` blocks,
  covering the constructs in §4.2.
- `pmx_write_rxode2()`, returning text plus fidelity.
- `pmx_fidelity()` and `pmx_losses()` with the four levels of §6.
- An expression printer for rxode2 syntax.

**Exit criteria.**

- Every `nlmixr2lib` model in the corpus round-trips with fidelity `exact`.
- Each fidelity level and each refusal has a test.
- `R CMD check` passes on the quick profile.

**Design.** §4.2, §4.3, §6, D9.

**Deviations (2026-10-28).**

- The corpus has no `nlmixr2lib` models (Stage 1), so the first exit
  criterion is checked on the installed library instead: every
  `nlmixr2lib` model either reads and round-trips `exact`, or is refused
  with `zupkpmx_parse_error`. With nlmixr2lib 0.3.2, 44 of 59 read, all
  `exact`, including every `PK_*` model; 15 are refused for `logit()`,
  `sin()`/`cos()` of time, a two-value omega and a `log(c(...))` bound.
- §6's "a model without estimates writes initial values and sets a loss"
  is read per writer: rxode2's `ini()` holds one value, so writing a model
  without estimates loses nothing, and writing one with estimates loses
  the initial values. Stage 5 applies the same rule to the NONMEM writer:
  a loss only when the model has estimates, of whichever of estimates and
  initial values is not written.
- Reading also records top-level metadata (`reference`, `units`), labels
  and constant `ini()` expressions such as `log(90)` as losses.

## Stage 4 — NONMEM reader

**Issue.** [#6](https://github.com/pedrobtz/zupkpmx/issues/6), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** NONMEM control streams read into the representation, with
estimates attached from the best available source.

**Work.**

- Tokenizer and recursive-descent parser for the records and the
  abbreviated-code subset of §4.2 (D6, D7).
- Estimates from `run.xml` through `zuxml` when present, else `.ext`, else
  `.lst` when asked. `zuxml` joins `Suggests`.
- `zupkpmx_parse_error` with file, line, column, token seen and expected;
  verbatim Fortran, `$INFN` and user subroutines refused.
- Settle Q3 (which general ODE `ADVAN`s are accepted) before parsing
  `$SUBROUTINES`.

**Exit criteria.**

- Every NONMEM model in the corpus parses.
- Every refusal in §8 has a test asserting its class.
- Each estimate source, and the fallback without `zuxml`, has a test.
- `R CMD check` passes on the quick profile.

**Design.** §4.2, §6 (estimates), §8, D6, D7, D13 (Q3 resolved: every
general ODE `ADVAN`, solver recorded as a loss).

## Stage 5 — NONMEM writer

**Issue.** [#7](https://github.com/pedrobtz/zupkpmx/issues/7), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** Any representation the v0.1.0 formats can hold writes out as a
NONMEM control stream.

**Work.**

- `pmx_write_nonmem(m, estimates = )`, with closed-form models written as
  their `ADVAN`/`TRANS` pair and ODE models as `$DES`.
- An expression printer for NM-TRAN, including the sequential-assignment
  rendering of `IF` without `ELSE`.

**Exit criteria.**

- Every corpus model round-trips rxode2 → NONMEM → rxode2 and NONMEM →
  rxode2 → NONMEM with fidelity `exact` or `equivalent`.
- A model without estimates writes initial values and records the loss.
- `R CMD check` passes on the quick profile.

**Design.** §4.3, §6, D9.

**Notes (2026-10-28).**

- Residual error is parameterised differently in the two formats: NONMEM
  as sigma variances over `EPS`, rxode2 as standard-deviation parameters.
  Each writer converts (`σ²` to a parameter `σ`, and back), which
  simulates identically, so cross-format writes are `equivalent`.
- First commit: fidelity compares constants within a few ulps. The full
  run on `main` for Stage 3 failed on macOS and `nold`, where R's decimal
  parser is not correctly rounded ([#12](https://github.com/pedrobtz/zupkpmx/issues/12)).

## Stage 6 — mrgsolve reader and writer

**Issue.** [#8](https://github.com/pedrobtz/zupkpmx/issues/8), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** mrgsolve model specifications read and write, completing the
three v0.1.0 formats.

**Work.**

- `pmx_read_mrgsolve()` for the blocks and documented macros of §4.2, with
  `$NMXML` and `$NMEXT` as estimate sources. Other C++ is refused.
- `pmx_write_mrgsolve()` and its expression printer.

**Exit criteria.**

- Every corpus model round-trips through every pair of the three formats
  that can hold it with fidelity `exact` or `equivalent`.
- Undocumented C++ raises `zupkpmx_parse_error`, tested.
- `R CMD check` passes on the quick profile.

**Design.** §4.2, §4.3, §6.

**Notes (2026-10-28).**

- mrgsolve's `$PARAM` has no field for bounds or fixed flags. They are
  written in `@annotated` descriptions (`[0, Inf] fixed`), a convention
  the reader reads back; otherwise every write of a bounded model would
  be `lossy`. Fixed flags of `$OMEGA`/`$SIGMA` are still a loss.
- `$NMXML`/`$NMEXT` read the run's thetas as parameters `THETA1`, ...
  with the estimates as both initial and final values, and its omega and
  sigma as blocks.

## Stage 7 — Equivalence harness

**Issue.** [#9](https://github.com/pedrobtz/zupkpmx/issues/9), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `full — the engines become a runtime Suggests path for
pmx_compare(); nosuggests and every platform leg must hold before tables
and reports build on it.`

**Goal.** `pmx_compare()` proves two models equal, or shows where they
differ, through independent engines.

**Work.**

- `pmx_design()`: typical subject, `n` subjects with seeded etas, the
  regimen set of §4.4, residual error off (D5).
- `pmx_compare()`: engine resolution that never uses the writer under test
  (D4), simulation, alignment and tolerances through `daffiz` (D11), and
  a `zupkpmx_comparison` value. A failed comparison is a
  value, never an error.
- `pmx_report()` writing Markdown with the design and both model texts.
- Corpus round-trip tests with `pmx_compare()`.

**Exit criteria.**

- The `nonmem2rx` oracle passes on the corpus; any disagreement is
  documented in `.agents/oracle-agreement.md`.
- Every corpus model round-trips through every pair of formats that can
  hold it with `pass`.
- A missing engine raises `zupkpmx_engine_error`, tested.
- The full r-actions profile is green on the pull request.

**Design.** §4.4, §6 (comparison), §7, D4, D5, D11.

## Stage 8 — Tables, JSON and property tests

**Issue.** [#10](https://github.com/pedrobtz/zupkpmx/issues/10), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** A reviewer can compare NONMEM output tables against a
translation, the representation serialises to JSON, and the test plan of
§9 is complete.

**Work.**

- `pmx_read_tables()` through `zucsv`, with a base R fallback, and
  `pmx_compare(tables, model)`.
- `pmx_write_json()` and `pmx_read_json()` through `zujson`.
- A property test generating random closed-form models and checking round
  trips.
- `zufuzz` targets for the three parsers.
- Settle Q1 (individual predictions from `$TABLE`).

**Exit criteria.**

- Every item of design §9 has a test or a fuzz target in the repository.
- JSON round trips are `exact` on the corpus.
- `R CMD check` passes on the quick profile.

**Design.** §4.4 (tables), §5, §9, Q1.

## Stage 9 — README and article

**Issue.** [#11](https://github.com/pedrobtz/zupkpmx/issues/11), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `quick`.

**Goal.** A pharmacometrician landing on the README sees `pmx_compare()`
first and can follow a translation check end to end.

**Work.**

- `README.Rmd` leading with `pmx_compare()` (D1), written with the
  `pkg-readme` skill.
- An article on translation validation: what `pmx_report()` shows and how
  a reviewer reads it.
- pkgdown reference index grouping readers, writers and the harness.
- Check the performance targets of §10 and record the measurements.

**Exit criteria.**

- The pkgdown site builds in CI with the article.
- README examples run.
- §10 measurements are recorded in this roadmap.

**Design.** §10, §11 (stage 7), D1.

## Stage 10 — Full CI

**Issue.** [#12](https://github.com/pedrobtz/zupkpmx/issues/12), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `full`. Always.

**Goal.** No new features. The full r-actions profile passes on the
default branch's code.

**Work.**

- Run the full profile (`full-ci` label) and fix everything it finds:
  gctorture, valgrind, sanitizers, every platform leg including Windows
  R-devel, `nosuggests`, `nold`, cran-special.
- Fix the failures recorded as comments on this stage's issue.

**Exit criteria.**

- A full-profile run is green on the pull request and on the merge commit.

**Design.** §12 (`R CMD check --as-cran` clean on all legs).

## Stage 11 — v0.1.0 readiness

**Issue.** [#13](https://github.com/pedrobtz/zupkpmx/issues/13), sub-issue of
[v0.1.0](https://github.com/pedrobtz/zupkpmx/issues/1).

**CI.** `full`. Always.

**Goal.** Only the submission itself is left.

**Work.**

- `Version: 0.1.0` and the matching `NEWS.md` heading.
- `cran-comments.md` listing the r-actions CI legs as test environments.
- Every `Suggests` entry, `daffiz` included, is on CRAN (D11).
- `pmx_report()` output for one corpus model reviewed by one
  pharmacometrician outside the family, and the review recorded (§12, Q4).
- `pkg-critical-review` run; blocking and required findings fixed.
- `pkg-cran-review` run; every finding resolved.
- `urlchecker::url_check()` and `spelling::spell_check_package()` clean.

**Exit criteria.**

- Every acceptance criterion of design §12 holds.
- A full-profile r-actions run is green on the release commit.

**Design.** §12, Q4.

## After 0.1.0

From design §2 and §14, in the order expected:

- Monolix mlxtran reader and writer, with `monolix2rx` as oracle.
- A Stan plus Torsten writer.
- `zupkode` as a comparison engine and writer target.
- Pumas, Phoenix PML, PK-Sim and Campsis writers.
- PharmML import for the DDMoRe repository, built on `zuxml`.
- `pmx_diff()`: a structural diff of two representations, built on
  `zudiff` (RFC 0045).
- Text diffs in fidelity reports: writer output against the source text
  through `zudiff`.
- `zupeg` grammars replacing the hand-written parsers (D7).
- Inter-occasion variability and mixtures in writers; `$PRIOR`.

Each waits because v0.1.0 is the three formats with free engines to test
against, and the harness that makes them checkable.

## Open questions

Q5 is settled by D11 (2026-10-28): `daffiz` goes to CRAN before Stage 7.
That makes `daffiz` reaching CRAN a dependency of Stage 7.

- **Q2, blocks Stage 1.** May the corpus carry open-access published models
  whose licence is silent? The design says no.
- **Q1, blocks Stage 8.** Compare individual predictions from estimation
  output as well as simulations?
- **Q4, blocks Stage 11.** Is the external pharmacometrician's review
  required before submission? The design says yes.
- The design's decisions marked (C), D3 and D6 above all, are still the
  maintainer's to confirm.
