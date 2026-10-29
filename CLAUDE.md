# CLAUDE.md

<!-- markdownlint-disable-next-line MD013 -->
This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`zufapmx` holds a population PK/PD model as one R object, reads it from
NONMEM, rxode2/nlmixr2 and mrgsolve, writes it back to any of the three, and
proves two models are the same model. `pmx_compare()` simulates both on a
canonical design, each through its own engine, and reports the worst
deviation against a tolerance. That proof is the product; the converters
exist to feed it. Pure R with no `Imports`: engines, oracles and the siblings
`zuxml`, `zucsv` and `zujson` are all `Suggests`. It is not a
modelling language, an estimator or a runner, and it does not translate
verbatim Fortran, user subroutines or C++ outside mrgsolve's documented
blocks.

The specification is [`.agents/design.md`](.agents/design.md), adopted from
[RFC 0046](https://github.com/pedrobtz/packages/blob/main/rfcs/started/0046-zufapmx-pharmacometric-model-interchange.md).
It outranks this file: `CLAUDE.md` orients, the design doc decides. Its
decisions marked (C) and its open questions (§15) are still the maintainer's
to confirm.

## Current state

Scaffolded 2026-10-28 with `create-pkg.sh`; no functionality yet. The
roadmap in `.agents/roadmap.md` takes it to v0.1.0.

## Stage tracking

Progress toward the next version is tracked as GitHub sub-issues, so the
parent issue shows a progress bar such as "6 of 7".

- **One parent issue per target version**, titled with the bare version, for
  example `v0.1.0`. This package's is
  [#1](https://github.com/pedrobtz/zufapmx/issues/1).
- **One sub-issue per roadmap stage**, titled as the roadmap titles it, for
  example `Stage 3 — R interface`, and linking to that section. The number of
  stages is whatever the roadmap has; there is no fixed scale.
- **Every tracking issue carries the `stage` label**, so it can be told apart
  from bug and feature issues and left out of open-issue counts.
- **Close a stage by merging its pull request.** Put `Closes #<n>` in the
  body of the stage's pull request. Never close a stage whose exit criteria
  are not met; record a deviation in the roadmap first.
- **The roadmap stays authoritative.** The issues record which stages are
  done, not what a stage requires. Adding, removing or renaming a stage
  means editing the roadmap and the sub-issues in the same change.
- **Each stage has a CI tier, `quick` or `full`.** The roadmap sets it, and
  `quick` is the default. See Continuous integration for what each means.
  The second-to-last stage, `Full CI`, runs the full set and fixes what it
  finds before `v0.1.0 readiness`.
- Close the parent issue when CRAN accepts the version and it is tagged.
  Opening a parent issue implies at least one stage sub-issue; a parent with
  no sub-issues shows no progress on the board.

## Versioning

A new package develops at `0.0.0.9000` and targets `0.1.0` for its first CRAN
release. Set `Version: 0.1.0` and the matching `NEWS.md` heading when the
package enters `Pre-flight`. Bump to `0.1.0.9000` only after CRAN accepts it,
never before: a `.9000` version above an unreleased `0.1.0` heading is wrong.

Until the first release the R API may change freely. Sibling packages can
still depend on this one before CRAN does, through `Remotes:` or `LinkingTo:`.
None does yet; `zufaode` (RFC 0047) plans to use this package's corpus as its
conformance suite, so a change to the corpus layout affects it. A package
that others wait on should reach CRAN first.

## Commands

Run from the package root.

```sh
Rscript -e 'devtools::load_all()'                # compile src/ and load
Rscript -e 'devtools::document()'                # roxygen -> NAMESPACE, man/
Rscript -e 'devtools::test()'                    # full testthat suite
Rscript -e 'devtools::test(filter = "<name>")'   # one test file
Rscript -e 'devtools::test(shuffle = TRUE)'      # order independence
Rscript -e 'devtools::check()'                   # R CMD check
air format .                                     # format R sources
Rscript tools/corpus-expected.R                  # rewrite expected.csv
Rscript tools/oracle-agreement.R                 # rewrite oracle table
```

The two `tools/` scripts need rxode2, mrgsolve and the oracles installed.
Run them only when the corpus changes, and review the diff before
committing it.

## Architecture

Pure R: there is no `src/` and no native layer. The rest grows as stages
land; see `.agents/design.md` (§4).

- **Expressions** ([R/expr.R](R/expr.R)) are R language objects:
  numbers, names and calls, restricted by `expr_check()` to the operators
  and function table of design §4.5. Statements are `name <- expr` or
  `if`/`else` (`stmt_check()`). Every reader builds these and every writer
  prints them; a construct outside the table is refused, not guessed.
- **The model** ([R/model.R](R/model.R)) is a classed list of the seven
  components of §4.1. `new_model()` is the only constructor and validates
  every component; it is internal, because readers are the public way to
  get a model (§5). The test helper `helper-model.R` builds models
  directly.
- **NONMEM** ([R/nmtran.R](R/nmtran.R), [R/read-nonmem.R](R/read-nonmem.R),
  [R/nonmem-estimates.R](R/nonmem-estimates.R)): records, a tokenizer and
  a recursive-descent parser for abbreviated code, then the same
  representation. NONMEM names stay as symbols (`THETA1`, `ETA1`, `EPS1`);
  the endpoint is `Y`, and `$ERROR` is reduced to a residual model of `Y`
  over `F`.
- **Readers and writers** ([R/read-rxode2.R](R/read-rxode2.R),
  [R/write-rxode2.R](R/write-rxode2.R)): a reader raises
  `zufapmx_parse_error` for anything it does not support, never a model
  error; a writer reads its own output back and sets the fidelity
  ([R/written.R](R/written.R)) from whether the representation came back
  identical.
- **mrgsolve** ([R/mrgsolve-cpp.R](R/mrgsolve-cpp.R),
  [R/read-mrgsolve.R](R/read-mrgsolve.R),
  [R/write-mrgsolve.R](R/write-mrgsolve.R)): blocks and a C++ subset
  parser; `$PKMODEL` uses NONMEM's names, so closed forms share
  `closed_forms` ([R/closed.R](R/closed.R)). Expressions print through
  dialects ([R/print-expr.R](R/print-expr.R)): NM-TRAN and C++.
- **Names.** Format code is prefixed `nm_`, `mrg_` or `cpp_`, and
  `rxode2_read_`/`rxode2_write_`; code more than one format uses has a
  neutral name in a shared file (`closed_forms`, `sigma_from_sd()`,
  `model_rename()`). `check_*()` validates an argument, `validate_*()` a
  model component, `expr_check()`/`stmt_check()` an expression, and the
  condition constructors are `*_error()`. Each format keeps one table per
  spelling (`nm_dosing_prefixes`), read inverted by `invert_names()`.
- **Writers** ([R/write-rxode2.R](R/write-rxode2.R),
  [R/write-nonmem.R](R/write-nonmem.R)) convert residual error between
  NONMEM's sigma variances and rxode2's standard-deviation parameters;
  that conversion is why cross-format writes are `equivalent`, not
  `exact`.
- **Conditions** ([R/errors.R](R/errors.R)): `zufapmx_abort()` and the §8
  constructors `parse_error()`, `write_error()` and `engine_error()`,
  each carrying the fields a caller acts on.
- **The corpus** ([R/corpus.R](R/corpus.R), `inst/corpus/`): see Testing
  conventions.

## Invariants that are easy to break

These come from the design's decisions (§13) and hold from the first stage.

- **No `Imports` (D8).** Every reader must still work without its
  `Suggests`: estimates fall back from `run.xml` to `.ext`, tables to base R.
  Adding an `Imports` entry breaks a family rule and a v0.1.0 acceptance
  criterion.
- **Every `Suggests` entry must be on CRAN at submission.** A CRAN
  package may not suggest an unpublished one. This is why `pmx_compare()`
  carries its own alignment and tolerance check instead of relying on
  `daffiz` (D17).
- **Writers never fabricate values and never silently drop a construct that
  changes predictions (D9).** Such a loss is `refused`, never `lossy`. A
  model without estimates is written with initial values and records the
  loss.
- **A comparison never runs on the engine of the writer under test (D4),**
  unless the user asks. Otherwise the check is not independent of what it
  checks.
- **Residual error is off in comparisons (D5).** Predictions are compared,
  not noise.
- **`pmx_compare()` never raises on a failed comparison.** Failure is a
  value; only missing or failing engines raise `zufapmx_engine_error`.
- **Unsupported code is refused with a position, never guessed (D6).** The
  NM-TRAN subset is what `nonmem2rx` and `nonmem2mrgsolve` translate today.
- **Siblings on CRAN are used, not re-implemented (D9a):** `zuxml` for
  `run.xml`, `zucsv` for tables.
- **The corpus ships only MIT-compatible models.** Entries are written for
  this package; `nlmixr2lib` and `modlib()` are GPL and are loaded from
  their installed copies at test time, never copied into `inst/`.
- **Each mrgsolve compile needs its own model name.** mrgsolve names the
  shared library after the model, and a second model of the same name in
  one session silently runs the first one's code.
- **Readers strip source references.** A model typed with
  `keep.source = TRUE` (tests, interactive use) otherwise differs from the
  same model parsed from text, and every round trip reads as
  `equivalent` instead of `exact`.
- **`linCmt()` has two naming styles.** rxode2's (`cl`, `v`, `q`, `vp`) and
  NONMEM's numbered `TRANS4` names, where `V2` is the central volume when
  there is a depot. `rxode2_lincmt_aliases()` tells them apart; reading `V2` as a
  peripheral volume refused every NONMEM-read closed form written back.
- **Read `run.xml` with an encoding override.** NONMEM declares
  `encoding="ASCII"`, which zuxml (expat) rejects
  ([pedrobtz/zuxml#84](https://github.com/pedrobtz/zuxml/issues/84)).
- **Fidelity compares numbers within a few ulps** (`same_content()`).
  Without long double (macOS arm64, `nold`) R's decimal parser is not
  correctly rounded, and `identical()` turned exact round trips into
  `equivalent` on those legs only.
- **Closed forms need names on both sides.** The NONMEM writer aliases an
  `ADVAN`'s required names (`V2 = V`) and renames model variables that
  hold one for another purpose; the rxode2 writer renames variables
  `linCmt()` would take for parameters, with an `x_` prefix, because
  rxode2 matches those names by prefix.
- **C++ literals carry a decimal point.** `1/2` is integer division in
  C++; the mrgsolve dialect writes `1.0 / 2.0`.
- **`$PARAM @annotated` descriptions hold bounds.** `[lower, upper]` and
  `fixed` are zufapmx's convention; without it every mrgsolve write of a
  bounded model would be `lossy`.
- **Who assigns and reads what (D14, D18).** The validator refuses
  assignments to parameters, random effects, compartments and time; ODE
  code reassigning an individual name; individual statements reading an
  amount; and a name an `if` assigns without a value before it. A reader
  that meets one of these refuses it or, for rxode2, moves statements into
  the ODE code; writers rely on all four.
- **Each record reads its own time (D15).** `TIME` in `$DES` or `$ODE` and
  `SOLVERTIME` outside `$ODE` are refused; `T` outside `$DES` is a NONMEM
  variable.
- **A statement's refusal points at its record** through `record_at()`:
  statements carry no positions of their own.
- **Exported functions are `pmx_*` (D10),** with `zufapmx_corpus()` the one
  package-level exception.

## Testing conventions

- **Self-sufficient.** Every test builds its own inputs inside the
  `test_that()` block. No file-scope objects.
- **Self-contained.** Global state goes through `withr::local_*()`, randomised
  input through `withr::local_seed()`. Tests write only under `tempdir()`.
- **Assert on condition classes, never message text.** Use
  `expect_error(..., class = "<pkg>_<kind>_error")`. Wording belongs in
  snapshot tests.
- **Order independence.** `devtools::test(shuffle = TRUE)` is part of the
  definition of done. testthat writes snapshot files in run order, so a
  shuffled run reorders `tests/testthat/_snaps/` without changing them;
  discard that with `git checkout tests/testthat/_snaps` before committing.
- **Borrow a conformance suite before writing one.** Mainstream
  implementations in Python, Java, Go or Node usually ship a test corpus.
  Use it as an oracle rather than inventing cases by hand. Here the
  corpus is `inst/corpus/`, one directory per model in every format that
  holds it, with `expected.csv` of typical-subject predictions. The
  oracles are `nonmem2rx` (every entry) and `amp.sim` (general ODE entries
  only); [`.agents/oracle-agreement.md`](.agents/oracle-agreement.md)
  records where each agrees and why it fails elsewhere.
- **Helpers live in `tests/testthat/helper-*.R`.** `helper-corpus.R`
  simulates the typical subject of a corpus entry through rxode2, mrgsolve
  or an oracle at fixed tight tolerances, and `corpus_max_rel_dev()`
  measures agreement. The `tools/` scripts source it, so the corpus files,
  the scripts and the tests share one implementation. `expect_refused()`
  asserts a refusal's class, `seen` and line; `expect_faithful()` a write
  that is exact or equivalent; `str_full()` snapshots a representation
  untruncated.
- **Skip engines with `skip_if_no_engine()`,** not
  `skip_if_not_installed()`. CI sets `ZUFAPMX_REQUIRE_ENGINES=true`, so a
  runner leg where an engine failed to install fails instead of skipping.
- **One test file per module**, named after the R file it tests.
- **Keep the suite inside the CRAN time budget.** No runtime recorded yet.

## Definition of done

`devtools::document()` and `devtools::check()` clean, meaning 0 errors, 0
warnings and 0 notes. The one allowed note is "New submission" before the first
release. `devtools::test(shuffle = TRUE)` green. CI green at the change's
tier: the quick profile for a `quick` roadmap stage or an ordinary pull
request, the full profile for a `full` stage, `Full CI`, and every release.
A user-facing change also needs a test, roxygen documentation and a `NEWS.md`
entry.

## Releasing to CRAN

- **CI is the pre-submission check.** The `pedrobtz/r-actions` R CMD check runs
  `--as-cran` on the CRAN-like runners and containers, and it replaces
  win-builder, the macOS builder and R-hub. None of them is a release step,
  and `cran-comments.md` lists the CI legs as its test environments.
- **Entering `Pre-flight`.** The roadmap's feature stages are done, the
  version is set as in Versioning, `NEWS.md` and `cran-comments.md` are
  written, and CI is green. Only submission work remains.
- **Before submitting,** run `pkg-critical-review` once the roadmap is
  implemented and fix what it files, then `pkg-cran-review`, and resolve
  every finding. Both skills are in `pedrobtz/r-skills`.
- **The pretest is automated and does not read `cran-comments.md`.** A NOTE
  that is explained but not fixed gets the upload archived. Fix the NOTE, as
  under Vendored native code, instead of justifying it.
- **After acceptance,** `pkg-post-cran` tags the submitted commit with the
  bare version, `0.1.0` (no `v`), publishes the GitHub release, bumps to
  `0.1.0.9000`, closes the version's parent issue and moves the board card.

## Editing rules

- roxygen comments are the source. Never edit `man/` or `NAMESPACE` by hand.
- Edit `README.Rmd`, not the generated `README.md`.
- Keep prose simple and short. One idea per sentence.
- Wrap roxygen text at 80 characters and run `air format .` on R sources.
- Use `lower_snake_case`, and prefix internal native symbols with `<pkg>_`.
- Keep hard runtime dependencies at zero unless a recorded design decision
  says otherwise.
- Export and document public functions. Do not create roxygen topics for
  internal ones.

## Continuous integration

Workflows come from `pedrobtz/r-actions`. Call those reusable workflows instead
of writing bespoke ones, and pin a released tag rather than `main`. Pin a
workflow that holds a write token, such as `coverage.yml`, to a commit, with
the tag in a comment. The `R-CMD-check` workflow must call `r-cmd-check.yml`,
because a hand-rolled one does not count as the pre-submission check. This
package calls `r-cmd-check.yml@v1` and `coverage.yml@v1`; `pkgdown.yaml`
uses the `r-lib/actions` steps directly.

Small edits get a small run. The shared workflows look at the files a pull
request changes, compared with its merge base:

- **Markdown only**, such as `README.md`, `NEWS.md` or `cran-comments.md`,
  skips every check. Only coverage and the pkgdown site run.
- **`DESCRIPTION` or `README.Rmd` as well**, and nothing else, adds one R CMD
  check on Ubuntu R-release.
- **Any other file** runs everything.

So put a README or `DESCRIPTION` touch-up in its own pull request. Once a pull
request changes code, every later push to it runs every workflow.

### Quick by default, full by exception

The full profile is slow. gctorture at step 20 alone has taken 36 to 107
minutes on sibling packages. Waiting on it between roadmap stages was the
main thing slowing roadmaps down, so:

- **Pull requests run the quick profile.** It should finish in under five
  minutes. If it doesn't, raise gctorture's `quick-step` or trim
  `quick-containers` in the caller workflows; never cut the full profile.
- **A `full` roadmap stage takes the `full-ci` label** and merges only on a
  green full run. The roadmap gives the reason in the stage's `**CI.**`
  line, for example first compiled code, a `PROTECT` or finalizer change, a
  vendored library, or a Windows-only path.
- **Don't wait on the full run a merge starts** on the default branch, and
  cancel older full runs it supersedes. A failure that only the full set
  shows goes on the `Full CI` stage issue as a comment with the run link,
  and is fixed there. A failure the quick profile also shows is fixed now.
- **`Full CI` and `v0.1.0 readiness` wait** for a green full run on their
  merge commit, and the release commit always needs one.
- Locally, run `gctorture(TRUE)` on the code a C change touches when it is
  cheap; the full run is the authority.

## Commits and pull requests

Short, imperative, sentence-case commit subjects, optionally scoped. Keep each
commit focused and do not sweep in unrelated files. A pull request explains the
user-visible outcome and the rationale, links related issues, lists the checks
that were run and the tests that were skipped, and flags platform-sensitive or
vendored changes. Performance claims need evidence.

Never commit or push to the default branch. Work on a branch, open a pull
request, and leave it for review. Do not merge a pull request unless you are
told to.

When you find a defect, in this package or in an upstream tool, open an issue
for it rather than only working around it.

## Working with skills

The portfolio's workflow skills live in `pedrobtz/r-skills` (`pkg-init`,
`pkg-roadmap`, `pkg-implement`, `pkg-critical-review`, `pkg-cran-review`,
`pkg-post-cran`, `pkg-readme`); the general process skills are
[Superpowers](https://github.com/obra/superpowers). `building-a-package.md`
in `pedrobtz/packages` walks through them in order. Both sets defer to this
file where they overlap, so the settlements are recorded here.

- **One design.** The specification is `.agents/design.md`, adopted from the
  package's RFC in `pedrobtz/packages/rfcs`. A package with an adopted design
  is past `brainstorming`; a design change goes through the RFC, not a new
  spec. Superpowers' default `docs/superpowers/specs/` is not used.
- **Plans at two levels.** `.agents/roadmap.md` holds the stages, one
  mergeable pull request each, with exit criteria and a CI tier
  (`pkg-roadmap`). Inside a stage, `writing-plans` breaks the work into
  tasks of one test cycle each, saved as `.agents/plans/stage-<n>.md`. The
  roadmap stays authoritative; a plan is scratch and is not kept in sync.
- **Build and merge.** `pkg-implement` runs the stage loop: branch
  `stage-<n>-<slug>` from the stage issue, implement the plan with
  `test-driven-development` (`Rscript -e 'devtools::test(filter = "<name>")'`
  is the red/green command), open the pull request, merge on green at the
  stage's CI tier. The confirmation the user gives at the start of that
  loop is the "told to merge" that Commits and pull requests requires, so
  the `finishing-a-development-branch` menu is not presented for stage
  pull requests. Any other pull request still waits for review.
- **Failures and claims.** A failing check or test goes through
  `systematic-debugging` before a fix. "Green", "done" and "ready for CRAN"
  are claimed only after `verification-before-completion`: run the command,
  read the output.
- **Review.** `requesting-code-review` gates tasks inside a stage.
  `pkg-critical-review` reviews the whole package once the roadmap is
  implemented, with Posit's reviewers, and files issues. CRAN's reviewer
  email and GitHub review threads go through `receiving-code-review`.
- **Keep scratch out of the tarball.** `R CMD build` reads `.Rbuildignore`,
  not `.gitignore`. It must list `^\.agents$`, `^\.superpowers$` (the
  Superpowers task ledger), `^\.worktrees$` (if git worktrees are kept in
  the package directory; a sibling directory avoids the question),
  `^\.claude$`, `^CLAUDE\.md$`, `^AGENTS\.md$`, `^CRAN-SUBMISSION$` and
  `^cran-comments\.md$`. A stray top-level file is a NOTE, and the pretest
  archives on NOTEs.
