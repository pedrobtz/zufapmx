# Check that two models are the same model

Simulates both models on one design, each through its own engine, and
compares their predictions. This is the check a translation exists to
pass: a model read from one format and written to another should predict
the same values for the same subjects.

## Usage

``` r
pmx_compare(
  a,
  b,
  design = pmx_design(),
  tol = 1e-06,
  atol = 1e-09,
  engines = NULL
)
```

## Arguments

- a, b:

  The models to compare: each a model read by a `pmx_read_*()` function,
  or a model written by a `pmx_write_*()` function.

- design:

  A design from
  [`pmx_design()`](https://pedrobtz.github.io/zufapmx/reference/pmx_design.md).

- tol:

  The relative tolerance, a positive number.

- atol:

  The absolute tolerance, a non-negative number.

- engines:

  `NULL` to choose engines as described above, or a character vector of
  two engines, `"rxode2"` or `"mrgsolve"`, for `a` and `b`. `NA` in
  either place chooses that side's engine as usual.

## Value

A `zufapmx_comparison`, a list with:

- pass:

  `TRUE` when every prediction agrees.

- reason:

  Why the models could not be compared point by point, or `NULL`.

- worst:

  A one-row data frame of the worst deviation, or `NULL` without
  predictions: the first missing or non-finite prediction, or else the
  largest relative deviation among points outside `atol`, or else the
  largest absolute deviation.

- regimens:

  One row per regimen: the number of points, the number that disagree,
  the largest absolute and relative deviations, and the reason a regimen
  was skipped.

- predictions:

  One row per point: `regimen`, `id`, `time`, the two predictions `a`
  and `b`, the deviations `abs` and `rel`, and whether it agrees
  (`pass`).

- engines:

  The engine and its version for each side.

- sides:

  For each side, how it was simulated, its provenance and the model text
  the engine ran.

- design, tol, atol:

  The arguments.

## Details

Each side runs on an engine (rxode2 or mrgsolve):

- A written rxode2 or mrgsolve model runs as written, on its own
  format's engine. That is the text under test.

- A model, or a written NONMEM model, is written by zufapmx to an engine
  and runs there: its source format's engine when that is rxode2 or
  mrgsolve, and rxode2 otherwise.

A side never runs on the engine of the other side's written text, so a
translation is never checked by the writer that made it: `a` compared
with `pmx_write_rxode2(a)` runs `a` on mrgsolve. `engines` overrides the
choice, including this rule.

Predictions are compared after aligning on regimen, subject and time. A
point agrees when its relative deviation is at most `tol` or its
absolute deviation at most `atol`, which covers predictions near zero.
The relative deviation is `abs(a - b) / max(abs(a), abs(b))`. A missing
or non-finite prediction never agrees. Residual error is off.

A comparison that fails is a value, not an error: `pass` is `FALSE` and
the result says where the models differ. Models that cannot be compared
point by point, because their numbers of random effects differ, fail
with a `reason` and no predictions.

## See also

[`pmx_report()`](https://pedrobtz.github.io/zufapmx/reference/pmx_report.md)
writes a comparison as Markdown.

Other comparison:
[`pmx_design()`](https://pedrobtz.github.io/zufapmx/reference/pmx_design.md),
[`pmx_report()`](https://pedrobtz.github.io/zufapmx/reference/pmx_report.md)

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral", "model.ctl",
  package = "zufapmx"
)
model <- pmx_read_nonmem(path)
# Check the NONMEM writer. Both sides run on rxode2, since NONMEM is not
# an engine here; pmx_write_rxode2(model) would run `model` on mrgsolve.
written <- pmx_write_nonmem(model)
cmp <- pmx_compare(model, written, pmx_design(n = 2, regimens = "single"))
cmp
#> <zufapmx comparison> pass
#> a: nonmem model, written to rxode2 by zufapmx, on rxode2 5.1.8
#> b: nonmem text, written to rxode2 by zufapmx, on rxode2 5.1.8
#> Tolerance: relative 1e-06, absolute 1e-09.
#> 0 of 24 predictions disagree.
#> Worst: single, subject 1, time 0.5: 0.77012778 against 0.77012778 (abs 0, rel 0).
#>  regimen points failed max_abs max_rel skipped
#>   single     24      0       0       0        
```
