# A simulation design for comparing models

Describes what
[`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
simulates: the regimens, the subjects and the covariate values. Each
regimen doses compartment `cmt` (by position, as NONMEM numbers
compartments) with 100 units and observes at fixed times:

## Usage

``` r
pmx_design(
  n = 20,
  seed = 1,
  regimens = c("single", "infusion", "multiple", "ss"),
  cmt = 1,
  covariates = list()
)
```

## Arguments

- n:

  The number of subjects with random effects, besides the typical
  subject. From 0 to 10000.

- seed:

  The seed of the random effects, a whole number. It is part of the
  design, so the same design always draws the same values.

- regimens:

  The regimens to simulate, a subset of `"single"`, `"infusion"`,
  `"multiple"` and `"ss"`.

- cmt:

  The compartment every regimen doses, by position. The default, 1, is
  NONMEM's default dose compartment: the depot of a model with
  first-order absorption.

- covariates:

  A named list or numeric vector of covariate values, one number each,
  given to every subject. Names match a model's covariates without
  regard to case, since NONMEM upper-cases them.

## Value

A `zupmxio_design`: a list with the arguments and `events`, a data frame
of the dose and observation records of every regimen with columns
`regimen`, `id`, `time`, `evid`, `amt`, `rate`, `ii`, `addl` and `ss`.

## Details

- `"single"`: one dose at time 0. Into a depot this is a first-order
  dose, into the central compartment a bolus. Observed at 0.5, 1, 2, 4,
  6, 8, 12 and 24.

- `"infusion"`: one dose at time 0 over 2 time units. Observed at the
  same times as `"single"`.

- `"multiple"`: five doses 12 time units apart (`ii = 12`, `addl = 4`).
  Observed at 1, 6, 11.5, 13, 18, 24.5, 36.5, 48.5, 50, 54, 60 and 72,
  never at a dose time.

- `"ss"`: a dose at steady state (`ss = 1`, `ii = 12`). Observed at 0.5,
  1, 2, 4, 6, 8 and 11.5.

A model that sets the dose compartment's rate or duration itself is
dosed with `rate = -1` or `rate = -2`, as NONMEM's data sets do, and its
`"infusion"` regimen is skipped.

Subject 1 is the typical subject, with every random effect at zero.
Subjects 2 to `n + 1` take random effects drawn from each model's omega
with `seed`. Both models get the same standard normal draws, so models
with the same omega get the same random effects. Residual error is
always off: a comparison is of predictions, not of noise.

## See also

Other comparison:
[`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md),
[`pmx_report()`](https://pedrobtz.github.io/zupmxio/reference/pmx_report.md)

## Examples

``` r
pmx_design()
#> <zupmxio design> single, infusion, multiple, ss, dosing compartment 1
#> The typical subject and 20 with random effects (seed 1).
pmx_design(n = 5, regimens = c("single", "ss"), covariates = list(WT = 70))
#> <zupmxio design> single, ss, dosing compartment 1
#> The typical subject and 5 with random effects (seed 1).
#> Covariates: WT = 70.
```
