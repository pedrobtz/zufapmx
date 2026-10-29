# Write a model as a NONMEM control stream

Prints a model as an NM-TRAN control stream with its fidelity (see
[`pmx_fidelity()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_fidelity.md)).

## Usage

``` r
pmx_write_nonmem(model, estimates = c("final", "initial"))
```

## Arguments

- model:

  A model read by one of the `pmx_read_*()` functions.

- estimates:

  `"final"` writes final estimates where the model has them, and its
  initial values elsewhere; `"initial"` writes initial values. Whichever
  of the two is not written is recorded as lost.

## Value

A `zupkpmx_written` object: the text as lines, with its fidelity and
losses (see
[`pmx_fidelity()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_fidelity.md)).
[`format()`](https://rdrr.io/r/base/format.html) and
[`as.character()`](https://rdrr.io/r/base/character.html) give the text.

## Details

Fixed effects become `THETA(n)`, random effects `ETA(n)` and residual
errors `EPS(n)`, numbered in the model's order; compartment amounts
become `A(n)`. A closed form is written as its `ADVAN`/`TRANS` pair, an
ODE model as `ADVAN13` with `$MODEL` and `$DES`. Residual error held as
standard deviations, as rxode2 holds it, is written as `$SIGMA`
variances: the same model, parameterised as NONMEM parameterises it.

`$INPUT` and `$DATA` are placeholders to point at a dataset, and no
`$ESTIMATION` is written: the representation holds neither.

## See also

Other writers:
[`pmx_write_mrgsolve()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_write_mrgsolve.md),
[`pmx_write_rxode2()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_write_rxode2.md)

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral", "model-rxode2.R",
  package = "zupkpmx"
)
fun <- eval(parse(path))
pmx_write_nonmem(pmx_read_rxode2(fun))
#> $PROBLEM zupkpmx model read from rxode2
#> ; zupkpmx: $INPUT and $DATA are placeholders; point them at your dataset.
#> $INPUT ID TIME EVID AMT RATE CMT DV
#> $DATA data.csv IGNORE=@
#> $SUBROUTINES ADVAN2 TRANS2
#> $PK
#> KA = THETA(1)
#> CL = THETA(2) * EXP(ETA(1))
#> V = THETA(3) * EXP(ETA(2))
#> S2 = V
#> $ERROR
#> Y = F * (1 + EPS(1))
#> $THETA (0, 1) ; tka
#> $THETA (0, 4) ; tcl
#> $THETA (0, 50) ; tv
#> $OMEGA 0.09
#> $OMEGA 0.09
#> $SIGMA 0.04
#> # nonmem, fidelity: equivalent
```
