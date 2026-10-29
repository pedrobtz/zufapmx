# Write a model as an mrgsolve model specification

Prints a model as the text of an mrgsolve model file (`.cpp`) with its
fidelity (see
[`pmx_fidelity()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md)).

## Usage

``` r
pmx_write_mrgsolve(model)
```

## Arguments

- model:

  A model read by one of the `pmx_read_*()` functions.

## Value

A `zufapmx_written` object: the text as lines, with its fidelity and
losses (see
[`pmx_fidelity()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md)).
[`format()`](https://rdrr.io/r/base/format.html) and
[`as.character()`](https://rdrr.io/r/base/character.html) give the text.

## Details

Fixed effects go to `$PARAM @annotated`, whose description records each
one's bounds and whether it is fixed (`[0, Inf] fixed`), so nothing an
estimation needs is lost;
[`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_mrgsolve.md)
reads them back. A closed form is written as `$PKMODEL` with its `advan`
and `trans`, an ODE model as `$CMT` and `$ODE`. The prediction is
captured as `IPRED` and the observation as `Y`, with residual error over
`EPS(n)` and `$SIGMA`; residual error held as standard deviations, as
rxode2 holds it, is written as variances. `$PARAM` holds one value per
parameter, so a model with estimates is written with them and its
initial values are recorded as lost.

## See also

Other writers:
[`pmx_fidelity()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md),
[`pmx_write_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_nonmem.md),
[`pmx_write_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_rxode2.md)

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral", "model-rxode2.R",
  package = "zufapmx"
)
fun <- eval(parse(path))
pmx_write_mrgsolve(pmx_read_rxode2(fun))
#> $PROB zufapmx model read from rxode2
#> 
#> $PARAM @annotated
#> tka : 1 : [0, Inf]
#> tcl : 4 : [0, Inf]
#> tv : 50 : [0, Inf]
#> 
#> $PKMODEL advan = 2, trans = 2, cmt = "DEPOT CENT"
#> 
#> $MAIN
#> double ka = tka;
#> double cl = tcl * exp(ETA(1));
#> double v = tv * exp(ETA(2));
#> double CL = cl;
#> double V = v;
#> double KA = ka;
#> 
#> $OMEGA
#> 0.09
#> 
#> $OMEGA
#> 0.09
#> 
#> $SIGMA
#> 0.04
#> 
#> $TABLE
#> double IPRED = CENT / V;
#> double Y = IPRED * (1 + EPS(1));
#> 
#> $CAPTURE IPRED Y
#> # mrgsolve, fidelity: equivalent
```
