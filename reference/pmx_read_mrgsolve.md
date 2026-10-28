# Read an mrgsolve model specification

Reads an mrgsolve model file (`.cpp`) into the package's representation.

## Usage

``` r
pmx_read_mrgsolve(file)
```

## Arguments

- file:

  Path to the model file.

## Value

A `zupkpmx_model`.

## Details

The reader covers `$PROB`, `$PARAM` (plain or `@annotated`), `$THETA`,
`$CMT`, `$INIT`, `$MAIN`/`$PK`, `$ODE`/`$DES`, `$TABLE`/`$ERROR`,
`$CAPTURE`, `$PKMODEL`, `$OMEGA` and `$SIGMA` (with `@block`), and
`$NMXML`/`$NMEXT` as sources of NONMEM estimates. Blocks may also be
written `[ NAME ]`. Code is the C++ subset mrgsolve documents for
models: declarations and assignments, `if`/`else`, arithmetic,
comparisons, [`exp()`](https://rdrr.io/r/base/Log.html),
[`log()`](https://rdrr.io/r/base/Log.html),
[`log10()`](https://rdrr.io/r/base/Log.html),
[`sqrt()`](https://rdrr.io/r/base/MathFun.html), `fabs()`, `pow()`,
`fmin()`, `fmax()`, `ETA(n)`, `EPS(n)`, and the macros `dxdt_CMT`,
`F_CMT`, `ALAG_CMT`, `R_CMT`, `D_CMT` and `CMT_0`. Anything else,
including `$GLOBAL`, `$PREAMBLE` and the `nm-vars` plugin, is refused
with an error of class `zupkpmx_parse_error`.

The observation is `Y` in `$TABLE`, written over a prediction such as
`IPRED`; its residual model is recognised as for NONMEM. An `@annotated`
`$PARAM` description of the form `[lower, upper]`, optionally followed
by `fixed`, gives a parameter's bounds, as
[`pmx_write_mrgsolve()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_write_mrgsolve.md)
writes them.

## Examples

``` r
corpus <- zupkpmx_corpus()
pmx_read_mrgsolve(file.path(corpus$path[[4]], "model.cpp"))
#> <zupkpmx_model> read from mrgsolve
#> Structure: two-compartment closed form, direct input (ADVAN3 TRANS4)
#> Parameters: 4, initial values only
#> Random effects: ETA1 on CL (exp), ETA2 on V1 (exp)
#> Endpoints: Y (proportional)
```
