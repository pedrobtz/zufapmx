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

A `zufapmx_model`; see
[pmx_inspect](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
and
[`pmx_write_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_mrgsolve.md).

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
`fmin()`, `fmax()`, `ETA(n)`, `EPS(n)`, `THETA(n)`, and the macros
`dxdt_CMT`, `F_CMT`, `ALAG_CMT`, `R_CMT`, `D_CMT` and `CMT_0`. Anything
else, including `$GLOBAL`, `$PREAMBLE`, the `nm-vars` plugin, `int`
declarations and integer division (`3/4`), is refused with an error of
class `zufapmx_parse_error`. `$PKMODEL` may take its parameters from
`$PARAM` directly or from `$MAIN`.

Recorded as losses of the read: the title, `$PLUGIN`, `$SET`, `$PARAM`
descriptions, `$PARAM` values written as constant expressions
(`log(2)`), which are evaluated, and `$TABLE` outputs `Y` does not use.
Time is `t`: read from `SOLVERTIME` in `$ODE` and from `TIME` elsewhere;
the other name is refused, since it is a different time. `$NMXML` and
`$NMEXT` resolve a relative path against the working directory, or the
model file's with `root = "cppfile"`, as mrgsolve does.

The observation is `Y` in `$TABLE`, written over a prediction such as
`IPRED`; its residual model is recognised as for NONMEM. An `@annotated`
`$PARAM` description of the form `[lower, upper]`, optionally followed
by `fixed`, gives a parameter's bounds, as
[`pmx_write_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_mrgsolve.md)
writes them.

## See also

[pmx_inspect](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
for the model's parts.

Other readers:
[`pmx_read_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_nonmem.md),
[`pmx_read_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_rxode2.md)

## Examples

``` r
path <- system.file(
  "corpus", "twocmt-bolus", "model.cpp",
  package = "zufapmx"
)
pmx_read_mrgsolve(path)
#> <zufapmx_model> read from mrgsolve
#> Structure: two-compartment closed form, direct input (ADVAN3 TRANS4)
#> Parameters: 4, initial values only
#> Random effects: ETA1 on CL (exp), ETA2 on V1 (exp)
#> Endpoints: Y (proportional)
```
