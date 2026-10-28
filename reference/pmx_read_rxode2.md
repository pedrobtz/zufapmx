# Read an rxode2 or nlmixr2 model function

Reads the `ini({})` and `model({})` blocks of a model function into the
package's representation. The function is read as R syntax, so neither
rxode2 nor nlmixr2 needs to be installed.

## Usage

``` r
pmx_read_rxode2(model)
```

## Arguments

- model:

  A model function, as written for rxode2 or nlmixr2.

## Value

A `zupkpmx_model`.

## Details

Supported: fixed effects with bounds and
[`fix()`](https://rdrr.io/r/utils/fix.html); random effects, alone or in
blocks; statements built from the function table; `linCmt()`; `d/dt()`;
`alag()`, [`lag()`](https://rdrr.io/r/stats/lag.html), `f()`, `rate()`,
`dur()` and initial amounts (`depot(0) <-`); and residual models
`add()`, `prop()`, `add() + prop()`, `lnorm()` and `pow()`. Anything
else is refused with an error of class `zupkpmx_parse_error`. Labels,
`description` and comments are dropped and recorded as losses.

## Examples

``` r
one_cmt <- function() {
  ini({
    tka <- 0.45
    tcl <- 1
    tv <- 3.45
    eta.cl ~ 0.3
    prop.sd <- 0.1
  })
  model({
    ka <- exp(tka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv)
    cp <- linCmt()
    cp ~ prop(prop.sd)
  })
}
pmx_read_rxode2(one_cmt)
#> <zupkpmx_model> read from rxode2
#> Structure: one-compartment closed form, first-order input (ADVAN2 TRANS2)
#> Parameters: 4, initial values only
#> Random effects: eta.cl on cl (exp)
#> Endpoints: cp (proportional)
```
