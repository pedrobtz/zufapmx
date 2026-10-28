# Write a model as an rxode2 model function

Prints a model as the text of an rxode2/nlmixr2 model function, with its
fidelity (see
[`pmx_fidelity()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_fidelity.md)).
Evaluate the text to get the function.

## Usage

``` r
pmx_write_rxode2(model)
```

## Arguments

- model:

  A model read by one of the `pmx_read_*()` functions.

## Value

A written model: the text as lines, its fidelity and its losses.

## Details

rxode2's `ini()` holds one value per parameter: a model with estimates
is written with its estimates, and its initial values are recorded as
lost. Residual error held as sigma variances, as NONMEM holds it, is
written as standard-deviation parameters named after the epsilons.
Constructs rxode2 cannot hold, such as residual error written as an
expression over epsilons, stop with an error of class
`zupkpmx_write_error`.

## Examples

``` r
corpus <- zupkpmx_corpus()
fun <- eval(parse(file.path(corpus$path[[2]], "model-rxode2.R")))
pmx_write_rxode2(pmx_read_rxode2(fun))
#> function() {
#>   ini({
#>     tka <- c(0, 1)
#>     tvm <- c(0, 20)
#>     tkm <- c(0, 2)
#>     tv <- c(0, 50)
#>     prop.sd <- 0.2
#>     eta.vm ~ 0.09
#>     eta.v ~ 0.09
#>   })
#>   model({
#>     ka <- tka
#>     vm <- tvm * exp(eta.vm)
#>     km <- tkm
#>     v <- tv * exp(eta.v)
#>     c2 <- central/v
#>     d/dt(depot) <- -ka * depot
#>     d/dt(central) <- ka * depot - vm * c2/(km + c2)
#>     cp <- central/v
#>     cp ~ prop(prop.sd)
#>   })
#> }
#> # rxode2, fidelity: exact
```
