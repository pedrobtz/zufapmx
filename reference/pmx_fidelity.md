# Fidelity and losses of a written model

Every `pmx_write_*()` function returns the model text together with how
faithful it is. `pmx_fidelity()` is one of:

## Usage

``` r
pmx_fidelity(written)

pmx_losses(written)
```

## Arguments

- written:

  The result of a `pmx_write_*()` function.

## Value

`pmx_fidelity()`: a string. `pmx_losses()`: a character vector, empty
when nothing was lost.

## Details

- `"exact"`: the written model reads back to the same representation.

- `"equivalent"`: a reparameterisation that simulates identically.

- `"lossy"`: something was dropped that does not change predictions,
  such as estimation settings, tables or comments; `pmx_losses()` names
  each item.

A construct that would change predictions is never dropped: the writer
stops with an error of class `zufapmx_write_error` instead.

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral", "model-rxode2.R",
  package = "zufapmx"
)
fun <- eval(parse(path))
written <- pmx_write_rxode2(pmx_read_rxode2(fun))
pmx_fidelity(written)
#> [1] "exact"
pmx_losses(written)
#> character(0)
```
