# Inspect a model

`pmx_parameters()` returns the fixed effects, `pmx_omega()` the
between-subject variability matrix, and `pmx_ode()` the differential
equations of an ODE model.

## Usage

``` r
pmx_parameters(model)

pmx_omega(model, values = c("estimate", "init"))

pmx_ode(model)
```

## Arguments

- model:

  A model read by one of the `pmx_read_*()` functions.

- values:

  For `pmx_omega()`: `"estimate"` (the default) returns the final
  estimates where the model was read with them, and `"init"` the initial
  values.

## Value

- `pmx_parameters()`: a data frame with one row per fixed effect and
  columns `name` (the tool's name), `role` (`"clearance"`, `"volume"`,
  `"ka"` or `"other"`), `init`, `lower`, `upper`, `fixed`, and
  `estimate` (`NA` when the model was read without estimates).

- `pmx_omega()`: a symmetric matrix with the random effects' names as
  dimnames, built block-diagonally from the model's omega blocks. With
  `values = "estimate"`, a block read with estimates contributes them
  and any other block its initial values.

- `pmx_ode()`: a named list of expressions, one derivative per
  compartment, or `NULL` for a closed-form model.

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral-mm", "model.ctl",
  package = "zufapmx"
)
model <- pmx_read_nonmem(path)
pmx_parameters(model)
#>     name  role init lower upper fixed estimate
#> 1 THETA1 other    1     0   Inf FALSE       NA
#> 2 THETA2 other   20     0   Inf FALSE       NA
#> 3 THETA3 other    2     0   Inf FALSE       NA
#> 4 THETA4 other   50     0   Inf FALSE       NA
pmx_omega(model, "init")
#>      ETA1 ETA2
#> ETA1 0.09 0.00
#> ETA2 0.00 0.09
pmx_ode(model)
#> $DEPOT
#> -KA * DEPOT
#> 
#> $CENTRAL
#> KA * DEPOT - VM * C2/(KM + C2)
#> 
```
