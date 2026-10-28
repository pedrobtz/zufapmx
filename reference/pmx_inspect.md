# Inspect a model

`pmx_parameters()` returns the fixed effects, `pmx_omega()` the
between-subject variability matrix, and `pmx_ode()` the differential
equations of an ODE model.

## Usage

``` r
pmx_parameters(model)

pmx_omega(model)

pmx_ode(model)
```

## Arguments

- model:

  A model read by one of the `pmx_read_*()` functions.

## Value

- `pmx_parameters()`: a data frame with one row per fixed effect and
  columns `name` (the tool's name), `role` (`"clearance"`, `"volume"`,
  `"ka"` or `"other"`), `init`, `lower`, `upper`, `fixed`, and
  `estimate` (`NA` when the model was read without estimates).

- `pmx_omega()`: a symmetric matrix with the random effects' names as
  dimnames, built block-diagonally from the model's omega blocks.

- `pmx_ode()`: a named list of expressions, one derivative per
  compartment, or `NULL` for a closed-form model.
