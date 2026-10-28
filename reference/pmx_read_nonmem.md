# Read a NONMEM control stream

Reads a control stream into the package's representation, with final
estimates when NONMEM's output files are next to it.

## Usage

``` r
pmx_read_nonmem(file, estimates = c("auto", "xml", "ext", "lst", "none"))
```

## Arguments

- file:

  Path to the control stream.

- estimates:

  Where final estimates come from. `"auto"` reads `run.xml`
  (`<file>.xml`) through zuxml when both exist, else the `.ext` file,
  else none. `"xml"`, `"ext"` and `"lst"` read that file and fail if it
  is missing; `"none"` reads no estimates.

## Value

A `zupkpmx_model`.

## Details

The reader covers `$PROBLEM`, `$INPUT`, `$DATA`, `$SUBROUTINES`,
`$MODEL`, `$PK`, `$DES`, `$ERROR`, `$THETA`, `$OMEGA`, `$SIGMA`,
`$ESTIMATION`, `$COVARIANCE` and `$TABLE`. Closed forms are `ADVAN1` to
`ADVAN4`, `ADVAN11` and `ADVAN12` with `TRANS2` or `TRANS4`; general ODE
models are `ADVAN6`, `ADVAN8`, `ADVAN9` and `ADVAN13`. Abbreviated code
may use assignments, `IF` in one line or in blocks, arithmetic,
comparisons, `EXP`, `LOG`, `LOG10`, `SQRT` and `ABS`.

Verbatim Fortran, `$INFN`, `$PRED`, `$MIX`, `$PRIOR`, user subroutines
and anything else outside that subset are refused with an error of class
`zupkpmx_parse_error` naming the file, line and column. The title, data
records, estimation settings, tables, comments and the ODE solver are
recorded as losses of the read.

NONMEM's names are kept: `THETA(1)` reads as `THETA1`, `ETA(1)` as
`ETA1` and `EPS(1)` as `EPS1`. The observation is the endpoint `Y`.

## Examples

``` r
corpus <- zupkpmx_corpus()
pmx_read_nonmem(file.path(corpus$path[[4]], "model.ctl"))
#> <zupkpmx_model> read from nonmem
#> Structure: two-compartment closed form, direct input (ADVAN3 TRANS4)
#> Parameters: 4, initial values only
#> Random effects: ETA1 on CL (exp), ETA2 on V1 (exp)
#> Endpoints: Y (proportional)
```
