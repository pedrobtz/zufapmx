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
  (`<file>.xml`) when it exists and zuxml is installed, else the `.ext`
  file, else none. `"xml"`, `"ext"` and `"lst"` read that file and fail
  if it is missing; `"none"` reads no estimates.

## Value

A `zufapmx_model`; see
[pmx_inspect](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
and
[`pmx_write_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_nonmem.md).

## Details

The reader covers `$PROBLEM`, `$INPUT`, `$DATA`, `$SUBROUTINES`,
`$MODEL`, `$PK`, `$DES`, `$ERROR`, `$THETA`, `$OMEGA`, `$SIGMA`,
`$ESTIMATION`, `$COVARIANCE` and `$TABLE`. Closed forms are `ADVAN1` to
`ADVAN4`, `ADVAN11` and `ADVAN12` with `TRANS2` or `TRANS4`; general ODE
models are `ADVAN6`, `ADVAN8`, `ADVAN9` and `ADVAN13`. Abbreviated code
may use assignments, `IF` in one line or in blocks, arithmetic,
comparisons, `EXP`, `LOG`, `LOG10`, `SQRT` and `ABS`.

Verbatim Fortran, `$INFN`, `$PRED`, `$MIX`, `$PRIOR`, user subroutines,
dosing or scale assignments inside `IF`, undeclared `THETA`/`ETA`/`EPS`
indices and anything else outside that subset are refused with an error
of class `zufapmx_parse_error` naming the file and, where it can, the
line and column.

Recorded as losses of the read: the title, `$SIZES`, data records,
estimation settings, `$SIMULATION`, `$ABBREVIATED`, tables (`$TABLE`,
`$SCATTERPLOT`), record options such as `$ERROR (ONLY OBSERVATIONS)`,
comments, the ODE solver, `$ERROR` outputs `Y` does not use, and values
that NM-TRAN keeps from earlier records for variables assigned only
inside `IF` (the representation starts them at 0).

NONMEM's names are kept, upper case: `THETA(1)` reads as `THETA1`,
`ETA(1)` as `ETA1` and `EPS(1)` as `EPS1`. Time is `t`: read from `T` in
`$DES` and from `TIME` elsewhere; `TIME` in `$DES` is refused. Free
names such as `WT` are recorded as covariates. The observation is the
endpoint `Y`.

## See also

[pmx_inspect](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
for the model's parts.

Other readers:
[`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_mrgsolve.md),
[`pmx_read_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_rxode2.md)

## Examples

``` r
path <- system.file(
  "corpus", "twocmt-bolus", "model.ctl",
  package = "zufapmx"
)
pmx_read_nonmem(path)
#> <zufapmx_model> read from nonmem
#> Structure: two-compartment closed form, direct input (ADVAN3 TRANS4)
#> Parameters: 4, initial values only
#> Random effects: ETA1 on CL (exp), ETA2 on V1 (exp)
#> Endpoints: Y (proportional)
```
