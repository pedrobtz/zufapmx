# Read NONMEM table output

Reads the files a NONMEM `$TABLE` record writes, to compare a
translation against the run's own predictions with
`pmx_compare(tables, model)`. The comparison simulates the model on the
table's records, so the table needs the dosing records as well as the
observations.

## Usage

``` r
pmx_read_tables(file, prediction = "PRED")
```

## Arguments

- file:

  The path of a table file, or several from one run.

- prediction:

  The name of the prediction column.

## Value

A `zupmxio_tables` object: a list with `records`, a data frame with
columns `id`, `time`, `evid`, `amt`, `cmt`, `rate`, `ii`, `addl`, `ss`
and `pred` followed by the table's other columns under their own names;
`prediction`, the column read as `pred`; `digits`, the significant
digits of the prediction column; and `files`.

## Details

A table is NONMEM's layout: an optional `TABLE NO. n` title line, a line
of column names, then one record per line, separated by spaces or
commas. Several files from one run (say `sdtab1` and `patab1`) are read
side by side: they must have the same records, and a column in more than
one file must hold the same values in each.

The records need `ID`, `TIME` and the prediction column. `AMT`, `EVID`,
`MDV`, `CMT`, `RATE`, `II`, `ADDL` and `SS` are read when present, with
NONMEM's defaults when not. Without `EVID`, a record with a positive
`AMT` is a dose and any other record with `MDV` 0 (or no `MDV`) is an
observation. Records that are neither are dropped, and `EVID` 3 and 4
(resets) are refused. Every other column is kept, so the model's
covariates can come from the table. Column names match without regard to
case.

The prediction column should be `PRED`, NONMEM's population prediction:
the comparison simulates the model's typical subject, every random
effect at zero (design Q1). NONMEM prints a fixed number of significant
digits, 5 by default; the reader records it, and the comparison accepts
a prediction that rounds to the table's value.

Tables are read with the zucsv package when it is installed, and with
base R otherwise; both give the same result.

## See also

[`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
compares a model with the tables.

Other readers:
[`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zupmxio/reference/pmx_read_mrgsolve.md),
[`pmx_read_nonmem()`](https://pedrobtz.github.io/zupmxio/reference/pmx_read_nonmem.md),
[`pmx_read_rxode2()`](https://pedrobtz.github.io/zupmxio/reference/pmx_read_rxode2.md),
[`pmx_write_json()`](https://pedrobtz.github.io/zupmxio/reference/pmx_write_json.md)

## Examples

``` r
file <- tempfile(fileext = ".tab")
writeLines(c(
  "TABLE NO.  1",
  " ID          TIME        AMT         EVID        PRED",
  "  1.0000E+00  0.0000E+00  1.0000E+02  1.0000E+00  0.0000E+00",
  "  1.0000E+00  1.0000E+00  0.0000E+00  0.0000E+00  1.8187E+00"
), file)
pmx_read_tables(file)
#> <zupmxio tables> 1 subject, 1 dose and 1 observation records
#> Prediction: `PRED`, 5 significant digits.
#> From file18784b026723.tab.
```
