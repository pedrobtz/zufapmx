# Write a model as JSON, and read it back

`pmx_write_json()` writes the representation as a JSON document and
`pmx_read_json()` reads one back. The document holds the model's seven
components as data, so a model read from JSON is the model that was
written: the same parameters, statements, provenance and all. It is
zufapmx's own representation, not a file standard (design D3).

## Usage

``` r
pmx_write_json(model, file = NULL, pretty = TRUE)

pmx_read_json(file)
```

## Arguments

- model:

  A model read by one of the `pmx_read_*()` functions.

- file:

  For `pmx_write_json()`, the path to write, or `NULL` to only return
  the text. For `pmx_read_json()`, the path of a JSON file.

- pretty:

  Whether to indent the JSON.

## Value

`pmx_write_json()`: the JSON text, invisibly when `file` is given.
`pmx_read_json()`: a model. A file that is not a zufapmx JSON model is a
`zufapmx_parse_error`.

## Details

Statements and expressions are R code in strings
(`"cl <- tcl * exp(eta.cl)"`), constants are numbers. JSON has no
infinity, so `Inf` and `-Inf` are the strings `"Inf"` and `"-Inf"`; a
missing value is `null`. Numbers are written in the fewest digits that
read back exactly.

Both functions need the zujson package, and raise an error of class
`zufapmx_engine_error` without it.

## See also

Other readers:
[`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_mrgsolve.md),
[`pmx_read_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_nonmem.md),
[`pmx_read_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_rxode2.md),
[`pmx_read_tables()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_tables.md)

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral", "model.ctl",
  package = "zufapmx"
)
model <- pmx_read_nonmem(path)
file <- tempfile(fileext = ".json")
pmx_write_json(model, file)
identical(pmx_read_json(file), model)
#> [1] TRUE
```
