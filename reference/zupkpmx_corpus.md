# The shipped model corpus

Lists the models that ship with zupkpmx. Each entry is one model written
in every format that can hold it, with the typical-subject predictions
the formats must agree on. The corpus is the package's test suite and a
set of known-equivalent translations to compare against.

## Usage

``` r
zupkpmx_corpus()
```

## Value

A data frame with one row per entry, ordered by `id`:

- id:

  The entry's name, also its directory name.

- title:

  A one-line description of the model.

- structure:

  The NONMEM `ADVAN`/`TRANS` pair it corresponds to.

- regimen:

  The dose route of its predictions: `"bolus"`, `"oral"` or
  `"infusion"`.

- formats:

  The formats the entry is written in, comma-separated: `"nonmem"`
  (`model.ctl`), `"rxode2"` (`model-rxode2.R`) and `"mrgsolve"`
  (`model.cpp`).

- source:

  Where the model comes from, and its licence.

- path:

  The entry's directory.

## Examples

``` r
corpus <- zupkpmx_corpus()
corpus[, c("id", "structure", "formats")]
#>                  id      structure                formats
#> 1       onecmt-oral  ADVAN2 TRANS2 nonmem,rxode2,mrgsolve
#> 2    onecmt-oral-mm        ADVAN13 nonmem,rxode2,mrgsolve
#> 3 threecmt-infusion ADVAN11 TRANS4 nonmem,rxode2,mrgsolve
#> 4      twocmt-bolus  ADVAN3 TRANS4 nonmem,rxode2,mrgsolve
#> 5       twocmt-oral  ADVAN4 TRANS4 nonmem,rxode2,mrgsolve
```
