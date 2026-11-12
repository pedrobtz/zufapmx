# Write a comparison as a Markdown report

Writes what a reviewer needs to judge a translation from the report
alone: the verdict, the engines and their versions, the design with its
seed, the deviations per regimen, the worst point, every point that
disagrees, and the text of both models as the engines ran them.

## Usage

``` r
pmx_report(comparison, file = NULL, max_points = 20)
```

## Arguments

- comparison:

  A comparison from
  [`pmx_compare()`](https://pedrobtz.github.io/zufapmx/reference/pmx_compare.md).

- file:

  The path to write, or `NULL` to only return the text.

- max_points:

  The most disagreeing points to list, from 0. The report says how many
  more there were.

## Value

The report's lines, invisibly.

## See also

Other comparison:
[`pmx_compare()`](https://pedrobtz.github.io/zufapmx/reference/pmx_compare.md),
[`pmx_design()`](https://pedrobtz.github.io/zufapmx/reference/pmx_design.md)

## Examples

``` r
path <- system.file(
  "corpus", "onecmt-oral", "model.ctl",
  package = "zufapmx"
)
model <- pmx_read_nonmem(path)
cmp <- pmx_compare(
  model,
  pmx_write_nonmem(model),
  pmx_design(n = 2, regimens = "single")
)
file <- tempfile(fileext = ".md")
pmx_report(cmp, file)
cat(readLines(file, n = 12), sep = "\n")
#> # Model comparison
#> 
#> **Result: pass.** Every prediction agrees within the tolerances.
#> 
#> ## Models
#> 
#> | Side | Simulated | Engine | Source |
#> | --- | --- | --- | --- |
#> | a | nonmem model, written to rxode2 by zufapmx | rxode2 5.1.8 | nonmem, `model.ctl` |
#> | b | nonmem text, written to rxode2 by zufapmx | rxode2 5.1.8 | written nonmem, fidelity exact |
#> 
#> ## Design
```
