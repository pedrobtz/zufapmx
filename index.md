# zupmxio

A population PK/PD model often has to move between tools: a NONMEM run
becomes an nlmixr2 model for a new analysis, or an mrgsolve model for
simulation. Converters exist, but a translated model is usually trusted
on sight, and a slip in a volume or a residual model shows up much
later, if at all.

zupmxio makes the translation checkable. It reads a model from NONMEM,
rxode2/nlmixr2 or mrgsolve into one R object, writes it to any of the
three, and proves that two models are the same model by simulating both
and comparing their predictions.

- **The check is the product.**
  [`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
  simulates both models on a canonical design (single dose, infusion,
  multiple doses and steady state, for a typical subject and subjects
  with random effects) and reports the worst deviation against a
  tolerance. A failed comparison is a result that says where the models
  differ, not an error.
- **Independent engines.** Each model runs on its own engine, rxode2 or
  mrgsolve, and never on the engine of the writer under test, so a
  translation is not checked by the code that made it.
- **Nothing is guessed.** Code outside the supported subset is refused
  with its position. A writer that would drop something that changes
  predictions refuses the model, and every write reports its fidelity:
  `exact`, `equivalent` or `lossy`, with what was lost.
- **Against NONMEM’s own output.**
  [`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
  also takes the tables a NONMEM run wrote, and checks a translation
  against the run’s `PRED`.
- **A corpus with oracles.** Every shipped model is checked against
  [nonmem2rx](https://CRAN.R-project.org/package=nonmem2rx) and, for ODE
  models, [amp.sim](https://CRAN.R-project.org/package=amp.sim).

## Installation

Install zupmxio from CRAN:

``` r

install.packages("zupmxio")
```

Or the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("pedrobtz/zupmxio")
```

zupmxio itself has no dependencies. Comparisons need the engines,
[rxode2](https://CRAN.R-project.org/package=rxode2) and
[mrgsolve](https://CRAN.R-project.org/package=mrgsolve), which need a C
and C++ compiler to build models.

## Usage

``` r

library(zupmxio)
```

Read a model. This one is a two-compartment model with first-order
absorption, from the corpus that ships with the package:

``` r

path <- system.file("corpus", "twocmt-oral", "model.ctl", package = "zupmxio")
model <- pmx_read_nonmem(path)
model
#> <zupmxio_model> read from nonmem
#> Structure: two-compartment closed form, first-order input (ADVAN4 TRANS4)
#> Parameters: 5, initial values only
#> Random effects: ETA1 on CL (exp), ETA2 on V2 (exp)
#> Endpoints: Y (proportional)
```

Write it to rxode2. The fidelity says the translation is a known
reparameterisation, here residual error as a standard deviation rather
than a variance:

``` r

rx <- pmx_write_rxode2(model)
pmx_fidelity(rx)
#> [1] "equivalent"
```

Check it. The NONMEM model runs on mrgsolve and the rxode2 text runs as
written, on rxode2, and every one of the predictions agrees:

``` r

pmx_compare(model, rx)
#> <zupmxio comparison> pass
#> a: nonmem model, written to mrgsolve by zupmxio, on mrgsolve 2.0.1
#> b: rxode2 text, simulated as written, on rxode2 5.1.8
#> Tolerance: relative 1e-06, absolute 1e-09.
#> 0 of 735 predictions disagree.
#> Worst: ss, subject 15, time 0.5: 4.4041147 against 4.4041147 (abs 6.06e-10, rel 1.38e-10).
#>   regimen points failed  max_abs  max_rel skipped
#>    single    168      0 1.02e-14 8.11e-15        
#>  infusion    168      0 8.44e-15 7.44e-15        
#>  multiple    252      0 1.60e-14 8.68e-15        
#>        ss    147      0 6.06e-10 1.79e-10
```

A slip fails. Here the central volume of the rxode2 text is 21 instead
of 20, and the comparison says where the predictions part:

``` r

slip <- rx
slip$text <- sub("THETA3 <- c(0, 20)", "THETA3 <- c(0, 21)", slip$text, fixed = TRUE)
pmx_compare(model, slip)
#> <zupmxio comparison> FAIL
#> a: nonmem model, written to mrgsolve by zupmxio, on mrgsolve 2.0.1
#> b: rxode2 text, simulated as written, on rxode2 5.1.8
#> Tolerance: relative 1e-06, absolute 1e-09.
#> 735 of 735 predictions disagree.
#> Worst: infusion, subject 5, time 8: 0.26833057 against 0.28530119 (abs 0.017, rel 0.0595).
#>   regimen points failed max_abs max_rel skipped
#>    single    168    168   0.125  0.0592        
#>  infusion    168    168   0.102  0.0595        
#>  multiple    252    252   0.125  0.0587        
#>        ss    147    147   0.119  0.0404
```

[`pmx_report()`](https://pedrobtz.github.io/zupmxio/reference/pmx_report.md)
writes a comparison as Markdown, with the design, the deviations and
both model texts, for a reviewer to read without running anything.

Functions are named by what they do: `pmx_read_*()` and `pmx_write_*()`
for each format, and
[`pmx_design()`](https://pedrobtz.github.io/zupmxio/reference/pmx_design.md),
[`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
and
[`pmx_report()`](https://pedrobtz.github.io/zupmxio/reference/pmx_report.md)
for the check.

## Learn more

- [Validating a
  translation](https://pedrobtz.github.io/zupmxio/articles/validating-a-translation.html)
  walks through a check end to end, and shows how to read a report.
- The [reference](https://pedrobtz.github.io/zupmxio/reference/) lists
  the readers, the writers and the comparison functions.
- [`zupmxio_corpus()`](https://pedrobtz.github.io/zupmxio/reference/zupmxio_corpus.md)
  lists the shipped models.

## Related work

zupmxio is checked against
[nonmem2rx](https://CRAN.R-project.org/package=nonmem2rx), which
translates NONMEM to rxode2, and
[amp.sim](https://CRAN.R-project.org/package=amp.sim), which translates
it to mrgsolve;
[oracle-agreement.md](https://github.com/pedrobtz/zupmxio/blob/main/.agents/oracle-agreement.md)
records where they agree. The
[nlmixr2lib](https://CRAN.R-project.org/package=nlmixr2lib) model
library is read in its tests. Its tables and run files are read with
[zucsv](https://CRAN.R-project.org/package=zucsv) and
[zuxml](https://CRAN.R-project.org/package=zuxml) when installed.
