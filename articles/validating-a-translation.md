# Validating a translation

A translated model is a claim: this rxode2 model, or this mrgsolve
model, is the NONMEM model it came from. This article shows how zupmxio
checks that claim, what a comparison report contains, and how a reviewer
reads one.

``` r

library(zupmxio)
```

## Translate

The example is a two-compartment model with first-order absorption, one
of the models that ship with the package. Reading it gives the model as
one R object, whatever format it came from:

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

Writing it gives the text in the target format and how faithful it is:

``` r

mrg <- pmx_write_mrgsolve(model)
pmx_fidelity(mrg)
#> [1] "exact"
```

A write is one of three things:

- `exact`: the written text reads back to the same model.
- `equivalent`: a known reparameterisation, such as residual error held
  as a standard deviation instead of a variance, or NONMEM’s `ADVAN4`
  parameters under rxode2’s `linCmt()` names.
- `lossy`: something was dropped that does not change predictions, such
  as estimation settings or initial values where the format holds only
  one value per parameter.
  [`pmx_losses()`](https://pedrobtz.github.io/zupmxio/reference/pmx_fidelity.md)
  names each item.

A construct that would change predictions is never dropped. The writer
stops with an error of class `zupmxio_write_error` that names it and the
formats that can hold it.

Fidelity is a check of the representation: the text reads back as the
same model. It does not run the model. That is the comparison’s job.

## Compare

[`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
simulates both models and compares their predictions, point by point:

``` r

cmp <- pmx_compare(model, mrg)
cmp
#> <zupmxio comparison> pass
#> a: nonmem model, written to rxode2 by zupmxio, on rxode2 5.1.8
#> b: mrgsolve text, simulated as written, on mrgsolve 2.0.1
#> Tolerance: relative 1e-06, absolute 1e-09.
#> 0 of 735 predictions disagree.
#> Worst: ss, subject 15, time 0.5: 4.4041147 against 4.4041147 (abs 6.06e-10, rel 1.38e-10).
#>   regimen points failed  max_abs  max_rel skipped
#>    single    168      0 1.02e-14 8.11e-15        
#>  infusion    168      0 8.44e-15 7.44e-15        
#>  multiple    252      0 1.60e-14 8.68e-15        
#>        ss    147      0 6.06e-10 1.79e-10
```

Three things decide what a comparison means.

**The design.** By default, four regimens of 100 units into compartment
1: a single dose, a 2-hour infusion, five doses 12 hours apart, and a
dose at steady state. Each is simulated for a typical subject, with
every random effect at zero, and 20 subjects whose random effects are
drawn from omega with a fixed seed. Residual error is always off: the
check is of predictions, not of noise.
[`pmx_design()`](https://pedrobtz.github.io/zupmxio/reference/pmx_design.md)
changes any of this, and gives covariates their values:

``` r

pmx_design(n = 5, regimens = c("single", "ss"), covariates = list(WT = 70))
#> <zupmxio design> single, ss, dosing compartment 1
#> The typical subject and 5 with random effects (seed 1).
#> Covariates: WT = 70.
```

Compartment 1 is NONMEM’s default dose compartment. Into a depot, a
single dose is a first-order dose; into a central compartment, a bolus.

**The engines.** Each side runs on rxode2 or mrgsolve. A written rxode2
or mrgsolve text runs as written, on its own engine: that is the text
under test. A model runs on its source format’s engine, and a NONMEM
model on rxode2, except that a side never runs on the engine of the
other side’s written text. Above, the mrgsolve text runs on mrgsolve, so
the NONMEM model runs on rxode2, written by a different writer. A check
of the mrgsolve writer that ran both sides through mrgsolve would share
its mistakes. `engines` overrides the choice when there is a reason to.

**The tolerance.** A point agrees when its relative deviation,
`abs(a - b) / max(abs(a), abs(b))`, is at most `tol` (`1e-6`), or its
absolute deviation at most `atol` (`1e-9`), which covers predictions
near zero. A missing or non-finite prediction never agrees. Both engines
solve with tight tolerances, so a correct translation agrees far inside
`tol`: the largest relative deviation above is 1.8e-10.

## When a comparison fails

Here is a translation with a slip: the central volume is 21 where the
source says 20.

``` r

slip <- mrg
slip$text <- sub("THETA3 : 20 :", "THETA3 : 21 :", slip$text, fixed = TRUE)
bad <- pmx_compare(model, slip)
bad
#> <zupmxio comparison> FAIL
#> a: nonmem model, written to rxode2 by zupmxio, on rxode2 5.1.8
#> b: mrgsolve text, simulated as written, on mrgsolve 2.0.1
#> Tolerance: relative 1e-06, absolute 1e-09.
#> 735 of 735 predictions disagree.
#> Worst: infusion, subject 5, time 8: 0.26833057 against 0.28530119 (abs 0.017, rel 0.0595).
#>   regimen points failed max_abs max_rel skipped
#>    single    168    168   0.125  0.0592        
#>  infusion    168    168   0.102  0.0595        
#>  multiple    252    252   0.125  0.0587        
#>        ss    147    147   0.119  0.0404
```

A failed comparison is a value, not an error. `bad$predictions` holds
every point, with both predictions and both deviations, so the pattern
of a failure can be read off it. A wrong central volume shows from the
first observation on, since every concentration is an amount over it:

``` r

p <- bad$predictions
head(p[p$regimen == "single" & p$id == 1, c("time", "a", "b", "rel")], 4)
#>   time        a        b         rel
#> 1  0.5 1.815977 1.736035 0.044021542
#> 2  1.0 2.666641 2.559871 0.040039347
#> 3  2.0 2.968728 2.876659 0.031013093
#> 4  4.0 2.081864 2.061203 0.009924413
```

Other slips leave other patterns. A wrong lag or bioavailability, for
instance, shows only on the regimens and times it touches.

## The report

[`pmx_report()`](https://pedrobtz.github.io/zupmxio/reference/pmx_report.md)
writes a comparison as Markdown, for a reviewer who will not run
anything:

``` r

file <- tempfile(fileext = ".md")
pmx_report(bad, file, max_points = 3)
```

It opens with the result, then names the models and how each was
simulated:

    # Model comparison

    **Result: FAIL.** 735 of 735 predictions disagree.

    ## Models

    | Side | Simulated | Engine | Source |
    | --- | --- | --- | --- |
    | a | nonmem model, written to rxode2 by zupmxio | rxode2 5.1.8 | nonmem, `model.ctl` |
    | b | mrgsolve text, simulated as written | mrgsolve 2.0.1 | written mrgsolve, fidelity exact |

    ## Design

    Regimens single, infusion, multiple, ss, each a dose of 100 into compartment 1.
    The typical subject (1) and 20 subjects with random effects drawn with seed 1. Residual error is off.
    A point agrees when its relative deviation is at most 1e-06 or its absolute deviation at most 1e-09.

Then the deviations, per regimen, the worst point, and the first points
that disagree:

    ## Results

    | Regimen | Points | Disagree | Largest absolute | Largest relative | Skipped |
    | --- | --- | --- | --- | --- | --- |
    | single | 168 | 168 | 0.125 | 0.0592 |  |
    | infusion | 168 | 168 | 0.102 | 0.0595 |  |
    | multiple | 252 | 252 | 0.125 | 0.0587 |  |
    | ss | 147 | 147 | 0.119 | 0.0404 |  |

    Worst point: infusion, subject 5, time 8: 0.2683305675 (a) against 0.2853011936 (b), absolute deviation 0.017, relative 0.0595.

    ### Points that disagree

    | Regimen | Subject | Time | a | b | Absolute | Relative |
    | --- | --- | --- | --- | --- | --- | --- |
    | single | 1 | 0.5 | 1.815977364 | 1.73603524 | 0.0799 | 0.044 |
    | single | 1 | 1 | 2.666641129 | 2.559870558 | 0.107 | 0.04 |
    | single | 1 | 2 | 2.968728415 | 2.876658965 | 0.0921 | 0.031 |

    And 732 more.

Last come both model texts, exactly as the engines ran them, so the
reviewer can see the slip:

    ## Model b

    ```cpp
    $PROB model.ctl

    $PARAM @annotated
    THETA1 : 1 : [0, Inf]
    THETA2 : 4 : [0, Inf]
    THETA3 : 21 : [0, Inf]
    THETA4 : 2 : [0, Inf]
    THETA5 : 40 : [0, Inf]

    $PKMODEL advan = 4, trans = 4, cmt = "DEPOT CENT PERIPH1"

Read a report in this order:

1.  **Result.** Pass or fail, and how many points disagree.
2.  **Models.** Which side is the text under test, which engine ran
    each, and their versions. A side “simulated as written” is the text
    itself; a side “written by zupmxio” went through another writer.
3.  **Design.** The regimens, the number of subjects and the seed. The
    same design and seed give the same random effects, so a comparison
    can be rerun.
4.  **Results.** Where the deviations are. A deviation in every regimen
    points at the structure or a parameter; one regimen alone points at
    dosing: an infusion, a lag, steady state.
5.  **Model texts.** What was actually compared.

## Against NONMEM output

A NONMEM run’s own tables are the reference closest to the source: they
come from NONMEM itself.
[`pmx_read_tables()`](https://pedrobtz.github.io/zupmxio/reference/pmx_read_tables.md)
reads them, and
[`pmx_compare()`](https://pedrobtz.github.io/zupmxio/reference/pmx_compare.md)
then simulates the model’s typical subject on the tables’ records and
compares it with `PRED`.

The table here is written the way a NONMEM `$TABLE` writes one, from the
predictions this model is known to make:

``` r

expected <- read.csv(system.file(
  "corpus", "twocmt-oral", "expected.csv",
  package = "zupmxio"
))
records <- data.frame(
  ID = 1,
  TIME = c(0, expected$time),
  AMT = c(100, rep(0, nrow(expected))),
  EVID = c(1, rep(0, nrow(expected))),
  PRED = c(0, expected$pred)
)
table <- tempfile(fileext = ".tab")
writeLines(c(
  "TABLE NO.  1",
  paste(sprintf("%-11s", names(records)), collapse = " "),
  apply(records, 1, function(r) paste(sprintf("%11.4E", r), collapse = " "))
), table)
readLines(table, n = 4)
#> [1] "TABLE NO.  1"                                               
#> [2] "ID          TIME        AMT         EVID        PRED       "
#> [3] " 1.0000E+00  0.0000E+00  1.0000E+02  1.0000E+00  0.0000E+00"
#> [4] " 1.0000E+00  5.0000E-01  0.0000E+00  0.0000E+00  1.8160E+00"
```

``` r

tables <- pmx_read_tables(table)
tables
#> <zupmxio tables> 1 subject, 1 dose and 8 observation records
#> Prediction: `PRED`, 5 significant digits.
#> From file1a7c58b92cc.tab.
pmx_compare(tables, mrg)
#> <zupmxio comparison> pass
#> a: NONMEM table output, column `PRED` (5 significant digits)
#> b: mrgsolve text, simulated as written, on mrgsolve 2.0.1
#> Tolerance: relative 1e-06, absolute 1e-09.
#> 0 of 8 predictions disagree.
#> Worst: table, subject 1, time 6: 1.2738 against 1.2737512 (abs 4.88e-05, rel 3.83e-05).
#>  regimen points failed  max_abs  max_rel skipped
#>    table      8      0 4.88e-05 3.83e-05
```

NONMEM prints 5 significant digits by default, so no tolerance near
`1e-6` could match its tables. The reader records the printed precision,
and a point also agrees when the model’s prediction rounds to the
table’s value. Covariates come from the table’s columns, and the records
may be any design the run had.

## What a comparison does not check

- **Residual error.** It is off on both sides. The residual model is
  checked by fidelity, as part of the representation, not by simulation.
- **Individual predictions from a run.** Tables are compared on `PRED`,
  the typical subject, not `IPRED` with the run’s random effects.
- **Estimation.** Settings such as `$ESTIMATION` are not translated;
  reading a control stream records them as lost.
- **Designs it did not simulate.** A pass says the models agree on the
  design they were given. A model with time-varying covariates, or a
  route the default regimens miss, deserves a design or a table that
  exercises it.
