# zupmxio 0.1.0

First release. zupmxio holds a population PK/PD model as one R object,
reads it from NONMEM, rxode2/nlmixr2 and mrgsolve, writes it to any of the
three, and checks that two models are the same model.

* `pmx_read_nonmem()`, `pmx_read_rxode2()` and `pmx_read_mrgsolve()` read
  a model: closed forms (ADVAN 1 to 4, 11 and 12) and ODE systems, with
  random effects, covariates, dosing attributes and residual models. Code
  outside the supported subset is refused with its position, never
  guessed.

* `pmx_write_nonmem()`, `pmx_write_rxode2()` and `pmx_write_mrgsolve()`
  write it, each reporting its fidelity (`exact`, `equivalent` or
  `lossy`) and what was lost. A construct that would change predictions
  is refused, never dropped.

* `pmx_compare()` simulates both models on a `pmx_design()` (single dose,
  infusion, multiple doses and steady state, for a typical subject and
  subjects with random effects), each on its own engine and never on the
  engine of the writer under test, and compares the predictions point by
  point. A failed comparison is a value that says where the models
  differ. `pmx_report()` writes a comparison as Markdown.

* `pmx_read_tables()` reads NONMEM `$TABLE` output, and
  `pmx_compare(tables, model)` checks a translation against the run's own
  `PRED`.

* `pmx_write_json()` and `pmx_read_json()` save and reload a model
  unchanged.

* `zupmxio_corpus()` lists the shipped models, each in every format that
  holds it, with the predictions they agree on.
