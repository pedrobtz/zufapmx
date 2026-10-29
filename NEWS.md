# zufapmx 0.0.0.9000

* Fidelity is stricter: a write is `"equivalent"` only through the
  reparameterisations writers are known to make, and a translation that
  reads back as a different model is a `zufapmx_write_error`.

* Time is one symbol, `t`, read from and written to each format's own
  spelling (NONMEM `TIME`/`T`, mrgsolve `TIME`/`SOLVERTIME`, rxode2
  `t`/`time`). Free names are recorded as covariates in a model's
  provenance.

* Readers refuse more of what they cannot translate faithfully (dosing
  assignments inside `IF`, undeclared `THETA`/`ETA`/`EPS`, C++ integer
  division) and accept more of what they can (`$PKMODEL` over `$PARAM`,
  `$SIZES`, record options, statement-defined residual parameters).

* `pmx_read_mrgsolve()` reads an mrgsolve model file and
  `pmx_write_mrgsolve()` writes one.

* `pmx_write_nonmem()` writes a NONMEM control stream.

* `pmx_read_nonmem()` reads a NONMEM control stream, with final estimates
  from `run.xml`, the `.ext` file or the `.lst` report.

* `pmx_read_rxode2()` reads an rxode2 or nlmixr2 model function and
  `pmx_write_rxode2()` writes one. `pmx_fidelity()` and `pmx_losses()` say
  how faithful a written model is.

* `pmx_parameters()`, `pmx_omega()` and `pmx_ode()` inspect a model, and
  `print()` summarises it.

* `zufapmx_corpus()` lists the shipped model corpus: one-, two- and
  three-compartment closed-form models and a Michaelis-Menten ODE model,
  each written in NONMEM, rxode2 and mrgsolve form with its typical-subject
  predictions.

* Initial CRAN submission.
