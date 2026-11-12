# zupmxio 0.0.0.9000

* The package is renamed zupmxio, from zufapmx (design D24). Condition
  classes are `zupmxio_*_error` and the corpus is `zupmxio_corpus()`; the
  `pmx_*` functions keep their names.

* `pmx_compare()` checks that two models are the same model. It simulates
  both on a `pmx_design()` (single dose, infusion, multiple doses and
  steady state, for a typical subject and `n` subjects with random
  effects), each through its own engine and never on the engine of the
  writer under test, and compares the predictions point by point. A
  failed comparison is a value that says where the models differ.
  `pmx_report()` writes a comparison as Markdown, with both model texts.

* Reading a control stream is about twice as fast: the NM-TRAN and C++
  tokenizers match all their patterns in one pass per line.

* `pmx_read_tables()` reads NONMEM `$TABLE` output, and
  `pmx_compare(tables, model)` checks a translation against the run's own
  `PRED`: the model's typical subject is simulated on the tables' records
  and compared within the precision NONMEM printed.

* `pmx_write_json()` and `pmx_read_json()` write the representation as
  JSON and read it back unchanged (needs zujson).

* `pmx_write_rxode2()` writes NONMEM's combined residual
  `IPRED * (1 + EPS(1)) + EPS(2)`, which it refused (#129), and writers no
  longer fail on a closed-form parameter assigned from a covariate (#130).

* `pmx_write_mrgsolve()` declares covariates in `$INPUT`, without a
  default value, so its output compiles; `pmx_read_mrgsolve()` reads
  `$INPUT` names as covariates (#126).

* Models follow one rule for who may assign and read what (design D18):
  parameters, random effects, compartments and time are never assigned,
  ODE code does not reassign individual parameters, and individual
  statements do not read compartment amounts. The rxode2 reader moves
  statements into the ODE code until that holds, and keeps an endpoint
  that other statements read as a statement.

* Readers refuse more constructs that would change predictions: a `Y`
  reassigned or set inside `IF`, a scale or dosing attribute assigned
  twice, `TSCALE`, `XSCALE`, `INITIALOFF`, code after a plain `ELSE`,
  fractional compartment indices, `$ODE` beside `$PKMODEL`, time from the
  wrong record (`TIME` in `$DES`, `SOLVERTIME` outside `$ODE`), `$NMXML`
  and `$NMEXT` options the reader does not honour, C++ integer division
  of comparisons, and rxode2's `combined1()`.

* A NONMEM `Y` that does not use `F`, as in `IPRED = A(2)/V`, gives its
  own prediction instead of the compartment amount.

* A refusal of a statement points at its record's first line, and a
  writer's refusal lists only formats that do write the model.

* `pmx_write_nonmem()` lists the model's covariates in `$INPUT`. Writers
  record lost bounds on residual standard deviations, and
  `pmx_read_mrgsolve()` records `$PARAM` values written as expressions.

* `pmx_read_rxode2()` accepts `=` in `ini()` and `combined2()`.

* An invalid argument is a `zupmxio_model_error` with one wording across
  functions, and a model edited after reading is validated again before
  it is written or inspected.

* Fidelity is stricter: a write is `"equivalent"` only through the
  reparameterisations writers are known to make, and a translation that
  reads back as a different model is a `zupmxio_write_error`.

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

* `zupmxio_corpus()` lists the shipped model corpus: one-, two- and
  three-compartment closed-form models and a Michaelis-Menten ODE model,
  each written in NONMEM, rxode2 and mrgsolve form with its typical-subject
  predictions.

* Initial CRAN submission.
