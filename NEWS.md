# zupmxio 0.0.0.9000

* Readers refuse more models whose meaning they could not keep:
  - a name reassigned after a derivative, dosing attribute or scale reads
    it, whose value the representation could not hold (#146);
  - a format's built-in names (`NEWIND`, `ICALL`, `EVID`, `tad`, ...) read
    as covariates, and NM-TRAN's `CALLFL` (#149); `pi`, `M_PI` and C++
    `true` and `false` read as numbers;
  - omega and sigma blocks that are not positive semi-definite (#150);
  - a NONMEM ODE model scaling a compartment it does not observe, since
    without `DEFOBS` NONMEM observes compartment 1 (#148);
  - an undeclared `EPS(n)` in a residual model (#30), unknown mrgsolve
    `@options` (#154), and `$PKMODEL advan = 2.9` (#155).

* `pmx_write_nonmem()` marks the observed compartment of an ODE model with
  `DEFOBS` (#148), and `pmx_write_rxode2()` writes ADVAN12 under names
  rxode2 simulates (#176).

* `pmx_read_mrgsolve()` reads `$INIT` compartments (#153), `$PARAM
  @covariates` (#154) and multi-line `/* */` comments (#68).

* `pmx_compare()` simulates a model's own rxode2 function or mrgsolve file
  when it can (#170), keeps the caller's random numbers (#162), matches the
  endpoint column exactly (#152), and documents the errors it raises
  (#163). Against tables, each value keeps its own printed precision, so a
  fixed-format small value no longer loosens the rest (#147); EVID 2
  records reach the engines (#169); and tables without observations fail
  with a reason (#156).

* `pmx_read_json()` raises only parse errors for malformed documents
  (#151).

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
