# Changelog

## zufapmx 0.0.0.9000

- Models follow one rule for who may assign and read what (design D18):
  parameters, random effects, compartments and time are never assigned,
  ODE code does not reassign individual parameters, and individual
  statements do not read compartment amounts. The rxode2 reader moves
  statements into the ODE code until that holds, and keeps an endpoint
  that other statements read as a statement.

- Readers refuse more constructs that would change predictions: a `Y`
  reassigned or set inside `IF`, a scale or dosing attribute assigned
  twice, `TSCALE`, `XSCALE`, `INITIALOFF`, code after a plain `ELSE`,
  fractional compartment indices, `$ODE` beside `$PKMODEL`, time from
  the wrong record (`TIME` in `$DES`, `SOLVERTIME` outside `$ODE`),
  `$NMXML` and `$NMEXT` options the reader does not honour, C++ integer
  division of comparisons, and rxode2’s `combined1()`.

- A NONMEM `Y` that does not use `F`, as in `IPRED = A(2)/V`, gives its
  own prediction instead of the compartment amount.

- A refusal of a statement points at its record’s first line, and a
  writer’s refusal lists only formats that do write the model.

- [`pmx_write_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_nonmem.md)
  lists the model’s covariates in `$INPUT`. Writers record lost bounds
  on residual standard deviations, and
  [`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_mrgsolve.md)
  records `$PARAM` values written as expressions.

- [`pmx_read_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_rxode2.md)
  accepts `=` in `ini()` and `combined2()`.

- An invalid argument is a `zufapmx_model_error` with one wording across
  functions, and a model edited after reading is validated again before
  it is written or inspected.

- Fidelity is stricter: a write is `"equivalent"` only through the
  reparameterisations writers are known to make, and a translation that
  reads back as a different model is a `zufapmx_write_error`.

- Time is one symbol, `t`, read from and written to each format’s own
  spelling (NONMEM `TIME`/`T`, mrgsolve `TIME`/`SOLVERTIME`, rxode2
  `t`/`time`). Free names are recorded as covariates in a model’s
  provenance.

- Readers refuse more of what they cannot translate faithfully (dosing
  assignments inside `IF`, undeclared `THETA`/`ETA`/`EPS`, C++ integer
  division) and accept more of what they can (`$PKMODEL` over `$PARAM`,
  `$SIZES`, record options, statement-defined residual parameters).

- [`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_mrgsolve.md)
  reads an mrgsolve model file and
  [`pmx_write_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_mrgsolve.md)
  writes one.

- [`pmx_write_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_nonmem.md)
  writes a NONMEM control stream.

- [`pmx_read_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_nonmem.md)
  reads a NONMEM control stream, with final estimates from `run.xml`,
  the `.ext` file or the `.lst` report.

- [`pmx_read_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_rxode2.md)
  reads an rxode2 or nlmixr2 model function and
  [`pmx_write_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_rxode2.md)
  writes one.
  [`pmx_fidelity()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md)
  and
  [`pmx_losses()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md)
  say how faithful a written model is.

- [`pmx_parameters()`](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md),
  [`pmx_omega()`](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
  and
  [`pmx_ode()`](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
  inspect a model, and [`print()`](https://rdrr.io/r/base/print.html)
  summarises it.

- [`zufapmx_corpus()`](https://pedrobtz.github.io/zufapmx/reference/zufapmx_corpus.md)
  lists the shipped model corpus: one-, two- and three-compartment
  closed-form models and a Michaelis-Menten ODE model, each written in
  NONMEM, rxode2 and mrgsolve form with its typical-subject predictions.

- Initial CRAN submission.
