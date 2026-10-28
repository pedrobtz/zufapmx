# zupkpmx 0.0.0.9000

* `pmx_read_nonmem()` reads a NONMEM control stream, with final estimates
  from `run.xml`, the `.ext` file or the `.lst` report.

* `pmx_read_rxode2()` reads an rxode2 or nlmixr2 model function and
  `pmx_write_rxode2()` writes one. `pmx_fidelity()` and `pmx_losses()` say
  how faithful a written model is.

* `pmx_parameters()`, `pmx_omega()` and `pmx_ode()` inspect a model, and
  `print()` summarises it.

* `zupkpmx_corpus()` lists the shipped model corpus: one-, two- and
  three-compartment closed-form models and a Michaelis-Menten ODE model,
  each written in NONMEM, rxode2 and mrgsolve form with its typical-subject
  predictions.

* Initial CRAN submission.
