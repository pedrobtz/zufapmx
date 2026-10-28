# Changelog

## zupkpmx 0.0.0.9000

- [`pmx_read_rxode2()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_read_rxode2.md)
  reads an rxode2 or nlmixr2 model function and
  [`pmx_write_rxode2()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_write_rxode2.md)
  writes one.
  [`pmx_fidelity()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_fidelity.md)
  and
  [`pmx_losses()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_fidelity.md)
  say how faithful a written model is.

- [`pmx_parameters()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_inspect.md),
  [`pmx_omega()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_inspect.md)
  and
  [`pmx_ode()`](https://pedrobtz.github.io/zupkpmx/reference/pmx_inspect.md)
  inspect a model, and [`print()`](https://rdrr.io/r/base/print.html)
  summarises it.

- [`zupkpmx_corpus()`](https://pedrobtz.github.io/zupkpmx/reference/zupkpmx_corpus.md)
  lists the shipped model corpus: one-, two- and three-compartment
  closed-form models and a Michaelis-Menten ODE model, each written in
  NONMEM, rxode2 and mrgsolve form with its typical-subject predictions.

- Initial CRAN submission.
