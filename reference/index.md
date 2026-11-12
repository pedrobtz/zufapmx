# Package index

## Check a translation

Simulate two models on one design, each through its own engine, and
compare their predictions.

- [`pmx_compare()`](https://pedrobtz.github.io/zufapmx/reference/pmx_compare.md)
  : Check that two models are the same model
- [`pmx_design()`](https://pedrobtz.github.io/zufapmx/reference/pmx_design.md)
  : A simulation design for comparing models
- [`pmx_report()`](https://pedrobtz.github.io/zufapmx/reference/pmx_report.md)
  : Write a comparison as a Markdown report

## Read a model

- [`pmx_read_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_nonmem.md)
  : Read a NONMEM control stream
- [`pmx_read_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_rxode2.md)
  : Read an rxode2 or nlmixr2 model function
- [`pmx_read_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_mrgsolve.md)
  : Read an mrgsolve model specification
- [`pmx_read_tables()`](https://pedrobtz.github.io/zufapmx/reference/pmx_read_tables.md)
  : Read NONMEM table output

## Write a model

Every writer returns the text with its fidelity: exact, equivalent or
lossy.

- [`pmx_write_rxode2()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_rxode2.md)
  : Write a model as an rxode2 model function
- [`pmx_write_nonmem()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_nonmem.md)
  : Write a model as a NONMEM control stream
- [`pmx_write_mrgsolve()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_mrgsolve.md)
  : Write a model as an mrgsolve model specification
- [`pmx_fidelity()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md)
  [`pmx_losses()`](https://pedrobtz.github.io/zufapmx/reference/pmx_fidelity.md)
  : Fidelity and losses of a written model
- [`pmx_write_json()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_json.md)
  [`pmx_read_json()`](https://pedrobtz.github.io/zufapmx/reference/pmx_write_json.md)
  : Write a model as JSON, and read it back

## Inspect a model

- [`pmx_parameters()`](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
  [`pmx_omega()`](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
  [`pmx_ode()`](https://pedrobtz.github.io/zufapmx/reference/pmx_inspect.md)
  : Inspect a model

## Model corpus

- [`zufapmx_corpus()`](https://pedrobtz.github.io/zufapmx/reference/zufapmx_corpus.md)
  : The shipped model corpus
