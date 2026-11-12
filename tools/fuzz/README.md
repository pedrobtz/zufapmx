# Fuzz targets

One [zufuzz](https://github.com/pedrobtz/zufuzz) harness per reader:
`nonmem.R`, `mrgsolve.R` and `rxode2.R`. Each feeds bytes to a reader and
applies the oracle in `tests/testthat/helper-fuzz.R`:

- the reader reads the input or refuses it with a `zupmxio_parse_error`;
- a model it reads writes to every format or is refused with a
  `zupmxio_write_error`, and a write's fidelity is `exact`, `equivalent`
  or `lossy`;
- the model round-trips through JSON when zujson is installed.

Any other error is a finding. The rxode2 harness never runs its input:
only a top-level `function` expression is evaluated, which builds a
function without running its body.

zufuzz is not on CRAN, so it is not in `Suggests`, and `tools/` is left
out of the package. The same oracle runs in every check:
`tests/testthat/test-fuzz.R` replays the corpus and seeded mutations of
it.

```sh
Rscript tools/fuzz/seed-corpus.R            # seeds from inst/corpus
Rscript -e 'zufuzz::fuzz_file("tools/fuzz/nonmem.R",
  corpus = "tools/fuzz/corpus/nonmem", time_limit = 300)'
```

A finding is replayed with `zufuzz::replay()` and minimized with
`zufuzz::minimize()`; add the minimized input to the mutations replayed
by `test-fuzz.R` as a regression test.
