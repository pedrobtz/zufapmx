# The oracles of design §7, run on the corpus. Where an oracle cannot read
# an entry, .agents/oracle-agreement.md records why.

test_that("nonmem2rx translations agree with every entry", {
  skip_on_cran()
  skip_if_no_engine("nonmem2rx")
  skip_if_no_engine("rxode2")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    got <- corpus_oracle_nonmem2rx(corpus$path[[i]], corpus$regimen[[i]])
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("amp.sim translations agree with every ODE entry", {
  skip_on_cran()
  skip_if_no_engine("amp.sim")
  skip_if_no_engine("mrgsolve")
  corpus <- zufapmx_corpus()
  # amp.sim writes compartments from $MODEL only, so it cannot translate the
  # closed-form ADVANs (oracle-agreement.md).
  corpus <- corpus[corpus$structure == "ADVAN13", ]
  expect_gt(nrow(corpus), 0)

  for (i in seq_len(nrow(corpus))) {
    got <- corpus_oracle_amp_sim(corpus$path[[i]], corpus$regimen[[i]])
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})
