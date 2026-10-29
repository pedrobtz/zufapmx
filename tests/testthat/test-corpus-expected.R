test_that("expected.csv has one row per design time and the right columns", {
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    expected <- corpus_expected(corpus$path[[i]])

    expect_named(expected, c("regimen", "time", "endpoint", "pred"))
    expect_true(
      all(expected$regimen == corpus$regimen[[i]]),
      label = corpus$id[[i]]
    )
    expect_true(all(expected$endpoint == "cp"), label = corpus$id[[i]])
    expect_equal(expected$time, corpus_times(corpus$regimen[[i]]))
    expect_true(all(expected$pred > 0), label = corpus$id[[i]])
  }
})

test_that("each entry's rxode2 model reproduces its expected.csv", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    got <- corpus_solve_entry_rxode2(corpus$path[[i]], corpus$regimen[[i]])
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("each entry's mrgsolve model reproduces its expected.csv", {
  skip_on_cran()
  skip_if_not_installed("mrgsolve")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    cpp <- file.path(corpus$path[[i]], "model.cpp")
    got <- corpus_solve_mrgsolve(cpp, corpus$regimen[[i]])
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("a missing or short output never counts as agreement", {
  expect_equal(corpus_max_rel_dev(NULL, c(1, 2)), Inf)
  expect_equal(corpus_max_rel_dev(1, c(1, 1)), Inf)
  expect_equal(corpus_max_rel_dev(c(1, NA), c(1, 2)), Inf)
  expect_equal(corpus_max_rel_dev(c(1, 2), c(1, 2)), 0)
})
