# Every nlmixr2lib model either reads and round-trips exactly, or is refused
# with a parse error naming the construct. The models are GPL and are read
# from the installed package, never copied into the corpus.

test_that("nlmixr2lib models round-trip exactly or are refused", {
  # The sweep follows whatever nlmixr2lib CRAN has, so it runs off CRAN.
  skip_on_cran()
  skip_if_no_engine("nlmixr2lib")
  names <- nlmixr2lib::modeldb$name
  accepted <- character()

  for (name in names) {
    fun <- suppressMessages(nlmixr2lib::readModelDb(name))
    model <- tryCatch(pmx_read_rxode2(fun), zufapmx_parse_error = function(e) {
      NULL
    })
    if (!is.null(model)) {
      accepted <- c(accepted, name)
      expect_equal(pmx_fidelity(pmx_write_rxode2(model)), "exact", label = name)
    }
  }

  pk <- grep("^PK_[123]cmt", names, value = TRUE)
  expect_true(all(pk %in% accepted))
  # nlmixr2lib 0.3.2 has 59 models, of which 44 read; a reader regression
  # that newly refuses one shows here.
  expect_gte(length(accepted), 44)
})
