# Every nlmixr2lib model either reads and round-trips exactly, or is refused
# with a parse error naming the construct. The models are GPL and are read
# from the installed package, never copied into the corpus.

test_that("nlmixr2lib models round-trip exactly or are refused", {
  skip_if_not_installed("nlmixr2lib")
  names <- nlmixr2lib::modeldb$name
  accepted <- character()

  for (name in names) {
    fun <- suppressMessages(nlmixr2lib::readModelDb(name))
    model <- tryCatch(pmx_read_rxode2(fun), zupkpmx_parse_error = function(e) {
      NULL
    })
    if (!is.null(model)) {
      accepted <- c(accepted, name)
      expect_equal(pmx_fidelity(pmx_write_rxode2(model)), "exact", label = name)
    }
  }

  pk <- grep("^PK_[123]cmt", names, value = TRUE)
  expect_true(all(pk %in% accepted))
  expect_gte(length(accepted), length(names) / 2)
})
