# Regenerate inst/corpus/*/expected.csv: typical-subject predictions of each
# entry's rxode2 model on the standard design. Run from the package root:
#
#   Rscript tools/corpus-expected.R
#
# Commit the result only after checking the change is intended; the corpus
# test compares the models against these files.

pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-corpus.R")

corpus <- zupkpmx_corpus()
for (i in seq_len(nrow(corpus))) {
  regimen <- corpus$regimen[[i]]
  pred <- corpus_solve_entry_rxode2(corpus$path[[i]], regimen)
  out <- data.frame(
    regimen = regimen,
    time = corpus_times(regimen),
    endpoint = "cp",
    pred = signif(pred, 10)
  )
  file <- file.path("inst", "corpus", corpus$id[[i]], "expected.csv")
  utils::write.csv(out, file, row.names = FALSE, quote = FALSE)
  message("Wrote ", file)
}
