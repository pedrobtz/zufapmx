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
  pred <- corpus_solve_rxode2(corpus$path[[i]], corpus$regimen[[i]])
  pred$pred <- signif(pred$pred, 10)
  out <- file.path("inst", "corpus", corpus$id[[i]], "expected.csv")
  utils::write.csv(pred, out, row.names = FALSE, quote = FALSE)
  message("Wrote ", out)
}
