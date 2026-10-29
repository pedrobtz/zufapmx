# Design §9: every corpus entry is a snapshot test of its representation,
# as read from each format it is written in.

test_that("every corpus entry's representation is stable", {
  for (id in zufapmx_corpus()$id) {
    models <- corpus_models(dirname(corpus_file(id, "entry.dcf")), id)
    for (from in names(models)) {
      expect_snapshot({
        cat(id, "read from", from, "\n")
        str(model_content(models[[from]]))
      })
    }
  }
})

test_that("a lossy write prints what it lost", {
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run1.ctl"), "ext")
  w <- pmx_write_nonmem(m)

  expect_snapshot(print(w))
  expect_identical(format(w), w$text)
  expect_identical(as.character(w), w$text)
})
