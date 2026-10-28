test_that("zupkpmx_corpus() lists every shipped entry", {
  corpus <- zupkpmx_corpus()

  expect_s3_class(corpus, "data.frame")
  expect_named(
    corpus,
    c("id", "title", "structure", "regimen", "formats", "source", "path")
  )
  expect_equal(
    corpus$id,
    c(
      "onecmt-oral",
      "onecmt-oral-mm",
      "threecmt-infusion",
      "twocmt-bolus",
      "twocmt-oral"
    )
  )
  expect_true(all(dir.exists(corpus$path)))
})

test_that("every format an entry lists has its model file", {
  corpus <- zupkpmx_corpus()
  files <- c(
    nonmem = "model.ctl",
    rxode2 = "model-rxode2.R",
    mrgsolve = "model.cpp"
  )

  for (i in seq_len(nrow(corpus))) {
    formats <- strsplit(corpus$formats[[i]], ",", fixed = TRUE)[[1]]
    expect_true(all(formats %in% names(files)), label = corpus$id[[i]])
    expect_true(
      all(file.exists(file.path(corpus$path[[i]], files[formats]))),
      label = corpus$id[[i]]
    )
  }
})

test_that("an entry with a missing field is an error", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "broken"))
  writeLines(
    c("Id: broken", "Title: t", "Structure: s", "Regimen: oral", "Source: s"),
    file.path(root, "broken", "entry.dcf")
  )

  expect_error(corpus_read(root), class = "zupkpmx_corpus_error")
})

test_that("an entry that lists a missing model file is an error", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "nofile"))
  writeLines(
    c(
      "Id: nofile",
      "Title: t",
      "Structure: s",
      "Regimen: oral",
      "Formats: nonmem",
      "Source: s"
    ),
    file.path(root, "nofile", "entry.dcf")
  )

  expect_error(corpus_read(root), class = "zupkpmx_corpus_error")
})
