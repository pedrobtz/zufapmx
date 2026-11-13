# Replays the fuzz targets (helper-fuzz.R) on the corpus and on seeded
# mutations of it. tools/fuzz/ runs the same targets under zufuzz.

test_that("every corpus model passes its parser's fuzz target", {
  for (format in c("nonmem", "mrgsolve", "rxode2")) {
    for (seed in fuzz_seeds(format)) {
      expect_fuzz_ok(seed, format, "seed")
      expect_false(is.null(fuzz_target(seed, format)))
    }
  }
})

test_that("mutated models are read, refused or written, never crash", {
  withr::local_seed(46)
  for (format in c("nonmem", "mrgsolve", "rxode2")) {
    seeds <- fuzz_seeds(format)
    for (s in seq_along(seeds)) {
      inputs <- fuzz_mutations(seeds[[s]], 25, fuzz_vocabulary[[format]])
      for (i in seq_along(inputs)) {
        expect_fuzz_ok(
          inputs[[i]],
          format,
          sprintf("seed %d, mutation %d", s, i)
        )
      }
    }
  }
})

test_that("the rxode2 target never runs the input", {
  marker <- tempfile()
  code <- sprintf("file.create(%s)", deparse(marker))
  for (text in c(
    code,
    paste0("function() { ", code, " }"),
    paste(code, "; function() {}")
  )) {
    expect_null(fuzz_target(charToRaw(text), "rxode2"))
  }
  expect_false(file.exists(marker))
})

test_that("fuzz bytes become valid text", {
  expect_identical(fuzz_text(as.raw(c(0x41, 0x00, 0x42))), "AB")
  expect_true(validUTF8(fuzz_text(as.raw(c(0x41, 0xff, 0x42)))))
})

test_that("seeds with CRLF line endings read as the same models", {
  # A Windows checkout gives the corpus CRLF line endings; readLines()
  # drops the CR, and so must the fuzz targets.
  for (format in c("nonmem", "mrgsolve", "rxode2")) {
    for (seed in fuzz_seeds(format)) {
      lf <- seed[seed != as.raw(13)]
      crlf <- charToRaw(gsub("\n", "\r\n", rawToChar(lf), fixed = TRUE))
      from_crlf <- fuzz_target(crlf, format)
      expect_false(is.null(from_crlf))
      # The models, not their provenance, which names a temporary file.
      expect_identical(
        model_content(from_crlf),
        model_content(fuzz_target(lf, format))
      )
    }
  }
})
