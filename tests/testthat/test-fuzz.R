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
