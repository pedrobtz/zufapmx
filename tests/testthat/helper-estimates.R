# NONMEM output fixtures for the estimate tests (fixtures/nonmem/README.md).

# run1.ctl beside an output file of the given type, written from `lines`.
local_run <- function(ext, lines, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(test_path("fixtures", "nonmem", "run1.ctl"), dir)
  writeLines(lines, file.path(dir, paste0("run1.", ext)))
  file.path(dir, "run1.ctl")
}

fixture_lines <- function(file) {
  readLines(test_path("fixtures", "nonmem", file))
}

# run2's three output files agree, with distinct off-diagonal elements.
expect_run2_estimates <- function(source) {
  omega <- matrix(
    c(0.11, 0.012, 0.023, 0.012, 0.21, 0.034, 0.023, 0.034, 0.31),
    3,
    dimnames = list(paste0("ETA", 1:3), paste0("ETA", 1:3))
  )
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run2.ctl"), source)
  re <- unclass(m)$random_effects

  expect_equal(pmx_parameters(m)$estimate, c(1.21, 4.31, 51.1), label = source)
  expect_equal(pmx_omega(m), omega, label = source)
  expect_equal(
    diag(pmx_omega(m, "init")),
    c(ETA1 = 0.1, ETA2 = 0.2, ETA3 = 0.3),
    label = source
  )
  expect_equal(
    c(re$sigma[[1]]$estimate[[1]], re$sigma[[2]]$estimate[[1]]),
    c(0.031, 0.41),
    label = source
  )
}
