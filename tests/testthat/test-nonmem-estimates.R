# The estimate sources of design §6: each malformed or mismatched file is
# refused with what was seen, never attached.

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

test_that("an .ext with the wrong number of thetas is refused", {
  ctl <- local_run("ext", fixture_lines("run2.ext"))
  expect_refused(pmx_read_nonmem(ctl, "ext"), "3 thetas")
})

test_that("an .ext whose omega does not fit the model's blocks is refused", {
  ext <- fixture_lines("run1.ext")
  cut <- function(line) {
    fields <- strsplit(trimws(line), "\\s+")[[1]]
    paste(fields[-(9:10)], collapse = "  ")
  }
  ext[-1] <- vapply(ext[-1], cut, character(1))
  ctl <- local_run("ext", ext)
  expect_refused(pmx_read_nonmem(ctl, "ext"), "a 1 by 1 matrix")
})

test_that("an .ext without rows or without a final row is refused", {
  ext <- fixture_lines("run1.ext")
  expect_refused(
    pmx_read_nonmem(local_run("ext", ext[1]), "ext"),
    "end of file",
    1
  )
  expect_refused(
    pmx_read_nonmem(
      local_run("ext", ext[!startsWith(ext, "  -1000000000")]),
      "ext"
    ),
    "no final row",
    1
  )
  expect_refused(
    pmx_read_nonmem(local_run("ext", "not a table"), "ext"),
    "not a table",
    1
  )
})

test_that("unreadable XML, or XML without an estimation, is refused", {
  skip_if_not_installed("zuxml")
  expect_refused(pmx_read_nonmem(local_run("xml", "<a"), "xml"), "the XML")
  expect_refused(
    pmx_read_nonmem(local_run("xml", "<a/>"), "xml"),
    "no <nm:estimation>"
  )
})

test_that("an .lst without final estimates is refused", {
  expect_refused(
    pmx_read_nonmem(local_run("lst", "NONMEM run aborted"), "lst"),
    "no final estimates"
  )
})

test_that("a missing output file named by `estimates` is a model error", {
  expect_error(
    pmx_read_nonmem(local_run("ext", fixture_lines("run1.ext")), "lst"),
    class = "zufapmx_model_error"
  )
})

test_that("$NMXML reads run.xml through zuxml", {
  skip_if_not_installed("zuxml")
  xml <- normalizePath(
    test_path("fixtures", "nonmem", "run1.xml"),
    winslash = "/"
  )
  m <- read_cpp(c(
    sprintf("$NMXML path = \"%s\"", xml),
    "$PKMODEL advan = 4, trans = 4, cmt = \"DEPOT CENT PERIPH\"",
    "$MAIN",
    "double KA = THETA1;",
    "double CL = THETA2 * exp(ETA(1));",
    "double V2 = THETA3 * exp(ETA(2));",
    "double Q = THETA4;",
    "double V3 = THETA5;",
    "$TABLE",
    "double IPRED = CENT / V2;",
    "double Y = IPRED * (1 + EPS(1));"
  ))
  # run1.xml's estimates differ from run1.ext's, so the source shows.
  expect_equal(pmx_parameters(m)$estimate, c(1.13, 4.26, 21.6, 2.16, 41.6))
  expect_equal(basename(unclass(m)$provenance$estimates), "run1.xml")
})

test_that("$NMEXT keeps a correlated omega as one block", {
  ext <- normalizePath(
    test_path("fixtures", "nonmem", "run2.ext"),
    winslash = "/"
  )
  m <- read_cpp(c(
    sprintf("$NMEXT path = \"%s\"", ext),
    "$PKMODEL cmt = \"GUT CENT\", depot = TRUE",
    "$MAIN",
    "double KA = THETA1 * exp(ETA(1));",
    "double CL = THETA2 * exp(ETA(2));",
    "double V = THETA3 * exp(ETA(3));",
    "$TABLE",
    "double IPRED = CENT / V;",
    "double Y = IPRED * (1 + EPS(1)) + EPS(2);"
  ))
  omega <- pmx_omega(m)
  expect_equal(dim(omega), c(3L, 3L))
  expect_true(all(omega[lower.tri(omega)] != 0))
  expect_length(unclass(m)$random_effects$omega, 1)
})
