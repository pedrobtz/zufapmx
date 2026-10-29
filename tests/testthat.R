# This file is part of the standard setup for testthat.
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html
#
# testthat is in Suggests, so the suite runs only when it is installed. CRAN's
# NOSUGGESTS check installs hard dependencies only and must not fail here.

if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(zufapmx)

  test_check("zufapmx")
}
