# Engines and oracles are Suggests, so their tests skip without them. CI
# sets ZUFAPMX_REQUIRE_ENGINES=true so that a leg where one failed to
# install fails instead of skipping its only end-to-end checks; legs that
# check without Suggests on purpose set _R_CHECK_FORCE_SUGGESTS_=false.
skip_if_no_engine <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(invisible(TRUE))
  }
  required <- identical(Sys.getenv("ZUFAPMX_REQUIRE_ENGINES"), "true") &&
    !identical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_"), "false")
  if (required) {
    testthat::fail(sprintf(
      "%s is required on this CI leg but not installed",
      pkg
    ))
  }
  testthat::skip(sprintf("%s is not installed", pkg))
}
