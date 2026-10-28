# Signal an error of class `zupkpmx_<kind>_error`, which also inherits from
# `zupkpmx_error`, so callers and tests can match on the class.
zupkpmx_abort <- function(message, kind, call = sys.call(-1), ...) {
  stop(structure(
    class = c(
      paste0("zupkpmx_", kind, "_error"),
      "zupkpmx_error",
      "error",
      "condition"
    ),
    list(message = message, call = call, ...)
  ))
}
