# Signal an error of class `zufapmx_<kind>_error`, which also inherits from
# `zufapmx_error`, so callers and tests can match on the class.
zufapmx_abort <- function(message, kind, call = sys.call(-1), ...) {
  stop(structure(
    class = c(
      paste0("zufapmx_", kind, "_error"),
      "zufapmx_error",
      "error",
      "condition"
    ),
    list(message = message, call = call, ...)
  ))
}

# The condition classes of design §8. Each carries the fields a caller needs
# to act on it; tests match on the class, never the message.

parse_error <- function(
  file,
  line,
  column,
  seen,
  expected = character(),
  message = NULL,
  call = sys.call(-1)
) {
  if (is.null(message)) {
    message <- sprintf(
      "found `%s`, expected %s",
      seen,
      paste0("`", expected, "`", collapse = " or ")
    )
  }
  zufapmx_abort(
    sprintf("%s:%d:%d: %s", file, line, column, message),
    "parse",
    call = call,
    file = file,
    line = line,
    column = column,
    seen = seen,
    expected = expected
  )
}

write_error <- function(construct, location, formats, call = sys.call(-1)) {
  zufapmx_abort(
    sprintf(
      "Cannot write %s (at `%s`). %s",
      construct,
      location,
      if (length(formats) == 0) {
        "No supported format can hold it."
      } else {
        sprintf("Formats that can: %s.", paste(formats, collapse = ", "))
      }
    ),
    "write",
    call = call,
    construct = construct,
    location = location,
    formats = formats
  )
}

engine_error <- function(engine, parent = NULL, call = sys.call(-1)) {
  zufapmx_abort(
    if (is.null(parent)) {
      sprintf("The %s engine is not installed.", engine)
    } else {
      sprintf("The %s engine failed: %s", engine, conditionMessage(parent))
    },
    "engine",
    call = call,
    engine = engine,
    parent = parent
  )
}
