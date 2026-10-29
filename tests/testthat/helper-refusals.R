# A reader's refusal (design §8, D6): a parse error naming what it saw and,
# for a file, the line. A refusal of a statement points at its record's
# first line, since statements carry no positions; an rxode2 model is an R
# function, so its refusals have no line.
expect_refused <- function(object, seen, line = NULL) {
  err <- expect_error(object, class = "zufapmx_parse_error")
  expect_equal(err$seen, seen)
  if (!is.null(line)) {
    expect_equal(err$line, line)
  }
  invisible(err)
}
