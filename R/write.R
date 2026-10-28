# Helpers every writer shares: renaming symbols, printing numbers and
# statements, and indenting.

rename_symbols <- function(x, map) {
  if (is.symbol(x)) {
    name <- as.character(x)
    return(if (name %in% names(map)) as.symbol(map[[name]]) else x)
  }
  if (is.call(x)) {
    x[] <- lapply(as.list(x), rename_symbols, map = map)
  }
  x
}

expr_line <- function(x) {
  paste(expr_deparse(x), collapse = " ")
}

# Deparse with 15 significant digits unless a constant needs 17 to read
# back as the same number.
expr_deparse <- function(x) {
  numbers <- expr_numbers(x)
  exact <- all(vapply(
    numbers,
    function(n) identical(as.numeric(as.character(n)), n),
    logical(1)
  ))
  opts <- if (exact) "keepNA" else c("keepNA", "digits17")
  deparse(x, width.cutoff = 500L, control = opts)
}

expr_numbers <- function(x) {
  if (is.numeric(x)) {
    return(as.list(x))
  }
  if (is.call(x)) {
    return(unlist(lapply(as.list(x)[-1], expr_numbers), recursive = FALSE))
  }
  list()
}

# 15 significant digits when they read back within a few ulps, which the
# fidelity check allows (same_content()); 17 otherwise. 0.2^2 prints as
# 0.04, not 0.040000000000000008.
num_text <- function(x) {
  text <- as.character(x)
  if (same_content(as.numeric(text), x)) text else sprintf("%.17g", x)
}

indent <- function(lines, n) {
  paste0(strrep(" ", n), lines)
}
