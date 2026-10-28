# Base R has `%||%` from 4.4.0; the package supports 4.1.
`%||%` <- function(x, y) if (is.null(x)) y else x
