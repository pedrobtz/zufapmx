# Base R has `%||%` from 4.4.0; the package supports 4.1.
`%||%` <- function(x, y) if (is.null(x)) y else x

# `x`, or `y` where `x` is NA, as a missing name subscripts to.
`%|NA|%` <- function(x, y) unname(ifelse(is.na(x), y, x))
