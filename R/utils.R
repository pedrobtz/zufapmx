# Base R has `%||%` from 4.4.0; the package supports 4.1.
`%||%` <- function(x, y) if (is.null(x)) y else x

# `y` wherever `x` is NA: a lookup such as map[name] gives NA for a name
# the map lacks.
`%|NA|%` <- function(x, y) unname(ifelse(is.na(x), y, x))
