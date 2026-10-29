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

# A parameter's value with its finite bounds: `value`, `c(lower, value)` or
# `c(lower, value, upper)`, the shapes NM-TRAN's $THETA and rxode2's ini()
# both use.
bounded_values <- function(lower, value, upper) {
  if (is.infinite(upper) && upper > 0) {
    if (is.infinite(lower) && lower < 0) value else c(lower, value)
  } else {
    c(lower, value, upper)
  }
}

# A closed form's ADVAN names its parameters (CL, V2, ...). `spelled` holds
# the target's spelling of `vars`; a variable holding one of these names for
# another purpose (rxode2's q2 is ADVAN11's Q3) gets a `_` suffix, and
# closed_alias_lines() assigns the name.
closed_free_names <- function(vars, spelled, structure) {
  if (!identical(structure$type, "closed")) {
    return(spelled)
  }
  required <- nm_closed[[as.character(structure$advan)]]$names
  owner <- unname(structure$parameters[names(required)])
  taken <- (spelled %in% required) & (vars != owner[match(spelled, required)])
  spelled[taken] <- paste0(spelled[taken], "_")
  spelled
}

# `NAME = var` lines, in `format`, for each parameter name the ADVAN needs
# that the model spells differently (a variable, or a parameter such as
# THETA(2)).
closed_alias_lines <- function(structure, spelling, format) {
  if (!identical(structure$type, "closed")) {
    return(character())
  }
  required <- nm_closed[[as.character(structure$advan)]]$names
  out <- character()
  for (canonical in names(required)) {
    spelled <- spelling[[structure$parameters[[canonical]]]]
    if (spelled != required[[canonical]]) {
      out <- c(out, sprintf(format, required[[canonical]], spelled))
    }
  }
  out
}

# What no writer can hold yet: inter-occasion variability, transformed
# endpoints, and (for NONMEM and mrgsolve) other than one endpoint.
check_writable <- function(m, one_endpoint = TRUE) {
  if (isTRUE(m$random_effects$iov$present)) {
    write_error(
      "inter-occasion variability",
      "random_effects$iov",
      formats = character(),
      call = NULL
    )
  }
  for (e in names(m$observations)) {
    if (!identical(m$observations[[e]]$transform, "none")) {
      write_error(
        sprintf("a %s-transformed endpoint", m$observations[[e]]$transform),
        sprintf("observations$%s", e),
        formats = character(),
        call = NULL
      )
    }
  }
  if (one_endpoint && length(m$observations) != 1) {
    write_error(
      if (length(m$observations) == 0) {
        "no endpoint"
      } else {
        "more than one endpoint"
      },
      "observations",
      formats = "rxode2",
      call = NULL
    )
  }
}

# Read a writer's own output back. Text that does not read back is the
# writer's failure, so it is a write error, not the reader's parse error.
read_back <- function(expr, format) {
  tryCatch(
    expr,
    # Only a refusal is a failed translation; any other error is a bug and
    # propagates as itself.
    zufapmx_parse_error = function(e) {
      write_error(
        sprintf(
          "a model whose %s text does not read back (%s)",
          format,
          conditionMessage(e)
        ),
        "text",
        formats = character(),
        call = NULL
      )
    }
  )
}
