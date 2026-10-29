# Helpers every writer shares: renaming symbols, printing expressions and
# numbers, indenting, closed-form names, the refusals no writer avoids
# (check_writable()) and their fields, and reading a writer's text back.

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

# The formats that hold a residual model a writer refuses: rxode2 alone
# writes a power model; an expression over standard deviations has no
# writer. (An expression over epsilons is NONMEM's, refused elsewhere.)
residual_formats <- function(type) {
  if (identical(type, "power")) "rxode2" else character()
}

closed_prediction_error <- function(endpoint) {
  write_error(
    "a closed-form prediction other than the central concentration",
    sprintf("observations$%s", endpoint),
    formats = character(),
    call = NULL
  )
}

model_title <- function(provenance) {
  if (identical(provenance$format, "nonmem")) {
    return(basename(provenance$file))
  }
  sprintf("zufapmx model read from %s", provenance$format)
}

# A variance in $SIGMA has no bounds, so bounds on a standard deviation
# other than the natural [0, Inf) are lost when it moves there.
sd_bound_losses <- function(m) {
  if (length(m$random_effects$sigma) > 0) {
    return(character())
  }
  sds <- unlist(lapply(m$observations, function(o) o$residual$parameters))
  p <- m$parameters[m$parameters$name %in% sds, ]
  bounded <- p$name[p$lower > 0 | is.finite(p$upper)]
  if (length(bounded) == 0) {
    return(character())
  }
  sprintf(
    "bounds of residual standard deviations: %s",
    paste(bounded, collapse = ", ")
  )
}

# Residual error held as standard deviations (as rxode2 holds it) moves to
# $SIGMA as variances: each sd parameter becomes one epsilon whose
# variance is its square, and leaves $THETA.
sigma_from_sd <- function(m) {
  if (length(m$random_effects$sigma) > 0) {
    return(m)
  }
  for (e in names(m$observations)) {
    r <- m$observations[[e]]$residual
    if (!r$type %in% c("additive", "proportional", "combined", "exponential")) {
      write_error(
        sprintf("the %s residual model", r$type),
        sprintf("observations$%s$residual", e),
        formats = residual_formats(r$type),
        call = NULL
      )
    }
    eps <- character()
    used <- unlist(lapply(
      c(m$individual, m$structure$statements, m$structure$derivatives),
      all.vars
    ))
    for (sd_name in r$parameters) {
      if (sd_name %in% used) {
        write_error(
          sprintf(
            "the residual parameter `%s`, which the model also uses",
            sd_name
          ),
          sprintf("observations$%s$residual", e),
          formats = "rxode2",
          call = NULL
        )
      }
      row <- m$parameters[m$parameters$name == sd_name, ]
      if (nrow(row) != 1) {
        write_error(
          sprintf(
            "the residual parameter `%s`, which is not a fixed effect",
            sd_name
          ),
          sprintf("observations$%s$residual", e),
          formats = character(),
          call = NULL
        )
      }
      name <- paste0("EPS", length(m$random_effects$sigma) + 1)
      block <- list(
        names = name,
        matrix = matrix(row$init^2, dimnames = list(name, name)),
        fixed = row$fixed
      )
      if (!is.na(row$estimate)) {
        block$estimate <- matrix(row$estimate^2, dimnames = list(name, name))
      }
      m$random_effects$sigma[[length(m$random_effects$sigma) + 1]] <- block
      m$parameters <- m$parameters[m$parameters$name != sd_name, ]
      eps <- c(eps, name)
    }
    m$observations[[e]]$residual$parameters <- eps
  }
  rownames(m$parameters) <- NULL
  m
}

index_names <- function(blocks, prefix) {
  symbols <- unlist(lapply(blocks, `[[`, "names"))
  stats::setNames(sprintf("%s(%d)", prefix, seq_along(symbols)), symbols)
}

# A whole number with a decimal point, so Fortran and C++ divide it as a
# real: 3/4 is 0 in both.
with_point <- function(text) {
  if (grepl("^-?[0-9]+$", text)) paste0(text, ".0") else text
}
