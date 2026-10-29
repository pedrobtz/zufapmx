# A writer's result: the text, the format, and what the translation kept
# (design §6). `refused` is never a value here; a refused write is an error.

new_written <- function(text, format, fidelity, losses) {
  structure(
    list(text = text, format = format, fidelity = fidelity, losses = losses),
    class = "zufapmx_written"
  )
}

#' Fidelity and losses of a written model
#'
#' Every `pmx_write_*()` function returns the model text together with how
#' faithful it is. `pmx_fidelity()` is one of:
#'
#' * `"exact"`: the written model reads back to the same representation.
#' * `"equivalent"`: a reparameterisation that simulates identically.
#' * `"lossy"`: something was dropped that does not change predictions,
#'   such as estimation settings, tables or comments; `pmx_losses()` names
#'   each item.
#'
#' A construct that would change predictions is never dropped: the writer
#' stops with an error of class `zufapmx_write_error` instead.
#'
#' @param written The result of a `pmx_write_*()` function.
#' @return `pmx_fidelity()`: a string. `pmx_losses()`: a character vector,
#'   empty when nothing was lost.
#' @family writers
#' @export
#' @examples
#' path <- system.file(
#'   "corpus", "onecmt-oral", "model-rxode2.R",
#'   package = "zufapmx"
#' )
#' fun <- eval(parse(path))
#' written <- pmx_write_rxode2(pmx_read_rxode2(fun))
#' pmx_fidelity(written)
#' pmx_losses(written)
pmx_fidelity <- function(written) {
  check_written(written)
  written$fidelity
}

#' @rdname pmx_fidelity
#' @export
pmx_losses <- function(written) {
  check_written(written)
  written$losses
}

#' @export
format.zufapmx_written <- function(x, ...) {
  x$text
}

#' @export
print.zufapmx_written <- function(x, ...) {
  cat(x$text, sep = "\n")
  cat(sprintf(
    "# %s, fidelity: %s%s\n",
    x$format,
    x$fidelity,
    if (length(x$losses) > 0) {
      paste0(" (lost: ", paste(x$losses, collapse = "; "), ")")
    } else {
      ""
    }
  ))
  invisible(x)
}

#' @export
as.character.zufapmx_written <- function(x, ...) {
  x$text
}

# Fidelity of a write, from the losses the writer recorded and whether its
# output reads back to the same representation.
written_fidelity <- function(
  losses,
  model,
  back,
  values = "final",
  fixed_blocks = TRUE
) {
  # What the writer meant to write: estimates in place of initial values
  # (or not), and blocks without their fixed flags where the format cannot
  # hold them. Declared losses are applied to the source before comparing,
  # so a lossy write is still checked (design D9).
  intended <- as_written(model, values, fixed_blocks)
  if (same_content(model_content(back), model_content(intended))) {
    return(if (length(losses) > 0) "lossy" else "exact")
  }
  # Equivalent only through the reparameterisations writers are known to
  # make (normalise_model()); anything else is a translation error.
  a <- normalise_model(intended)
  b <- normalise_model(back)
  if (same_content(a, b)) {
    return(if (length(losses) > 0) "lossy" else "equivalent")
  }
  differs <- names(a)[
    !vapply(names(a), function(n) same_content(a[[n]], b[[n]]), logical(1))
  ]
  write_error(
    "a translation that does not read back as the same model",
    differs[[1]],
    formats = character(),
    call = NULL
  )
}

# A model as a writer writes it: with `values = "final"` its estimates
# become its initial values; with `"initial"` its estimates are dropped.
# `fixed_blocks = FALSE` drops the fixed flags of omega and sigma blocks.
as_written <- function(model, values, fixed_blocks) {
  m <- unclass(model)
  p <- m$parameters
  if (values == "final") {
    has <- !is.na(p$estimate)
    p$init[has] <- p$estimate[has]
  }
  p$estimate <- NA_real_
  m$parameters <- p
  block <- function(b) {
    if (values == "final" && !is.null(b$estimate)) {
      b$matrix <- b$estimate
    }
    b$estimate <- NULL
    if (!fixed_blocks) {
      b$fixed <- FALSE
    }
    b
  }
  m$random_effects$omega <- lapply(m$random_effects$omega, block)
  m$random_effects$sigma <- lapply(m$random_effects$sigma, block)
  structure(m, class = class(model))
}

# A model with the reparameterisations writers make undone, for comparing
# a translation with its source: residual error as sigma variances,
# aliases (`V2 <- V`) substituted away, parentheses dropped, and every name
# replaced by its position (P1 for the first parameter, E1, S1, C1 for the
# first eta, epsilon and compartment, V1 for the first variable assigned,
# Y1 for the first endpoint). Covariates compare case-insensitively, since
# NONMEM upper-cases them. Roles and eta attachment are derived from the
# statements, so they are left out.
normalise_model <- function(model) {
  m <- unclass(model)
  # A model whose residual error has no sigma form (an SD parameter used
  # elsewhere) is compared as it stands; both sides then keep their SDs.
  m <- tryCatch(sigma_from_sd(m), zufapmx_write_error = function(e) m)
  m <- drop_aliases(m)

  closed <- identical(m$structure$type, "closed")
  compartments <- model_compartments(m$structure)
  etas <- unlist(lapply(m$random_effects$omega, `[[`, "names"))
  eps <- unlist(lapply(m$random_effects$sigma, `[[`, "names"))
  vars <- unique(c(
    stmt_targets(m$individual),
    stmt_targets(m$structure$statements)
  ))
  map <- c(
    stats::setNames(
      paste0("P", seq_len(nrow(m$parameters))),
      m$parameters$name
    ),
    stats::setNames(paste0("E", seq_along(etas)), etas),
    stats::setNames(paste0("S", seq_along(eps)), eps),
    stats::setNames(paste0("C", seq_along(compartments)), compartments),
    stats::setNames(paste0("V", seq_along(vars)), vars)
  )
  covariates <- setdiff(
    unique(unlist(lapply(model_expressions(m), all.vars))),
    c(names(map), "t", "F")
  )
  folded <- toupper(gsub(".", "_", covariates, fixed = TRUE))
  if (anyDuplicated(folded)) {
    # `wt` and `WT` are two covariates here: folding would merge them, so a
    # writer that does must fail the comparison.
    folded <- covariates
  }
  map <- c(map, stats::setNames(folded, covariates))
  rename <- function(x) strip_parens(rename_symbols(x, map))
  block <- function(b) {
    b$names <- unname(map[b$names])
    dimnames(b$matrix) <- list(b$names, b$names)
    if (!is.null(b$estimate)) {
      dimnames(b$estimate) <- list(b$names, b$names)
    }
    b
  }

  p <- m$parameters
  p$name <- unname(map[p$name])
  p$role <- NULL
  rownames(p) <- NULL
  s <- m$structure
  if (closed) {
    s$parameters <- vapply(s$parameters, function(v) map[[v]], character(1))
    s$parameters <- s$parameters[sort(names(s$parameters))]
  } else {
    s$compartments <- unname(map[s$compartments])
    s$statements <- lapply(s$statements, rename)
    s$derivatives <- stats::setNames(
      lapply(s$derivatives, rename),
      unname(map[names(s$derivatives)])
    )
  }
  observations <- lapply(m$observations, function(o) {
    o$prediction <- rename(o$prediction)
    o$residual$parameters <- unname(
      map[o$residual$parameters] %|NA|% o$residual$parameters
    )
    if (!is.null(o$residual$expression)) {
      o$residual$expression <- rename(o$residual$expression)
    }
    o
  })
  list(
    parameters = p,
    omega = lapply(m$random_effects$omega, block),
    sigma = lapply(m$random_effects$sigma, block),
    iov = m$random_effects$iov,
    individual = lapply(m$individual, rename),
    structure = s,
    dosing = stats::setNames(
      lapply(m$dosing, function(d) lapply(d, rename)),
      unname(map[names(m$dosing)])
    ),
    observations = stats::setNames(
      observations,
      paste0("Y", seq_along(observations))
    )
  )
}

# Substitute aliases away: a top-level `A <- B`, B a name, makes A another
# name for B in every later expression. Writers add such statements for
# names a target requires (an ADVAN's V2, a renamed variable). Neither
# name may be assigned again: after `A <- B; B <- 3`, A is not B.
drop_aliases <- function(m) {
  stmts <- c(m$individual, m$structure$statements)
  map <- character()
  keep <- list()
  for (i in seq_along(m$individual)) {
    x <- m$individual[[i]]
    later <- stmt_targets(stmts[-seq_len(i)])
    if (
      is_call(x, "<-") &&
        is.symbol(x[[3]]) &&
        !any(c(as.character(x[[2]]), as.character(x[[3]])) %in% later)
    ) {
      map[[as.character(x[[2]])]] <- map[as.character(x[[3]])] %|NA|%
        as.character(x[[3]])
      next
    }
    keep[[length(keep) + 1]] <- rename_symbols(x, map)
  }
  if (length(map) == 0) {
    return(m)
  }
  m$individual <- keep
  rename <- function(x) rename_symbols(x, map)
  s <- m$structure
  if (identical(s$type, "closed")) {
    s$parameters <- vapply(
      s$parameters,
      function(v) map[v] %|NA|% v,
      character(1)
    )
  } else {
    s$statements <- lapply(s$statements, rename)
    s$derivatives <- lapply(s$derivatives, rename)
  }
  m$structure <- s
  m$dosing <- lapply(m$dosing, function(d) lapply(d, rename))
  m$observations <- lapply(m$observations, function(o) {
    o$prediction <- rename(o$prediction)
    if (!is.null(o$residual$expression)) {
      o$residual$expression <- rename(o$residual$expression)
    }
    o
  })
  m
}

# Identical, except that numbers may differ by a few units in the last
# place: without long double (macOS arm64, CRAN's NOLD flavor) R's decimal
# parser is not correctly rounded, so a written constant can read back one
# ulp away from the value that was written.
same_content <- function(a, b) {
  if (is.double(a) && is.double(b)) {
    return(
      identical(attributes(a), attributes(b)) &&
        identical(is.na(a), is.na(b)) &&
        all(
          a == b | abs(a - b) <= 4 * .Machine$double.eps * pmax(abs(a), abs(b)),
          na.rm = TRUE
        )
    )
  }
  if (is.list(a) || is.language(a)) {
    if (
      !identical(typeof(a), typeof(b)) ||
        length(a) != length(b) ||
        !identical(names(a), names(b))
    ) {
      return(FALSE)
    }
    if (is.symbol(a)) {
      return(identical(a, b))
    }
    if (
      !identical(
        attributes(a)[setdiff(names(attributes(a)), "names")],
        attributes(b)[setdiff(names(attributes(b)), "names")]
      )
    ) {
      return(FALSE)
    }
    return(all(mapply(same_content, as.list(a), as.list(b))))
  }
  identical(a, b)
}

# Everything but provenance, which records where a model came from rather
# than what it is.
model_content <- function(model) {
  unclass(model)[setdiff(model_components, "provenance")]
}

check_written <- function(x) {
  if (!inherits(x, "zufapmx_written")) {
    zufapmx_abort(
      sprintf("Expected a written model, not %s.", class(x)[[1]]),
      "model"
    )
  }
}
