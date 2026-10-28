# A writer's result: the text, the format, and what the translation kept
# (design §6). `refused` is never a value here; a refused write is an error.

fidelity_levels <- c("exact", "equivalent", "lossy")

new_written <- function(text, format, fidelity, losses) {
  structure(
    list(text = text, format = format, fidelity = fidelity, losses = losses),
    class = "zupkpmx_written"
  )
}

# Fidelity of a write, from the losses the writer recorded and whether its
# output reads back to the same representation.
written_fidelity <- function(losses, model, back) {
  if (length(losses) > 0) {
    return("lossy")
  }
  if (same_content(model_content(back), model_content(model))) {
    "exact"
  } else {
    "equivalent"
  }
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
#' stops with an error of class `zupkpmx_write_error` instead.
#'
#' @param written The result of a `pmx_write_*()` function.
#' @return `pmx_fidelity()`: a string. `pmx_losses()`: a character vector,
#'   empty when nothing was lost.
#' @export
#' @examples
#' corpus <- zupkpmx_corpus()
#' fun <- eval(parse(file.path(corpus$path[[1]], "model-rxode2.R")))
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

check_written <- function(x) {
  if (!inherits(x, "zupkpmx_written")) {
    zupkpmx_abort(
      sprintf("Expected a written model, not %s.", class(x)[[1]]),
      "model"
    )
  }
}

#' @export
format.zupkpmx_written <- function(x, ...) {
  x$text
}

#' @export
print.zupkpmx_written <- function(x, ...) {
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
as.character.zupkpmx_written <- function(x, ...) {
  x$text
}
