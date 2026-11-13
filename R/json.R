#' Write a model as JSON, and read it back
#'
#' `pmx_write_json()` writes the representation as a JSON document and
#' `pmx_read_json()` reads one back. The document holds the model's seven
#' components as data, so a model read from JSON is the model that was
#' written: the same parameters, statements, provenance and all. It is
#' zupmxio's own representation, not a file standard.
#'
#' Statements and expressions are R code in strings (`"cl <- tcl *
#' exp(eta.cl)"`), constants are numbers. JSON has no infinity, so
#' `Inf` and `-Inf` are the strings `"Inf"` and `"-Inf"`; a missing value
#' is `null`. Numbers are written in the fewest digits that read back
#' exactly.
#'
#' Both functions need the zujson package, and raise an error of class
#' `zupmxio_engine_error` without it.
#'
#' @inheritParams pmx_inspect
#' @param file For `pmx_write_json()`, the path to write, or `NULL` to only
#'   return the text. For `pmx_read_json()`, the path of a JSON file.
#' @param pretty Whether to indent the JSON.
#' @return `pmx_write_json()`: the JSON text, invisibly when `file` is
#'   given. `pmx_read_json()`: a model. A file that is not a zupmxio JSON
#'   model is a `zupmxio_parse_error`.
#' @family readers
#' @family writers
#' @export
#' @examplesIf requireNamespace("zujson", quietly = TRUE)
#' path <- system.file(
#'   "corpus", "onecmt-oral", "model.ctl",
#'   package = "zupmxio"
#' )
#' model <- pmx_read_nonmem(path)
#' file <- tempfile(fileext = ".json")
#' pmx_write_json(model, file)
#' identical(pmx_read_json(file), model)
pmx_write_json <- function(model, file = NULL, pretty = TRUE) {
  check_model(model)
  check_path_or_null(file)
  check_flag(pretty)
  json_require()
  text <- zujson::json_write(json_utf8(json_model(model)), pretty = pretty)
  if (is.null(file)) {
    return(text)
  }
  writeLines(text, file, useBytes = TRUE)
  invisible(text)
}

#' @rdname pmx_write_json
#' @export
pmx_read_json <- function(file) {
  check_file(file)
  json_require()
  x <- tryCatch(
    zujson::json_parse_file(file, simplify = FALSE),
    error = function(e) {
      json_error(file, sprintf("not JSON (%s)", conditionMessage(e)))
    }
  )
  # Any failure to build the model from the document is the document's:
  # indexing a component of the wrong JSON type raises base errors (#151).
  model <- tryCatch(
    json_unmodel(x, file),
    zupmxio_parse_error = function(e) stop(e),
    zupmxio_model_error = function(e) json_error(file, conditionMessage(e)),
    error = function(e) {
      json_error(
        file,
        sprintf("not a zupmxio model (%s)", conditionMessage(e))
      )
    }
  )
  model
}

json_version <- 1L

json_require <- function() {
  if (!engine_available("zujson")) {
    engine_error("zujson", call = NULL)
  }
}

json_error <- function(file, message) {
  parse_error(
    file,
    NA_integer_,
    NA_integer_,
    seen = "the JSON document",
    message = message,
    call = NULL
  )
}

# ---- writing ---------------------------------------------------------------

json_model <- function(model) {
  m <- unclass(model)
  re <- m$random_effects
  s <- m$structure
  list(
    zupmxio = "model",
    version = json_version,
    parameters = lapply(seq_len(nrow(m$parameters)), function(i) {
      p <- m$parameters[i, ]
      list(
        name = p$name,
        role = p$role,
        init = json_num(p$init),
        lower = json_num(p$lower),
        upper = json_num(p$upper),
        fixed = p$fixed,
        estimate = json_num(p$estimate)
      )
    }),
    random_effects = list(
      omega = lapply(re$omega, json_block),
      sigma = lapply(re$sigma, json_block),
      attach = lapply(seq_len(nrow(re$attach)), function(i) {
        as.list(re$attach[i, ])
      }),
      iov = list(present = re$iov$present, occasion = json_chr(re$iov$occasion))
    ),
    individual = lapply(m$individual, json_expr),
    structure = if (identical(s$type, "closed")) {
      list(
        type = s$type,
        ncmt = s$ncmt,
        depot = s$depot,
        parameters = as.list(s$parameters),
        advan = s$advan,
        trans = s$trans
      )
    } else {
      list(
        type = s$type,
        compartments = I(s$compartments),
        statements = lapply(s$statements, json_expr),
        derivatives = lapply(s$derivatives, json_expr)
      )
    },
    dosing = json_object(lapply(m$dosing, function(d) {
      json_object(lapply(Filter(Negate(is.null), d), json_expr))
    })),
    observations = json_object(lapply(m$observations, function(o) {
      r <- o$residual
      list(
        prediction = json_expr(o$prediction),
        residual = c(
          list(type = r$type, parameters = I(r$parameters)),
          if (!is.null(r$expression)) {
            list(expression = json_expr(r$expression))
          }
        ),
        transform = o$transform
      )
    })),
    provenance = lapply(m$provenance, function(v) {
      if (length(v) == 1) json_chr(v) else I(v)
    })
  )
}

# Every string and name marked UTF-8 where its bytes are UTF-8. Outside a
# UTF-8 locale R holds a non-ASCII name as unmarked bytes, which a writer
# would convert from the native encoding and mangle.
json_utf8 <- function(x) {
  mark <- function(v) {
    ok <- !is.na(v) & validUTF8(v)
    Encoding(v[ok]) <- "UTF-8"
    v
  }
  if (is.character(x)) {
    x <- mark(x)
  } else if (is.list(x)) {
    x[] <- lapply(x, json_utf8)
  }
  if (!is.null(names(x))) {
    names(x) <- mark(names(x))
  }
  x
}

# An empty named list writes as `{}`, not `[]`.
json_object <- function(x) {
  if (length(x) == 0) structure(list(), names = character()) else x
}

json_block <- function(b) {
  c(
    list(names = I(b$names), matrix = json_matrix(b$matrix)),
    if (!is.null(b$estimate)) list(estimate = json_matrix(b$estimate)),
    list(fixed = b$fixed)
  )
}

json_matrix <- function(m) {
  lapply(seq_len(nrow(m)), function(i) I(json_num(unname(m[i, ]))))
}

# Numbers JSON cannot hold become strings; NA becomes null.
json_num <- function(x) {
  out <- as.list(x)
  out[is.na(x)] <- list(NULL)
  out[is.infinite(x) & x > 0] <- "Inf"
  out[is.infinite(x) & x < 0] <- "-Inf"
  out[is.nan(x)] <- "NaN"
  if (length(x) == 1) out[[1]] else out
}

json_chr <- function(x) if (is.na(x)) NULL else x

# A constant is a number; anything else is R code in a string, in the
# fewest digits that parse back to the same expression.
json_expr <- function(x) {
  if (is.numeric(x) && length(x) == 1 && is.null(attributes(x))) {
    return(json_num(x))
  }
  # deparse() escapes a non-ASCII name as bytes outside a UTF-8 locale, so
  # such names go through ASCII placeholders and come back backquoted.
  names <- all.names(x)
  wide <- unique(names[!grepl("^[ -~]*$", names)])
  placeholders <- sprintf("zupmxio_name_%d_", seq_along(wide))
  y <- rename_symbols(x, stats::setNames(placeholders, wide))
  print_text <- function(control) {
    text <- paste(
      deparse(y, width.cutoff = 500L, control = control),
      collapse = "\n"
    )
    for (i in seq_along(wide)) {
      text <- gsub(
        placeholders[[i]],
        paste0("`", wide[[i]], "`"),
        text,
        fixed = TRUE
      )
    }
    # The names' own bytes, marked UTF-8 when they are: converting would
    # read a C locale's bytes as Latin-1.
    if (validUTF8(text)) {
      Encoding(text) <- "UTF-8"
    }
    text
  }
  text <- print_text(c("keepNA", "keepInteger", "niceNames", "showAttributes"))
  # Exactly: same_content() allows a few ulps, and a constant such as
  # 0.30000000000000004 prints as 0.3 in 15 digits.
  if (!identical(str2lang(text), x)) {
    text <- print_text(c("keepNA", "keepInteger", "niceNames", "digits17"))
  }
  text
}

# ---- reading ---------------------------------------------------------------

json_unmodel <- function(x, file) {
  x <- json_native_all(x)
  if (!is.list(x) || !identical(x$zupmxio, "model")) {
    json_error(file, "not a zupmxio model (no `\"zupmxio\": \"model\"`)")
  }
  version <- tryCatch(json_int(x$version), zupmxio_model_error = function(e) NA)
  if (!identical(version, json_version)) {
    json_error(
      file,
      sprintf(
        "a zupmxio model of version %s; this zupmxio reads version %d",
        format(json_scalar(x$version)),
        json_version
      )
    )
  }
  p <- x$parameters
  parameters <- data.frame(
    name = json_chr_col(p, "name"),
    role = json_chr_col(p, "role"),
    init = json_num_col(p, "init"),
    lower = json_num_col(p, "lower"),
    upper = json_num_col(p, "upper"),
    fixed = json_lgl_col(p, "fixed"),
    estimate = json_num_col(p, "estimate")
  )
  re <- x$random_effects
  attach <- re$attach
  structure_x <- x$structure
  type <- json_string(structure_x$type)
  structure <- if (identical(type, "closed")) {
    list(
      type = type,
      ncmt = json_int(structure_x$ncmt),
      depot = json_logical(structure_x$depot),
      parameters = unlist(lapply(structure_x$parameters, json_string)) %||%
        stats::setNames(character(), character()),
      advan = json_int(structure_x$advan),
      trans = json_int(structure_x$trans)
    )
  } else {
    list(
      type = type,
      compartments = json_strings(structure_x$compartments),
      statements = lapply(structure_x$statements, json_unexpr),
      derivatives = lapply(structure_x$derivatives, json_unexpr)
    )
  }
  # Readers build an empty list() when a model has no endpoint.
  observations <- if (length(x$observations) == 0) {
    list()
  } else {
    lapply(x$observations, function(o) {
      r <- o$residual
      list(
        prediction = json_unexpr(o$prediction),
        residual = c(
          list(
            type = json_string(r$type),
            parameters = json_strings(r$parameters)
          ),
          if (!is.null(r$expression)) {
            list(expression = json_unexpr(r$expression))
          }
        ),
        transform = json_string(o$transform)
      )
    })
  }
  provenance <- lapply(x$provenance, function(v) {
    if (is.null(v)) NA_character_ else json_strings(v)
  })
  for (field in c("losses", "covariates")) {
    if (field %in% names(provenance) && length(provenance[[field]]) == 0) {
      provenance[[field]] <- character()
    }
  }
  new_model(
    parameters = parameters,
    random_effects = list(
      omega = lapply(re$omega, json_unblock),
      sigma = lapply(re$sigma, json_unblock),
      attach = data.frame(
        eta = json_chr_col(attach, "eta"),
        parameter = json_chr_col(attach, "parameter"),
        transform = json_chr_col(attach, "transform")
      ),
      iov = list(
        present = json_logical(re$iov$present),
        occasion = if (is.null(re$iov$occasion)) {
          NA_character_
        } else {
          json_string(re$iov$occasion)
        }
      )
    ),
    individual = lapply(x$individual, json_unexpr),
    structure = structure,
    # Readers build dosing as an unnamed list() when there is none.
    dosing = if (length(x$dosing) == 0) {
      list()
    } else {
      lapply(x$dosing, function(d) lapply(d, json_unexpr))
    },
    observations = observations,
    provenance = provenance
  )
}

json_unblock <- function(b) {
  names <- json_strings(b$names)
  # The readers' order: names, matrix, fixed, then any estimate.
  out <- list(
    names = names,
    matrix = json_unmatrix(b$matrix, names),
    fixed = json_logical(b$fixed)
  )
  if (!is.null(b$estimate)) {
    out$estimate <- json_unmatrix(b$estimate, names)
  }
  out
}

json_unmatrix <- function(rows, names) {
  values <- lapply(rows, json_nums)
  n <- length(names)
  if (length(values) != n || any(lengths(values) != n)) {
    model_abort(
      "The block for %s must be a %d by %d matrix.",
      paste(names, collapse = ", "),
      n,
      n
    )
  }
  matrix(
    unlist(values, use.names = FALSE),
    n,
    n,
    byrow = TRUE,
    dimnames = list(names, names)
  )
}

json_unexpr <- function(x) {
  if (
    is.null(x) || is.numeric(x) || identical(x, "Inf") || identical(x, "-Inf")
  ) {
    return(json_nums(list(x)))
  }
  # Bytes as they are (json_native()): outside a UTF-8 locale the parser
  # would turn a UTF-8 name into `<U+00EF>`.
  text <- json_string(x)
  out <- tryCatch(
    str2lang(text),
    error = function(e) model_abort("`%s` is not an R expression.", text)
  )
  out
}

json_scalar <- function(x) {
  if (is.list(x) || length(x) != 1) {
    model_abort("Expected a single value, not %s.", json_describe(x))
  }
  x
}

# A whole number in R's integer range, never truncated or coerced to NA.
json_int <- function(x) {
  x <- json_scalar(x)
  if (
    !is.numeric(x) ||
      !is.finite(x) ||
      x != round(x) ||
      abs(x) > .Machine$integer.max
  ) {
    model_abort("Expected a whole number, not %s.", json_describe(x))
  }
  as.integer(x)
}

json_string <- function(x) {
  x <- json_scalar(x)
  if (!is.character(x)) {
    model_abort("Expected a string, not %s.", json_describe(x))
  }
  json_native(x)
}

# json_native() on every string and name of a parsed document: object keys
# become list names (endpoints, compartments), which name symbols too.
json_native_all <- function(x) {
  if (is.character(x)) {
    x <- json_native(x)
  } else if (is.list(x)) {
    x[] <- lapply(x, json_native_all)
  }
  if (!is.null(names(x))) {
    names(x) <- json_native(names(x))
  }
  x
}

# Outside a UTF-8 locale, R holds a non-ASCII name as its bytes; reading
# the JSON's UTF-8 as those bytes gives back the name that was written.
json_native <- function(x) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    Encoding(x) <- "unknown"
  }
  x
}

json_strings <- function(x) {
  if (length(x) == 0) {
    return(character())
  }
  vapply(x, json_string, character(1), USE.NAMES = FALSE)
}

json_logical <- function(x) {
  x <- json_scalar(x)
  if (!is.logical(x) || is.na(x)) {
    model_abort("Expected true or false, not %s.", json_describe(x))
  }
  x
}

json_nums <- function(x) {
  vapply(
    x,
    function(v) {
      if (is.null(v)) {
        return(NA_real_)
      }
      if (is.character(v) && length(v) == 1) {
        out <- c(`Inf` = Inf, `-Inf` = -Inf, `NaN` = NaN)[v]
        if (is.na(out) && !identical(v, "NaN")) {
          model_abort("Expected a number, not \"%s\".", v)
        }
        return(unname(out))
      }
      if (!is.numeric(v) || length(v) != 1) {
        model_abort("Expected a number, not %s.", json_describe(v))
      }
      as.numeric(v)
    },
    numeric(1),
    USE.NAMES = FALSE
  )
}

json_col <- function(rows, field) lapply(rows, function(r) r[[field]])

json_chr_col <- function(rows, field) {
  json_strings(json_col(rows, field))
}

json_num_col <- function(rows, field) {
  json_nums(json_col(rows, field))
}

json_lgl_col <- function(rows, field) {
  vapply(json_col(rows, field), json_logical, logical(1), USE.NAMES = FALSE)
}

json_describe <- function(x) {
  if (is.null(x)) {
    "null"
  } else if (is.list(x)) {
    sprintf("an array or object of %d", length(x))
  } else {
    sprintf("`%s`", paste(format(x), collapse = ", "))
  }
}
