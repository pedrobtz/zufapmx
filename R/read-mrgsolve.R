#' Read an mrgsolve model specification
#'
#' Reads an mrgsolve model file (`.cpp`) into the package's representation.
#'
#' The reader covers `$PROB`, `$PARAM` (plain or `@annotated`), `$THETA`,
#' `$CMT`, `$INIT`, `$MAIN`/`$PK`, `$ODE`/`$DES`, `$TABLE`/`$ERROR`,
#' `$CAPTURE`, `$PKMODEL`, `$OMEGA` and `$SIGMA` (with `@block`), and
#' `$NMXML`/`$NMEXT` as sources of NONMEM estimates. Blocks may also be
#' written `[ NAME ]`. Code is the C++ subset mrgsolve documents for models:
#' declarations and assignments, `if`/`else`, arithmetic, comparisons,
#' `exp()`, `log()`, `log10()`, `sqrt()`, `fabs()`, `pow()`, `fmin()`,
#' `fmax()`, `ETA(n)`, `EPS(n)`, and the macros `dxdt_CMT`, `F_CMT`,
#' `ALAG_CMT`, `R_CMT`, `D_CMT` and `CMT_0`. Anything else, including
#' `$GLOBAL`, `$PREAMBLE` and the `nm-vars` plugin, is refused with an error
#' of class `zupkpmx_parse_error`.
#'
#' The observation is `Y` in `$TABLE`, written over a prediction such as
#' `IPRED`; its residual model is recognised as for NONMEM. An `@annotated`
#' `$PARAM` description of the form `[lower, upper]`, optionally followed
#' by `fixed`, gives a parameter's bounds, as [pmx_write_mrgsolve()] writes
#' them.
#'
#' @param file Path to the model file.
#' @return A `zupkpmx_model`.
#' @export
#' @examples
#' corpus <- zupkpmx_corpus()
#' pmx_read_mrgsolve(file.path(corpus$path[[4]], "model.cpp"))
pmx_read_mrgsolve <- function(file) {
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    zupkpmx_abort("`file` must be the path of an existing file.", "model")
  }
  mrg_read(file)
}

mrg_blocks_known <- c(
  "PROB",
  "PARAM",
  "THETA",
  "CMT",
  "INIT",
  "MAIN",
  "PK",
  "ODE",
  "DES",
  "TABLE",
  "ERROR",
  "CAPTURE",
  "PKMODEL",
  "OMEGA",
  "SIGMA",
  "NMXML",
  "NMEXT",
  "PLUGIN",
  "SET"
)

mrg_split <- function(lines, file) {
  lines <- mrg_strip_comments(lines)
  header <- "^\\s*(\\$([A-Za-z]+)|\\[\\s*([A-Za-z]+)\\s*\\])(.*)$"
  starts <- grep(header, lines)
  if (
    length(starts) == 0 || any(nzchar(trimws(lines[seq_len(starts[[1]] - 1)])))
  ) {
    parse_error(
      file,
      1L,
      1L,
      seen = trimws(lines[[1]]),
      expected = "a $BLOCK",
      call = NULL
    )
  }
  ends <- c(starts[-1] - 1L, length(lines))
  lapply(seq_along(starts), function(i) {
    m <- regmatches(
      lines[[starts[[i]]]],
      regexec(header, lines[[starts[[i]]]])
    )[[1]]
    name <- toupper(if (nzchar(m[[3]])) m[[3]] else m[[4]])
    if (!name %in% mrg_blocks_known) {
      parse_error(
        file,
        starts[[i]],
        1L,
        seen = paste0("$", name),
        message = sprintf("not translated: $%s at line %d", name, starts[[i]]),
        call = NULL
      )
    }
    at <- starts[[i]]:ends[[i]]
    text <- lines[at]
    text[[1]] <- m[[5]]
    options <- regmatches(text, gregexpr("@[A-Za-z_]+", text))
    list(
      name = name,
      lines = gsub("@[A-Za-z_]+", " ", text),
      line = at,
      options = unique(unlist(options))
    )
  })
}

# Remove // and /* */ comments, keeping line numbers.
mrg_strip_comments <- function(lines) {
  text <- paste(lines, collapse = "\n")
  text <- gsub("/\\*.*?\\*/", "", text, perl = TRUE)
  out <- strsplit(text, "\n", fixed = TRUE)[[1]]
  out <- c(out, rep("", length(lines) - length(out)))
  sub("//.*$", "", out)
}

mrg_read <- function(file) {
  blocks <- mrg_split(readLines(file, warn = FALSE), file)
  names <- vapply(blocks, `[[`, character(1), "name")
  block <- function(...) blocks[names %in% c(...)]
  losses <- character()
  if ("PROB" %in% names) {
    losses <- c(losses, "title ($PROB)")
  }
  if ("SET" %in% names) {
    losses <- c(losses, "simulation settings ($SET)")
  }
  for (b in block("PLUGIN")) {
    plugins <- strsplit(
      trimws(paste(b$lines, collapse = " ")),
      "[[:space:],]+"
    )[[1]]
    if ("nm-vars" %in% plugins) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = "nm-vars",
        message = sprintf(
          "not translated: the nm-vars plugin at line %d",
          b$line[[1]]
        ),
        call = NULL
      )
    }
    losses <- c(losses, "plugins ($PLUGIN)")
  }

  params <- mrg_params(block("PARAM"), file)
  thetas <- mrg_thetas(block("THETA"), file)
  omega <- mrg_matrix_blocks(block("OMEGA"), "ETA", file)
  sigma <- mrg_matrix_blocks(block("SIGMA"), "EPS", file)
  nm <- mrg_nonmem(block("NMXML", "NMEXT"), file)
  if (!is.null(nm)) {
    thetas <- rbind(thetas, nm$thetas)
    omega <- c(omega, nm$omega)
    sigma <- c(sigma, nm$sigma)
  }
  parameters <- rbind(params$parameters, thetas)
  losses <- c(losses, params$losses)

  pkmodel <- block("PKMODEL")
  cmt_names <- unlist(lapply(block("CMT"), mrg_cmt_names))
  closed <- NULL
  if (length(pkmodel) > 0) {
    closed <- mrg_pkmodel(pkmodel[[1]], file)
    if (length(closed$cmt) == 0) {
      closed$cmt <- cmt_names
    }
    canonical <- closed_compartments(closed$structure)
    if (length(closed$cmt) != length(canonical)) {
      parse_error(
        file,
        pkmodel[[1]]$line[[1]],
        1L,
        seen = paste(closed$cmt, collapse = " "),
        expected = sprintf("%d compartments", length(canonical)),
        call = NULL
      )
    }
    rename <- stats::setNames(canonical, closed$cmt)
    compartments <- canonical
  } else {
    if (length(cmt_names) == 0) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = "end of file",
        expected = "$CMT or $PKMODEL",
        call = NULL
      )
    }
    rename <- character()
    compartments <- cmt_names
  }
  code <- function(...) {
    stmts <- unlist(
      lapply(block(...), mrg_code, file = file),
      recursive = FALSE
    )
    lapply(stmts, rename_symbols, map = rename)
  }

  main <- mrg_main(code("MAIN", "PK"), compartments, file)
  init <- mrg_init(block("INIT"), file)
  for (cmt in names(init)) {
    target <- rename[cmt] %|NA|% cmt
    main$dosing[[target]] <- c(
      main$dosing[[target]],
      list(initial = init[[cmt]])
    )
  }
  if (!is.null(closed)) {
    structure <- nm_closed_structure(
      list(advan = closed$structure$advan, trans = closed$structure$trans),
      nm_closed[[as.character(closed$structure$advan)]],
      main$individual,
      file
    )
  } else {
    structure <- mrg_ode(code("ODE", "DES"), compartments, file)
  }
  table <- mrg_table(code("TABLE", "ERROR"), block("CAPTURE"), file)
  losses <- c(losses, table$losses)

  parameters$role <- infer_roles(parameters$name, main$individual, structure)
  new_model(
    parameters = parameters,
    random_effects = list(
      omega = omega,
      sigma = sigma,
      attach = attach_etas(
        unlist(lapply(omega, `[[`, "names")),
        main$individual
      ),
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = main$individual,
    structure = structure,
    dosing = main$dosing[intersect(compartments, names(main$dosing))],
    observations = list(
      Y = list(
        prediction = table$prediction,
        residual = table$residual,
        transform = "none"
      )
    ),
    provenance = list(
      format = "mrgsolve",
      file = normalizePath(file),
      estimates = if (is.null(nm)) NA_character_ else nm$path,
      hash = unname(tools::md5sum(file)),
      fidelity = if (length(losses) == 0) "exact" else "lossy",
      losses = unique(losses)
    )
  )
}

# $PARAM: `name = value` items, or `@annotated` lines `name : value : text`.
mrg_params <- function(blocks, file) {
  rows <- list()
  losses <- character()
  for (b in blocks) {
    if ("@annotated" %in% b$options) {
      for (k in seq_along(b$lines)) {
        line <- trimws(b$lines[[k]])
        if (!nzchar(line)) {
          next
        }
        parts <- trimws(strsplit(line, ":", fixed = TRUE)[[1]])
        if (length(parts) < 2) {
          parse_error(
            file,
            b$line[[k]],
            1L,
            seen = line,
            expected = "name : value : description",
            call = NULL
          )
        }
        text <- paste(parts[-(1:2)], collapse = ":")
        bounds <- regmatches(
          text,
          regexec("\\[\\s*([^,]+),\\s*([^]]+)\\]", text)
        )[[1]]
        if (nzchar(text) && length(bounds) == 0 && !grepl("^fixed$", text)) {
          losses <- c(losses, "parameter descriptions")
        }
        rows[[length(rows) + 1]] <- data.frame(
          name = parts[[1]],
          role = "other",
          init = mrg_number(parts[[2]], file, b$line[[k]]),
          lower = if (length(bounds) == 3) {
            mrg_number(bounds[[2]], file, b$line[[k]])
          } else {
            -Inf
          },
          upper = if (length(bounds) == 3) {
            mrg_number(bounds[[3]], file, b$line[[k]])
          } else {
            Inf
          },
          fixed = grepl("\\bfixed\\b", text),
          estimate = NA_real_
        )
      }
    } else {
      items <- mrg_assignments(b, file)
      for (name in names(items)) {
        rows[[length(rows) + 1]] <- data.frame(
          name = name,
          role = "other",
          init = items[[name]],
          lower = -Inf,
          upper = Inf,
          fixed = FALSE,
          estimate = NA_real_
        )
      }
    }
  }
  list(
    parameters = if (length(rows) == 0) {
      model_parameters_empty()
    } else {
      do.call(rbind, rows)
    },
    losses = unique(losses)
  )
}

# `name = value` pairs separated by commas or new lines, values numeric.
mrg_assignments <- function(b, file) {
  text <- paste(b$lines, collapse = ",")
  items <- trimws(strsplit(text, ",", fixed = TRUE)[[1]])
  items <- items[nzchar(items)]
  out <- list()
  for (item in items) {
    parts <- trimws(strsplit(item, "=", fixed = TRUE)[[1]])
    if (length(parts) != 2 || !grepl("^[A-Za-z_][A-Za-z0-9_]*$", parts[[1]])) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = item,
        expected = "name = value",
        call = NULL
      )
    }
    out[[parts[[1]]]] <- mrg_number(parts[[2]], file, b$line[[1]])
  }
  out
}

# A number, or a constant expression of the function table (`log(2)`).
mrg_number <- function(text, file, line) {
  text <- trimws(text)
  if (text %in% c("Inf", "-Inf")) {
    return(if (text == "Inf") Inf else -Inf)
  }
  x <- tryCatch(str2lang(text), error = function(e) NULL)
  ok <- !is.null(x) &&
    length(all.vars(x)) == 0 &&
    tryCatch(
      {
        expr_check(x)
        TRUE
      },
      zupkpmx_model_error = function(e) FALSE
    )
  if (!ok) {
    parse_error(
      file,
      line,
      NA_integer_,
      seen = text,
      expected = "a number",
      call = NULL
    )
  }
  as.numeric(eval(x, baseenv()))
}

mrg_thetas <- function(blocks, file) {
  values <- unlist(lapply(blocks, function(b) mrg_values(b, file)))
  if (length(values) == 0) {
    return(model_parameters_empty())
  }
  data.frame(
    name = paste0("THETA", seq_along(values)),
    role = "other",
    init = values,
    lower = -Inf,
    upper = Inf,
    fixed = FALSE,
    estimate = NA_real_
  )
}

mrg_values <- function(b, file) {
  tokens <- unlist(strsplit(
    trimws(paste(b$lines, collapse = " ")),
    "[[:space:],]+"
  ))
  tokens <- tokens[nzchar(tokens)]
  vapply(
    tokens,
    mrg_number,
    numeric(1),
    file = file,
    line = b$line[[1]],
    USE.NAMES = FALSE
  )
}

mrg_matrix_blocks <- function(blocks, prefix, file) {
  out <- list()
  for (b in blocks) {
    bad <- intersect(b$options, c("@correlation", "@cor", "@cholesky"))
    if (length(bad) > 0) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = bad[[1]],
        message = sprintf(
          "not translated: %s in $%s at line %d",
          bad[[1]],
          b$name,
          b$line[[1]]
        ),
        call = NULL
      )
    }
    values <- mrg_values(b, file)
    taken <- length(unlist(lapply(out, `[[`, "names")))
    if ("@block" %in% b$options) {
      n <- (sqrt(8 * length(values) + 1) - 1) / 2
      if (n != round(n)) {
        parse_error(
          file,
          b$line[[1]],
          1L,
          seen = sprintf("%d values", length(values)),
          expected = "a lower triangle",
          call = NULL
        )
      }
      names <- paste0(prefix, taken + seq_len(n))
      m <- lower_to_full(values)
      dimnames(m) <- list(names, names)
      out[[length(out) + 1]] <- list(names = names, matrix = m, fixed = FALSE)
    } else {
      for (i in seq_along(values)) {
        name <- paste0(prefix, taken + i)
        out[[length(out) + 1]] <- list(
          names = name,
          matrix = matrix(values[[i]], dimnames = list(name, name)),
          fixed = FALSE
        )
      }
    }
  }
  out
}

mrg_cmt_names <- function(b) {
  lines <- trimws(b$lines)
  lines <- lines[nzchar(lines)]
  if ("@annotated" %in% b$options) {
    return(trimws(sub(":.*$", "", lines)))
  }
  unlist(strsplit(lines, "[[:space:],]+"))
}

mrg_init <- function(blocks, file) {
  out <- list()
  for (b in blocks) {
    items <- mrg_assignments(b, file)
    out[names(items)] <- items
  }
  Filter(function(v) v != 0, out)
}

mrg_pkmodel <- function(b, file) {
  text <- paste(b$lines, collapse = " ")
  args <- tryCatch(
    as.list(str2lang(sprintf("list(%s)", text)))[-1],
    error = function(e) NULL
  )
  if (
    is.null(args) ||
      any(!nzchar(names(args))) ||
      !all(vapply(args, function(a) is.atomic(a) && length(a) == 1, logical(1)))
  ) {
    parse_error(
      file,
      b$line[[1]],
      1L,
      seen = trimws(text),
      expected = "options name = value",
      call = NULL
    )
  }
  cmt <- if (is.null(args$cmt)) {
    character()
  } else {
    strsplit(trimws(args$cmt), "[[:space:],]+")[[1]]
  }
  if (!is.null(args$advan)) {
    advan <- as.integer(args$advan)
  } else {
    ncmt <- as.integer(args$ncmt %||% 1L)
    depot <- isTRUE(args$depot)
    advan <- if (depot) c(2L, 4L, 12L)[[ncmt]] else c(1L, 3L, 11L)[[ncmt]]
  }
  closed <- nm_closed[[as.character(advan)]]
  if (is.null(closed)) {
    parse_error(
      file,
      b$line[[1]],
      1L,
      seen = sprintf("advan = %d", advan),
      expected = "advan 1, 2, 3, 4, 11 or 12",
      call = NULL
    )
  }
  trans <- as.integer(args$trans %||% NA)
  if (is.na(trans)) {
    # Without `trans`, mrgsolve takes NONMEM's names when there is a depot;
    # without one, its default differs from TRANS4 and is refused.
    if (!advan %in% c(1L, 2L, 4L, 12L)) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = trimws(text),
        expected = "trans = 4 for a model without a depot",
        call = NULL
      )
    }
    trans <- closed$trans
  }
  if (trans != closed$trans) {
    parse_error(
      file,
      b$line[[1]],
      1L,
      seen = sprintf("trans = %d", trans),
      expected = sprintf("trans = %d", closed$trans),
      call = NULL
    )
  }
  list(
    cmt = cmt,
    structure = list(
      type = "closed",
      ncmt = sum(c("v", "vp", "vp2") %in% names(closed$names)),
      depot = "ka" %in% names(closed$names),
      advan = advan,
      trans = trans
    )
  )
}

# $MAIN: individual statements and the dosing macros.
mrg_main <- function(stmts, compartments, file) {
  dosing <- list()
  individual <- list()
  macros <- c(F = "bioavailability", ALAG = "lag", R = "rate", D = "duration")
  for (x in stmts) {
    target <- if (is_call(x, "<-")) as.character(x[[2]]) else ""
    m <- regmatches(target, regexec("^(F|ALAG|R|D)_(.+)$", target))[[1]]
    init <- regmatches(target, regexec("^(.+)_0$", target))[[1]]
    if (length(m) == 3 && m[[3]] %in% compartments) {
      dosing[[m[[3]]]] <- c(
        dosing[[m[[3]]]],
        stats::setNames(list(x[[3]]), macros[[m[[2]]]])
      )
    } else if (length(init) == 2 && init[[2]] %in% compartments) {
      dosing[[init[[2]]]] <- c(dosing[[init[[2]]]], list(initial = x[[3]]))
    } else {
      individual[[length(individual) + 1]] <- x
    }
  }
  list(individual = individual, dosing = dosing)
}

mrg_ode <- function(stmts, compartments, file) {
  derivatives <- list()
  statements <- list()
  for (x in stmts) {
    target <- if (is_call(x, "<-")) as.character(x[[2]]) else ""
    if (startsWith(target, "dxdt_")) {
      cmt <- sub("^dxdt_", "", target)
      if (!cmt %in% compartments) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = target,
          expected = "dxdt_ of a compartment",
          call = NULL
        )
      }
      derivatives[[cmt]] <- x[[3]]
    } else {
      statements[[length(statements) + 1]] <- x
    }
  }
  missing <- setdiff(compartments, names(derivatives))
  if (length(missing) > 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = "$ODE",
      expected = sprintf("dxdt_%s", missing[[1]]),
      call = NULL
    )
  }
  list(
    type = "ode",
    compartments = compartments,
    statements = statements,
    derivatives = derivatives[compartments]
  )
}

# $TABLE: Y over a prediction. The prediction is the variable Y reads that
# $TABLE defines without an epsilon.
mrg_table <- function(stmts, capture, file) {
  defs <- list()
  for (x in stmts) {
    if (!is_call(x, "<-")) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = expr_text(x),
        expected = "assignments in $TABLE",
        call = NULL
      )
    }
    defs[[as.character(x[[2]])]] <- x[[3]]
  }
  if (is.null(defs$Y)) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = "$TABLE",
      expected = "Y = ... with EPS(n)",
      call = NULL
    )
  }
  has_eps <- function(e) any(grepl("^EPS[0-9]+$", all.vars(e)))
  pred <- Filter(
    function(v) v %in% names(defs) && !has_eps(defs[[v]]),
    all.vars(defs$Y)
  )
  if (length(pred) == 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = expr_text(defs$Y),
      expected = "Y over a prediction defined in $TABLE",
      call = NULL
    )
  }
  pred <- pred[[1]]
  used <- c("Y", pred)
  subst <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (name == pred) {
        return(quote(F))
      }
      if (name %in% names(defs)) {
        used <<- union(used, name)
        return(call("(", subst(defs[[name]])))
      }
      return(e)
    }
    if (is.call(e)) {
      e[-1] <- lapply(as.list(e)[-1], subst)
    }
    e
  }
  expr <- subst(defs$Y)
  captured <- unlist(lapply(capture, function(b) {
    strsplit(trimws(paste(b$lines, collapse = " ")), "[[:space:],]+")[[1]]
  }))
  outputs <- setdiff(c(names(defs), captured), used)
  list(
    prediction = defs[[pred]],
    residual = nm_residual(strip_parens(expr), expr),
    losses = if (length(outputs) > 0) {
      sprintf(
        "$TABLE outputs not used by Y: %s",
        paste(unique(outputs), collapse = ", ")
      )
    }
  )
}

# $NMXML or $NMEXT: thetas, omega and sigma from a NONMEM run, as
# parameters (THETA1, ...) and blocks whose values are the estimates.
mrg_nonmem <- function(blocks, file) {
  if (length(blocks) == 0) {
    return(NULL)
  }
  b <- blocks[[1]]
  args <- tryCatch(
    as.list(str2lang(sprintf("list(%s)", paste(b$lines, collapse = " "))))[-1],
    error = function(e) NULL
  )
  if (is.null(args)) {
    parse_error(
      file,
      b$line[[1]],
      1L,
      seen = paste(b$lines, collapse = " "),
      expected = "options name = value",
      call = NULL
    )
  }
  ext <- if (b$name == "NMXML") ".xml" else ".ext"
  path <- if (!is.null(args$path)) {
    args$path
  } else if (!is.null(args$run)) {
    file.path(args$project %||% ".", args$run, paste0(args$run, ext))
  } else {
    parse_error(
      file,
      b$line[[1]],
      1L,
      seen = paste(b$lines, collapse = " "),
      expected = "path or run",
      call = NULL
    )
  }
  if (!grepl("^/", path)) {
    path <- file.path(dirname(file), path)
  }
  if (!file.exists(path)) {
    zupkpmx_abort(sprintf("No NONMEM output at `%s`.", path), "model")
  }
  est <- if (b$name == "NMXML") {
    if (!has_zuxml()) {
      engine_error("zuxml", call = NULL)
    }
    nm_estimates_xml(path)
  } else {
    nm_estimates_ext(path)
  }
  split <- function(full, prefix) {
    n <- nrow(full)
    names <- paste0(prefix, seq_len(n))
    dimnames(full) <- list(names, names)
    if (all(full[upper.tri(full)] == 0)) {
      return(lapply(seq_len(n), function(i) {
        m <- full[i, i, drop = FALSE]
        list(names = names[[i]], matrix = m, fixed = FALSE, estimate = m)
      }))
    }
    list(list(names = names, matrix = full, fixed = FALSE, estimate = full))
  }
  list(
    path = normalizePath(path),
    thetas = data.frame(
      name = paste0("THETA", seq_along(est$theta)),
      role = "other",
      init = unname(est$theta),
      lower = -Inf,
      upper = Inf,
      fixed = FALSE,
      estimate = unname(est$theta)
    ),
    omega = split(est$omega, "ETA"),
    sigma = split(est$sigma, "EPS")
  )
}
