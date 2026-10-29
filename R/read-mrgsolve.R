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
#' `fmax()`, `ETA(n)`, `EPS(n)`, `THETA(n)`, and the macros `dxdt_CMT`,
#' `F_CMT`, `ALAG_CMT`, `R_CMT`, `D_CMT` and `CMT_0`. Anything else,
#' including `$GLOBAL`, `$PREAMBLE`, the `nm-vars` plugin, `int`
#' declarations and integer division (`3/4`), is refused with an error of
#' class `zufapmx_parse_error`. `$PKMODEL` may take its parameters from
#' `$PARAM` directly or from `$MAIN`.
#'
#' Recorded as losses of the read: the title, `$PLUGIN`, `$SET`, `$PARAM`
#' descriptions, `$PARAM` values written as constant expressions
#' (`log(2)`), which are evaluated, and `$TABLE` outputs `Y` does not use. Time is `t`: read
#' from `SOLVERTIME` in `$ODE` and from `TIME` elsewhere; the other name is
#' refused, since it is a different time. `$NMXML` and
#' `$NMEXT` resolve a relative path against the working directory, or the
#' model file's with `root = "cppfile"`, as mrgsolve does.
#'
#' The observation is `Y` in `$TABLE`, written over a prediction such as
#' `IPRED`; its residual model is recognised as for NONMEM. An `@annotated`
#' `$PARAM` description of the form `[lower, upper]`, optionally followed
#' by `fixed`, gives a parameter's bounds, as [pmx_write_mrgsolve()] writes
#' them.
#'
#' @param file Path to the model file.
#' @return A `zufapmx_model`; see [pmx_inspect] and [pmx_write_mrgsolve()].
#' @family readers
#' @seealso [pmx_inspect] for the model's parts.
#' @export
#' @examples
#' path <- system.file(
#'   "corpus", "twocmt-bolus", "model.cpp",
#'   package = "zufapmx"
#' )
#' pmx_read_mrgsolve(path)
pmx_read_mrgsolve <- function(file) {
  check_file(file)
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
      seen = if (length(lines) > 0) trimws(lines[[1]]) else "an empty file",
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
    block_options <- regmatches(text, gregexpr("@[A-Za-z_]+", text))
    list(
      name = name,
      lines = gsub("@[A-Za-z_]+", " ", text),
      line = at,
      options = unique(unlist(block_options))
    )
  })
}

# Remove // and /* */ comments, keeping line numbers.
mrg_strip_comments <- function(lines) {
  text <- paste(lines, collapse = "\n")
  # A /* */ comment keeps its new lines, so later lines keep their numbers.
  comments <- gregexpr("/\\*.*?\\*/", text, perl = TRUE)
  regmatches(text, comments) <- lapply(regmatches(text, comments), function(m) {
    gsub("[^\n]", "", m)
  })
  out <- strsplit(text, "\n", fixed = TRUE)[[1]]
  out <- c(out, rep("", length(lines) - length(out)))
  # A // comment starts outside a string: a path such as "a//b" is kept.
  sub('^((?:[^"/]|"[^"]*"|/(?!/))*)//.*$', "\\1", out, perl = TRUE)
}

mrg_read <- function(file) {
  lines <- readLines(file, warn = FALSE)
  blocks <- mrg_split(lines, file)
  block_names <- vapply(blocks, `[[`, character(1), "name")
  block <- function(...) blocks[block_names %in% c(...)]
  losses <- character()
  if ("PROB" %in% block_names) {
    losses <- c(losses, "title ($PROB)")
  }
  if ("SET" %in% block_names) {
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
  random <- mrg_random_effects(
    block("OMEGA", "SIGMA", "NMXML", "NMEXT"),
    thetas,
    file
  )
  omega <- random$omega
  sigma <- random$sigma
  thetas <- random$thetas
  estimates <- random$estimates
  parameters <- rbind(params$parameters, thetas)
  losses <- c(losses, params$losses)

  layout <- mrg_compartments(
    block("PKMODEL"),
    block("CMT"),
    file,
    length(lines)
  )
  closed <- layout$closed
  rename <- layout$rename
  compartments <- layout$compartments
  code <- function(..., ode = FALSE) {
    stmts <- unlist(
      lapply(block(...), cpp_parse, file = file, ode = ode),
      recursive = FALSE
    )
    time <- if (ode) c(SOLVERTIME = "t") else c(TIME = "t")
    lapply(stmts, rename_symbols, map = c(rename, time))
  }

  # The first of a record's blocks, for refusals of its statements.
  at <- function(...) {
    b <- block(...)
    if (length(b) == 0) {
      return(record_at(paste0("$", c(...)[[1]]), NA_integer_))
    }
    record_at(paste0("$", b[[1]]$name), b[[1]]$line[[1]])
  }
  main <- mrg_main(
    code("MAIN", "PK"),
    compartments,
    rename,
    file,
    at("MAIN", "PK")
  )
  init <- mrg_init(block("INIT"), file)
  for (cmt in names(init)) {
    target <- rename[cmt] %|NA|% cmt
    main$dosing[[target]] <- c(
      main$dosing[[target]],
      list(initial = init[[cmt]])
    )
  }
  if (!is.null(closed)) {
    # Compartments beside a closed form need ODE code it cannot hold.
    for (b in block("ODE", "DES")) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = paste0("$", b$name),
        message = sprintf(
          "not translated: $%s beside $PKMODEL at line %d",
          b$name,
          b$line[[1]]
        ),
        call = NULL
      )
    }
    structure <- closed_structure(
      list(advan = closed$structure$advan, trans = closed$structure$trans),
      closed_forms[[as.character(closed$structure$advan)]],
      # mrgsolve's usual idiom is $PKMODEL over $PARAM names directly.
      c(stmt_targets(main$individual), parameters$name),
      file,
      at("PKMODEL")
    )
  } else {
    structure <- split_ode(
      code("ODE", "DES", ode = TRUE),
      compartments,
      mrg_derivative(compartments, file, at("ODE", "DES")),
      file,
      at("ODE", "DES")
    )
  }
  captured <- unlist(lapply(block("CAPTURE"), function(b) {
    strsplit(trimws(paste(b$lines, collapse = " ")), "[[:space:],]+")[[1]]
  }))
  table <- read_observation(
    code("TABLE", "ERROR"),
    file,
    at("TABLE", "ERROR"),
    outputs = captured
  )
  losses <- c(losses, table$losses)

  parameters$role <- infer_roles(parameters$name, main$individual, structure)
  read_model(
    file,
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
      estimates = estimates,
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
        if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", parts[[1]])) {
          parse_error(
            file,
            b$line[[k]],
            1L,
            seen = parts[[1]],
            expected = "a parameter name",
            call = NULL
          )
        }
        if (any(mrg_evaluated(c(parts[[2]], bounds[-1])))) {
          losses <- c(
            losses,
            "parameter values written as expressions, evaluated"
          )
        }
        init <- mrg_number(parts[[2]], file, b$line[[k]])
        values <- if (length(bounds) == 3) {
          c(
            mrg_number(bounds[[2]], file, b$line[[k]]),
            init,
            mrg_number(bounds[[3]], file, b$line[[k]])
          )
        } else {
          init
        }
        checked <- read_bounds(values)
        if (is.null(checked)) {
          parse_error(
            file,
            b$line[[k]],
            1L,
            seen = line,
            expected = "lower <= value <= upper",
            call = NULL
          )
        }
        rows[[length(rows) + 1]] <- data.frame(
          name = parts[[1]],
          role = "other",
          init = checked$init,
          lower = checked$lower,
          upper = checked$upper,
          fixed = grepl("\\bfixed\\b", text),
          estimate = NA_real_
        )
      }
    } else {
      items <- mrg_assignments(b, file)
      if (any(attr(items, "evaluated"))) {
        losses <- c(
          losses,
          "parameter values written as expressions, evaluated"
        )
      }
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
  evaluated <- logical()
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
    evaluated <- c(evaluated, mrg_evaluated(parts[[2]]))
  }
  attr(out, "evaluated") <- evaluated
  out
}

# A number, or a constant expression of the function table (`log(2)`).
# A value written as an expression rather than a number (`log(2)`): it is
# evaluated, and its text is lost.
mrg_evaluated <- function(text) {
  !grepl(
    "^[-+]?(Inf|([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?)$",
    trimws(text)
  )
}

mrg_number <- function(text, file, line) {
  text <- trimws(text)
  if (text %in% c("Inf", "-Inf")) {
    return(if (text == "Inf") Inf else -Inf)
  }
  x <- tryCatch(str2lang(text), error = function(e) NULL)
  ok <- !is.null(x) && is_constant_expr(x)
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
      out[[length(out) + 1]] <- cov_block(next_names(out, prefix, n), values)
    } else {
      for (v in values) {
        out[[length(out) + 1]] <- cov_block(next_names(out, prefix, 1), v)
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

# A block's `name = value` options, each value one constant.
mrg_block_options <- function(b, file) {
  text <- paste(b$lines, collapse = " ")
  args <- tryCatch(
    as.list(str2lang(sprintf("list(%s)", text)))[-1],
    error = function(e) NULL
  )
  if (
    is.null(args) ||
      is.null(names(args)) ||
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
  args
}

mrg_pkmodel <- function(b, file) {
  text <- paste(b$lines, collapse = " ")
  args <- mrg_block_options(b, file)
  cmt <- if (is.null(args$cmt)) {
    character()
  } else {
    strsplit(trimws(args$cmt), "[[:space:],]+")[[1]]
  }
  if (!is.null(args$advan)) {
    advan <- as.integer(args$advan)
  } else {
    ncmt <- as.integer(args$ncmt %||% 1L)
    if (is.na(ncmt) || !ncmt %in% 1:3) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = sprintf("ncmt = %s", args$ncmt),
        expected = "ncmt 1, 2 or 3",
        call = NULL
      )
    }
    depot <- isTRUE(args$depot)
    advan <- closed_advan(ncmt, depot)
  }
  closed <- closed_forms[[as.character(advan)]]
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

# $NMXML or $NMEXT: thetas, omega and sigma from a NONMEM run, as
# parameters (THETA1, ...) and blocks whose values are the estimates.
mrg_nonmem <- function(b, file) {
  args <- mrg_block_options(b, file)
  # Other options (`omega = FALSE`, `tname`, ...) change what is imported
  # and how it is named; the defaults of theta, omega and sigma are TRUE.
  known <- c("path", "run", "project", "root")
  defaults <- c("theta", "omega", "sigma")
  for (i in seq_along(args)) {
    key <- names(args)[[i]] %||% ""
    value <- args[[i]]
    ok <- if (key %in% known) {
      # A run may be a number: `run = 101`.
      (is.character(value) || key == "run" && is.numeric(value)) &&
        length(value) == 1 &&
        !is.na(value)
    } else {
      key %in% defaults && isTRUE(value)
    }
    if (!ok) {
      parse_error(
        file,
        b$line[[1]],
        1L,
        seen = if (nzchar(key)) key else expr_text(value),
        message = sprintf(
          "not translated: `%s` in $%s at line %d",
          if (nzchar(key)) key else expr_text(value),
          b$name,
          b$line[[1]]
        ),
        call = NULL
      )
    }
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
  # mrgsolve resolves a relative path against the working directory, or
  # against the model file's directory with root = "cppfile".
  if (!grepl("^(/|~|[A-Za-z]:[/\\\\])", path)) {
    root <- if (identical(args$root, "cppfile")) dirname(file) else getwd()
    path <- file.path(root, path)
  }
  if (!file.exists(path)) {
    parse_error(
      file,
      b$line[[1]],
      1L,
      seen = path,
      message = sprintf(
        "no NONMEM output at `%s`, named in $%s at line %d",
        path,
        b$name,
        b$line[[1]]
      ),
      call = NULL
    )
  }
  est <- if (b$name == "NMXML") {
    if (!has_zuxml()) {
      engine_error("zuxml", call = NULL)
    }
    nm_estimates_xml(path)
  } else {
    nm_estimates_ext(path)
  }
  as_blocks <- function(full, prefix) {
    n <- nrow(full)
    eta_names <- paste0(prefix, seq_len(n))
    dimnames(full) <- list(eta_names, eta_names)
    if (all(full[upper.tri(full)] == 0)) {
      return(lapply(seq_len(n), function(i) {
        m <- full[i, i, drop = FALSE]
        list(names = eta_names[[i]], matrix = m, fixed = FALSE, estimate = m)
      }))
    }
    list(list(names = eta_names, matrix = full, fixed = FALSE, estimate = full))
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
    omega = as_blocks(est$omega, "ETA"),
    sigma = as_blocks(est$sigma, "EPS")
  )
}

# $MAIN: individual statements and the dosing macros. Macros name
# compartments as the file does (`F_GUT`); `rename` maps those names to the
# representation's (a closed form's `depot`).
mrg_main <- function(stmts, compartments, rename, file, record) {
  dosing <- list()
  individual <- list()
  macros <- c(F = "bioavailability", ALAG = "lag", R = "rate", D = "duration")
  file_names <- c(names(rename), setdiff(compartments, rename))
  to_model <- function(cmt) rename[cmt] %|NA|% cmt
  for (x in stmts) {
    target <- if (is_call(x, "<-")) as.character(x[[2]]) else ""
    m <- regmatches(target, regexec("^(F|ALAG|R|D)_(.+)$", target))[[1]]
    init <- regmatches(target, regexec("^(.+)_0$", target))[[1]]
    if (length(m) == 3 && m[[3]] %in% file_names) {
      cmt <- to_model(m[[3]])
      dosing[[cmt]] <- c(
        dosing[[cmt]],
        stats::setNames(list(x[[3]]), macros[[m[[2]]]])
      )
    } else if (length(init) == 2 && init[[2]] %in% file_names) {
      cmt <- to_model(init[[2]])
      dosing[[cmt]] <- c(dosing[[cmt]], list(initial = x[[3]]))
    } else {
      individual[[length(individual) + 1]] <- x
    }
  }
  pattern <- sprintf(
    "^(F|ALAG|R|D)_(%s)$|^(%s)_0$",
    paste(file_names, collapse = "|"),
    paste(file_names, collapse = "|")
  )
  check_dosing_names(individual, function(n) grepl(pattern, n), file, record)
  list(individual = individual, dosing = dosing)
}

# $ODE's dxdt_CMT names compartment CMT.
mrg_derivative <- function(compartments, file, record) {
  function(x) {
    target <- as.character(x[[2]])
    if (!startsWith(target, "dxdt_")) {
      return(NULL)
    }
    cmt <- sub("^dxdt_", "", target)
    if (!cmt %in% compartments) {
      parse_error(
        file,
        record_line(record),
        NA_integer_,
        seen = target,
        expected = "dxdt_ of a compartment",
        call = NULL
      )
    }
    cmt
  }
}

# Random effects in file order: $OMEGA and $SIGMA blocks, and the matrices
# a $NMXML or $NMEXT imports where it stands; its thetas replace $THETA's.
# ETA(n) and EPS(n) count positions, so names are given once all are in.
mrg_random_effects <- function(blocks, thetas, file) {
  omega <- list()
  sigma <- list()
  nm <- NULL
  for (b in blocks) {
    if (b$name == "OMEGA") {
      omega <- c(omega, mrg_matrix_blocks(list(b), "ETA", file))
    } else if (b$name == "SIGMA") {
      sigma <- c(sigma, mrg_matrix_blocks(list(b), "EPS", file))
    } else {
      if (!is.null(nm) || nrow(thetas) > 0) {
        parse_error(
          file,
          b$line[[1]],
          1L,
          seen = paste0("$", b$name),
          expected = "one source of THETA1, ...: $THETA or one $NMXML/$NMEXT",
          call = NULL
        )
      }
      nm <- mrg_nonmem(b, file)
      omega <- c(omega, nm$omega)
      sigma <- c(sigma, nm$sigma)
    }
  }
  list(
    omega = renumber_blocks(omega, "ETA"),
    sigma = renumber_blocks(sigma, "EPS"),
    thetas = if (is.null(nm)) thetas else nm$thetas,
    estimates = if (is.null(nm)) NA_character_ else nm$path
  )
}

# The compartments: a $PKMODEL's canonical ones, with `rename` from the
# file's names (DEPOT CENT) to them, or an ODE model's $CMT.
mrg_compartments <- function(pkmodel, cmt, file, last_line) {
  cmt_names <- unlist(lapply(cmt, mrg_cmt_names))
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
        seen = if (length(closed$cmt) == 0) {
          "no compartment names"
        } else {
          paste(closed$cmt, collapse = " ")
        },
        expected = sprintf(
          "%d compartment names, in `cmt =` or $CMT",
          length(canonical)
        ),
        call = NULL
      )
    }
    rename <- stats::setNames(canonical, closed$cmt)
    compartments <- canonical
  } else {
    if (length(cmt_names) == 0) {
      parse_error(
        file,
        last_line,
        NA_integer_,
        seen = "end of file",
        expected = "$CMT or $PKMODEL",
        call = NULL
      )
    }
    rename <- character()
    compartments <- cmt_names
  }
  list(closed = closed, rename = rename, compartments = compartments)
}
