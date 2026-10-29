#' Read a NONMEM control stream
#'
#' Reads a control stream into the package's representation, with final
#' estimates when NONMEM's output files are next to it.
#'
#' The reader covers `$PROBLEM`, `$INPUT`, `$DATA`, `$SUBROUTINES`,
#' `$MODEL`, `$PK`, `$DES`, `$ERROR`, `$THETA`, `$OMEGA`, `$SIGMA`,
#' `$ESTIMATION`, `$COVARIANCE` and `$TABLE`. Closed forms are `ADVAN1`
#' to `ADVAN4`, `ADVAN11` and `ADVAN12` with `TRANS2` or `TRANS4`; general
#' ODE models are `ADVAN6`, `ADVAN8`, `ADVAN9` and `ADVAN13`. Abbreviated
#' code may use assignments, `IF` in one line or in blocks, arithmetic,
#' comparisons, `EXP`, `LOG`, `LOG10`, `SQRT` and `ABS`.
#'
#' Verbatim Fortran, `$INFN`, `$PRED`, `$MIX`, `$PRIOR`, user subroutines,
#' dosing or scale assignments inside `IF`, undeclared `THETA`/`ETA`/`EPS`
#' indices and anything else outside that subset are refused with an error
#' of class `zufapmx_parse_error` naming the file and, where it can, the
#' line and column.
#'
#' Recorded as losses of the read: the title, `$SIZES`, data records,
#' estimation settings, `$SIMULATION`, `$ABBREVIATED`, tables
#' (`$TABLE`, `$SCATTERPLOT`), record options such as
#' `$ERROR (ONLY OBSERVATIONS)`, comments, the ODE solver, `$ERROR`
#' outputs `Y` does not use, and values that NM-TRAN keeps from earlier
#' records for variables assigned only inside `IF` (the representation
#' starts them at 0).
#'
#' NONMEM's names are kept, upper case: `THETA(1)` reads as `THETA1`,
#' `ETA(1)` as `ETA1` and `EPS(1)` as `EPS1`. Time is `t`: read from `T` in
#' `$DES` and from `TIME` elsewhere; `TIME` in `$DES` is refused. Free names
#' such as `WT` are recorded as covariates. The observation is the endpoint
#' `Y`.
#'
#' @param file Path to the control stream.
#' @param estimates Where final estimates come from. `"auto"` reads
#'   `run.xml` (`<file>.xml`) when it exists and zuxml is installed, else
#'   the `.ext` file, else none. `"xml"`, `"ext"` and `"lst"` read that
#'   file and fail if it is missing; `"none"` reads no estimates.
#' @return A `zufapmx_model`; see [pmx_inspect] and [pmx_write_nonmem()].
#' @family readers
#' @seealso [pmx_inspect] for the model's parts.
#' @export
#' @examples
#' path <- system.file(
#'   "corpus", "twocmt-bolus", "model.ctl",
#'   package = "zufapmx"
#' )
#' pmx_read_nonmem(path)
pmx_read_nonmem <- function(
  file,
  estimates = c("auto", "xml", "ext", "lst", "none")
) {
  estimates <- check_choice(estimates)
  check_file(file)
  model <- nm_read(file)
  nm_attach_estimates(model, file, estimates)
}

nm_ode_advans <- c(6L, 8L, 9L, 13L)

nm_lost_records <- c(
  SIZES = "array sizes ($SIZES)",
  PROBLEM = "title ($PROBLEM)",
  INPUT = "data records ($INPUT, $DATA)",
  DATA = "data records ($INPUT, $DATA)",
  ESTIMATION = "estimation settings ($ESTIMATION, $COVARIANCE)",
  COVARIANCE = "estimation settings ($ESTIMATION, $COVARIANCE)",
  TABLE = "tables ($TABLE, $SCATTERPLOT)",
  SCATTERPLOT = "tables ($TABLE, $SCATTERPLOT)",
  SIMULATION = "simulation settings ($SIMULATION)",
  ABBREVIATED = "abbreviated-code options ($ABBREVIATED)"
)

nm_read <- function(file) {
  stream <- nm_split(readLines(file, warn = FALSE), file)
  records <- stream$records
  record_names <- vapply(records, `[[`, character(1), "name")
  losses <- if (stream$comments) "comments" else character()

  losses <- c(losses, nm_record_losses(records, file))
  record <- function(name) records[record_names == name]
  one <- function(name) {
    found <- record(name)
    if (length(found) == 0) {
      parse_error(
        file,
        stream$lines,
        NA_integer_,
        seen = "end of file",
        expected = paste0("$", name),
        call = NULL
      )
    }
    if (length(found) > 1) {
      parse_error(
        file,
        found[[2]]$line[[1]],
        1L,
        seen = paste0("$", name),
        expected = sprintf("one $%s", name),
        call = NULL
      )
    }
    found[[1]]
  }

  subroutines <- nm_subroutines(one("SUBROUTINES"), file)
  losses <- union(losses, subroutines$losses)
  closed <- closed_forms[[as.character(subroutines$advan)]]
  if (!is.null(closed)) {
    if (length(record("DES")) > 0 || length(record("MODEL")) > 0) {
      r <- c(record("DES"), record("MODEL"))[[1]]
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = paste0("$", r$name),
        expected = sprintf("no $%s with ADVAN%d", r$name, subroutines$advan),
        call = NULL
      )
    }
    compartments <- closed$cmt
    defobs <- closed$central
  } else {
    comp <- nm_compartments(one("MODEL"), file)
    compartments <- comp$names
    defobs <- comp$defobs
  }
  cmt <- function(n, line, column) {
    nm_compartment(n, compartments, sprintf("A(%d)", n), file, line, column)
  }

  thetas <- nm_thetas(record("THETA"), file)
  omega <- nm_blocks(record("OMEGA"), "ETA", file)
  sigma <- nm_blocks(record("SIGMA"), "EPS", file)

  at <- function(name) record_at(paste0("$", name), one(name)$line[[1]])
  pk <- nm_pk(nm_code(one("PK"), file, cmt), compartments, file, at("PK"))
  if (!is.null(closed)) {
    structure <- closed_structure(
      subroutines,
      closed,
      stmt_targets(pk$individual),
      file,
      at("PK")
    )
  } else {
    structure <- split_ode(
      nm_code(one("DES"), file, cmt, des = TRUE),
      compartments,
      nm_derivative(compartments, file, at("DES")),
      file,
      at("DES")
    )
  }

  prediction <- nm_prediction(
    pk$scale,
    compartments,
    defobs,
    !is.null(closed),
    file,
    at("PK")
  )

  error <- read_observation(
    nm_code(one("ERROR"), file, cmt),
    file,
    at("ERROR"),
    prediction = quote(F)
  )
  losses <- union(losses, error$losses)

  parameters <- thetas
  parameters$role <- infer_roles(parameters$name, pk$individual, structure)
  read_model(
    file,
    parameters = parameters,
    random_effects = list(
      omega = omega,
      sigma = sigma,
      attach = attach_etas(
        unlist(lapply(omega, `[[`, "names")),
        pk$individual
      ),
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = pk$individual,
    structure = structure,
    dosing = pk$dosing,
    observations = list(
      Y = list(
        # F is the scaled compartment; a Y without F names its own.
        prediction = if (identical(error$prediction, quote(F))) {
          prediction
        } else {
          error$prediction
        },
        residual = error$residual,
        transform = "none"
      )
    ),
    provenance = list(
      format = "nonmem",
      file = normalizePath(file),
      estimates = NA_character_,
      hash = unname(tools::md5sum(file)),
      fidelity = if (length(losses) == 0) "exact" else "lossy",
      losses = losses
    )
  )
}

nm_subroutines <- function(r, file) {
  text <- toupper(paste(r$lines, collapse = " "))
  if (grepl("\\bOTHER\\s*=", text)) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = "OTHER=",
      message = sprintf(
        "not translated: user subroutines at line %d",
        r$line[[1]]
      ),
      call = NULL
    )
  }
  advan <- regmatches(text, regexpr("ADVAN[0-9]+", text))
  if (length(advan) == 0) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = trimws(text),
      expected = "ADVANn",
      call = NULL
    )
  }
  advan <- as.integer(sub("ADVAN", "", advan))
  trans <- regmatches(text, regexpr("TRANS[0-9]+", text))
  trans <- if (length(trans) == 0) 1L else as.integer(sub("TRANS", "", trans))

  if (as.character(advan) %in% names(closed_forms)) {
    if (trans != closed_forms[[as.character(advan)]]$trans) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = sprintf("ADVAN%d TRANS%d", advan, trans),
        expected = sprintf(
          "ADVAN%d TRANS%d",
          advan,
          closed_forms[[as.character(advan)]]$trans
        ),
        call = NULL
      )
    }
    return(list(advan = advan, trans = trans, losses = character()))
  }
  if (advan %in% nm_ode_advans) {
    return(list(
      advan = advan,
      trans = trans,
      losses = sprintf("ODE solver (ADVAN%d)", advan)
    ))
  }
  parse_error(
    file,
    r$line[[1]],
    1L,
    seen = sprintf("ADVAN%d", advan),
    expected = "a closed-form or general ODE ADVAN",
    call = NULL
  )
}

nm_compartments <- function(r, file) {
  text <- paste(r$lines, collapse = " ")
  groups <- regmatches(
    text,
    gregexpr(
      "COMP\\s*=\\s*(\\([^)]*\\)|[A-Za-z_][A-Za-z0-9_]*)",
      text,
      ignore.case = TRUE
    )
  )[[1]]
  if (length(groups) == 0) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = trimws(text),
      expected = "COMP=(NAME)",
      call = NULL
    )
  }
  words <- lapply(groups, function(g) {
    inner <- sub("^COMP\\s*=\\s*\\(?", "", g, ignore.case = TRUE)
    strsplit(trimws(gsub("[(),]", " ", inner)), "\\s+")[[1]]
  })
  comp_names <- vapply(words, `[[`, character(1), 1)
  # INITIALOFF starts a compartment off, which changes predictions.
  if (any(vapply(words, function(w) "INITIALOFF" %in% toupper(w[-1]), NA))) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = "INITIALOFF",
      message = sprintf(
        "not translated: `INITIALOFF` in $MODEL at line %d",
        r$line[[1]]
      ),
      call = NULL
    )
  }
  defobs <- which(vapply(
    words,
    function(w) "DEFOBS" %in% toupper(w[-1]),
    logical(1)
  ))
  list(names = comp_names, defobs = if (length(defobs) == 1) defobs else 1L)
}

# $THETA: one row per theta, in order across records.
nm_thetas <- function(records, file) {
  items <- nm_init_items(records, file)
  rows <- lapply(seq_along(items), function(i) {
    bounds <- read_bounds(items[[i]]$values)
    if (is.null(bounds)) {
      parse_error(
        file,
        items[[i]]$line,
        NA_integer_,
        seen = paste(items[[i]]$values, collapse = ", "),
        expected = "one to three values, lower <= initial <= upper",
        call = NULL
      )
    }
    data.frame(
      name = paste0("THETA", i),
      role = "other",
      init = bounds$init,
      lower = bounds$lower,
      upper = bounds$upper,
      fixed = items[[i]]$fixed,
      estimate = NA_real_
    )
  })
  if (length(rows) == 0) model_parameters_empty() else do.call(rbind, rows)
}

# $OMEGA and $SIGMA: blocks named ETA1, ETA2, ... or EPS1, EPS2, ...
nm_blocks <- function(records, prefix, file) {
  blocks <- list()
  for (r in records) {
    text <- toupper(paste(r$lines, collapse = " "))
    bad <- regmatches(
      text,
      regexpr("\\b(SAME|SD|STANDARD|CORR|CORRELATION|CHOL|CHOLESKY)\\b", text)
    )
    if (length(bad) == 1) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = bad,
        message = sprintf(
          "not translated: %s in $%s at line %d",
          bad,
          r$name,
          r$line[[1]]
        ),
        call = NULL
      )
    }
    block_size <- "BLOCK\\s*\\(\\s*([0-9]+)\\s*\\)"
    size <- regmatches(text, regexec(block_size, text))[[1]]
    r$lines <- gsub(
      "(BLOCK|DIAGONAL)\\s*\\(\\s*[0-9]+\\s*\\)|\\b(VARIANCE|COVARIANCE)\\b",
      " ",
      r$lines,
      ignore.case = TRUE
    )
    items <- nm_init_items(list(r), file)
    values <- vapply(
      items,
      function(i) i$values[[length(i$values)]],
      numeric(1)
    )
    if (length(size) == 2) {
      n <- as.integer(size[[2]])
      if (length(values) != n * (n + 1) / 2) {
        parse_error(
          file,
          r$line[[1]],
          1L,
          seen = sprintf("%d values", length(values)),
          expected = sprintf("%d values for BLOCK(%d)", n * (n + 1) / 2, n),
          call = NULL
        )
      }
      blocks[[length(blocks) + 1]] <- cov_block(
        next_names(blocks, prefix, n),
        values,
        fixed = any(vapply(items, `[[`, logical(1), "fixed")) ||
          grepl("\\bFIX(ED)?\\b", text)
      )
    } else {
      for (i in seq_along(values)) {
        blocks[[length(blocks) + 1]] <- cov_block(
          next_names(blocks, prefix, 1),
          values[[i]],
          fixed = items[[i]]$fixed
        )
      }
    }
  }
  blocks
}

# Initial-value items: `x`, `x FIX`, `(lo, x)`, `(lo, x, hi)`, each with an
# optional FIX inside or after the parentheses.
nm_init_items <- function(records, file) {
  tokens <- unlist(
    lapply(records, function(r) {
      unlist(
        lapply(seq_along(r$lines), function(k) {
          nm_tokenize(r$lines[[k]], r$line[[k]], file)
        }),
        recursive = FALSE
      )
    }),
    recursive = FALSE
  )
  # Running out of items is reported at the last line of the records.
  s <- token_stream(
    tokens,
    file,
    end = list(
      value = "end of record",
      line = max(unlist(lapply(records, `[[`, "line")))
    )
  )
  number <- function() {
    sign <- 1
    if (s$at(c("-", "+"))) {
      sign <- if (s$take()$value == "-") -1 else 1
    }
    tok <- s$peek()
    if (!is.null(tok) && tok$type == "number") {
      s$take()
      return(sign * as.numeric(sub("[Dd]", "E", tok$value)))
    }
    if (s$at("INF")) {
      s$take()
      return(sign * Inf)
    }
    s$fail("a number")
  }
  fix <- function() {
    if (!s$at(c("FIX", "FIXED"))) {
      return(FALSE)
    }
    s$take()
    TRUE
  }
  items <- list()
  while (!s$done()) {
    line <- s$peek()$line
    fixed <- FALSE
    if (s$at("(")) {
      s$take()
      values <- number()
      while (s$at(",")) {
        s$take()
        values <- c(values, number())
      }
      fixed <- fix()
      s$take(")")
    } else {
      values <- number()
    }
    if (length(values) > 3) {
      s$fail("one to three values")
    }
    fixed <- fix() || fixed
    items[[length(items) + 1]] <- list(
      values = values,
      fixed = fixed,
      line = line
    )
  }
  items
}


# $PK: individual statements, scale factors and dosing attributes.
nm_pk <- function(stmts, compartments, file, record) {
  dosing <- list()
  scale <- list()
  individual <- list()
  attrs <- invert_names(nm_dosing_prefixes)
  # Scales S<n> and the dosing attributes, numbered by compartment.
  predpp <- sprintf(
    "^(S|%s)([0-9]+)$",
    paste(nm_dosing_prefixes, collapse = "|")
  )
  check_cmt <- function(n, name) {
    nm_compartment(n, compartments, name, file, record_line(record))
  }
  # Each PREDPP name is assigned once: the reader keeps one value for it.
  refuse <- function(name, expected) {
    parse_error(
      file,
      record_line(record),
      NA_integer_,
      seen = name,
      expected = expected,
      call = NULL
    )
  }
  once <- function(cmt, attr, name, value) {
    if (!is.null(dosing[[cmt]][[attr]])) {
      refuse(name, sprintf("%s assigned once in $PK", name))
    }
    dosing[[cmt]][[attr]] <<- value
  }
  for (x in stmts) {
    target <- if (is_call(x, "<-")) x[[2]]
    name <- if (is.symbol(target)) as.character(target) else ""
    m <- regmatches(name, regexec(predpp, name))[[1]]
    if (name %in% c("TSCALE", "XSCALE")) {
      # They rescale time or amounts inside PREDPP.
      refuse(name, "a $PK statement zufapmx translates")
    } else if (length(m) == 3) {
      n <- as.integer(m[[3]])
      if (m[[2]] == "S") {
        check_cmt(n, name)
        if (!is.null(scale[[as.character(n)]])) {
          refuse(name, sprintf("%s assigned once in $PK", name))
        }
        scale[[as.character(n)]] <- x[[3]]
      } else {
        once(check_cmt(n, name), attrs[[m[[2]]]], name, x[[3]])
      }
    } else if (is_call(target, "A_0")) {
      cmt <- check_cmt(target[[2]], expr_text(target))
      once(cmt, "initial", expr_text(target), x[[3]])
    } else if (is_call(target, "DADT")) {
      refuse(expr_text(target), "DADT(n) in $DES")
    } else {
      individual[[length(individual) + 1]] <- x
    }
  }
  check_dosing_names(
    individual,
    function(n) grepl(predpp, n) | grepl("^A_0\\(", n),
    file,
    record
  )
  list(
    individual = individual,
    scale = scale,
    dosing = dosing[intersect(compartments, names(dosing))]
  )
}

# $DES's DADT(n) names compartment n.
nm_derivative <- function(compartments, file, record) {
  function(x) {
    if (!is_call(x[[2]], "DADT")) {
      return(NULL)
    }
    nm_compartment(
      x[[2]][[2]],
      compartments,
      expr_text(x[[2]]),
      file,
      record_line(record)
    )
  }
}

# Compartment n's name, or a refusal of `seen` naming the range.
nm_compartment <- function(n, compartments, seen, file, line, column = NA) {
  if (n < 1 || n > length(compartments)) {
    parse_error(
      file,
      line,
      as.integer(column),
      seen = seen,
      expected = sprintf("a compartment from 1 to %d", length(compartments)),
      call = NULL
    )
  }
  compartments[[n]]
}

# Refuse the records the reader does not translate, and name what the
# others lose: the title, data records, settings, tables, record options.
nm_record_losses <- function(records, file) {
  record_names <- vapply(records, `[[`, character(1), "name")
  losses <- character()
  for (r in records) {
    if (r$name %in% c("PRED", "INFN", "MIX", "PRIOR")) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = paste0("$", r$name),
        message = sprintf(
          "not translated: $%s at line %d",
          r$name,
          r$line[[1]]
        ),
        call = NULL
      )
    }
  }
  losses <- union(
    losses,
    unique(unname(nm_lost_records[intersect(
      record_names,
      names(nm_lost_records)
    )]))
  )
  for (r in records) {
    if (length(r$options) == 1 && r$name %in% c("PK", "DES", "ERROR")) {
      losses <- union(
        losses,
        sprintf("record options ($%s %s)", r$name, r$options)
      )
    }
  }
  losses
}

# The prediction F: the observed compartment over its scale. A closed form
# scales its central compartment; an ODE model the one its S<n> names, or
# its DEFOBS compartment unscaled.
nm_prediction <- function(scale, compartments, defobs, closed, file, record) {
  scaled <- names(scale)
  if (
    length(scaled) > 1 ||
      (length(scaled) == 1 && as.integer(scaled) != defobs && closed)
  ) {
    parse_error(
      file,
      record_line(record),
      NA_integer_,
      seen = paste0("S", scaled[[length(scaled)]]),
      expected = sprintf("one scale, S%d", defobs),
      call = NULL
    )
  }
  if (length(scaled) == 1) {
    # nm_pk() has checked that S<n> names a compartment.
    defobs <- as.integer(scaled)
  }
  observed <- as.symbol(compartments[[defobs]])
  if (length(scaled) == 1) {
    call("/", observed, scale[[1]])
  } else {
    observed
  }
}
