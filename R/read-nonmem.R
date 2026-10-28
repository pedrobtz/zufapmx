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
#' Verbatim Fortran, `$INFN`, `$PRED`, `$MIX`, `$PRIOR`, user subroutines
#' and anything else outside that subset are refused with an error of
#' class `zupkpmx_parse_error` naming the file, line and column. The title,
#' data records, estimation settings, tables, comments and the ODE solver
#' are recorded as losses of the read.
#'
#' NONMEM's names are kept: `THETA(1)` reads as `THETA1`, `ETA(1)` as
#' `ETA1` and `EPS(1)` as `EPS1`. The observation is the endpoint `Y`.
#'
#' @param file Path to the control stream.
#' @param estimates Where final estimates come from. `"auto"` reads
#'   `run.xml` (`<file>.xml`) through zuxml when both exist, else the
#'   `.ext` file, else none. `"xml"`, `"ext"` and `"lst"` read that file and
#'   fail if it is missing; `"none"` reads no estimates.
#' @return A `zupkpmx_model`.
#' @export
#' @examples
#' corpus <- zupkpmx_corpus()
#' pmx_read_nonmem(file.path(corpus$path[[4]], "model.ctl"))
pmx_read_nonmem <- function(
  file,
  estimates = c("auto", "xml", "ext", "lst", "none")
) {
  estimates <- match.arg(estimates)
  if (!is.character(file) || length(file) != 1 || !file.exists(file)) {
    zupkpmx_abort("`file` must be the path of an existing file.", "model")
  }
  model <- nm_read(file)
  nm_attach_estimates(model, file, estimates)
}

nm_closed <- list(
  `1` = list(
    trans = 2L,
    cmt = "central",
    central = 1L,
    names = c(cl = "CL", v = "V")
  ),
  `2` = list(
    trans = 2L,
    cmt = c("depot", "central"),
    central = 2L,
    names = c(cl = "CL", v = "V", ka = "KA")
  ),
  `3` = list(
    trans = 4L,
    cmt = c("central", "peripheral1"),
    central = 1L,
    names = c(cl = "CL", v = "V1", q = "Q", vp = "V2")
  ),
  `4` = list(
    trans = 4L,
    cmt = c("depot", "central", "peripheral1"),
    central = 2L,
    names = c(cl = "CL", v = "V2", q = "Q", vp = "V3", ka = "KA")
  ),
  `11` = list(
    trans = 4L,
    cmt = c("central", "peripheral1", "peripheral2"),
    central = 1L,
    names = c(cl = "CL", v = "V1", q = "Q2", vp = "V2", q2 = "Q3", vp2 = "V3")
  ),
  `12` = list(
    trans = 4L,
    cmt = c("depot", "central", "peripheral1", "peripheral2"),
    central = 2L,
    names = c(
      cl = "CL",
      v = "V2",
      q = "Q3",
      vp = "V3",
      q2 = "Q4",
      vp2 = "V4",
      ka = "KA"
    )
  )
)
nm_ode_advans <- c(6L, 8L, 9L, 13L)

nm_lost_records <- c(
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
  split <- nm_split(readLines(file, warn = FALSE), file)
  records <- split$records
  names <- vapply(records, `[[`, character(1), "name")
  losses <- if (split$comments) "comments" else character()

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
    unique(unname(nm_lost_records[intersect(names, names(nm_lost_records))]))
  )
  record <- function(name) records[names == name]
  one <- function(name) {
    found <- record(name)
    if (length(found) == 0) {
      parse_error(
        file,
        NA_integer_,
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

  sub <- nm_subroutines(one("SUBROUTINES"), file)
  losses <- union(losses, sub$losses)
  closed <- nm_closed[[as.character(sub$advan)]]
  if (!is.null(closed)) {
    if (length(record("DES")) > 0 || length(record("MODEL")) > 0) {
      r <- c(record("DES"), record("MODEL"))[[1]]
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = paste0("$", r$name),
        expected = sprintf("no $%s with ADVAN%d", r$name, sub$advan),
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
  cmt <- function(n, column) {
    if (n < 1 || n > length(compartments)) {
      parse_error(
        file,
        NA_integer_,
        column,
        seen = sprintf("A(%d)", n),
        expected = sprintf("a compartment from 1 to %d", length(compartments)),
        call = NULL
      )
    }
    compartments[[n]]
  }

  thetas <- nm_inits(record("THETA"), "THETA", file)
  omega <- nm_blocks(record("OMEGA"), "ETA", file)
  sigma <- nm_blocks(record("SIGMA"), "EPS", file)

  pk <- nm_pk(nm_code(one("PK"), file, cmt), compartments, file)
  if (!is.null(closed)) {
    structure <- nm_closed_structure(sub, closed, pk$individual, file)
    defobs <- closed$central
  } else {
    structure <- nm_ode_structure(
      nm_code(one("DES"), file, cmt),
      compartments,
      file
    )
  }

  scaled <- names(pk$scale)
  if (
    length(scaled) > 1 ||
      (length(scaled) == 1 && as.integer(scaled) != defobs && !is.null(closed))
  ) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = paste0("S", scaled[[length(scaled)]]),
      expected = sprintf("one scale, S%d", defobs),
      call = NULL
    )
  }
  if (length(scaled) == 1) {
    defobs <- as.integer(scaled)
  }
  observed <- as.symbol(compartments[[defobs]])
  prediction <- if (length(scaled) == 1) {
    call("/", observed, pk$scale[[1]])
  } else {
    observed
  }

  error <- nm_error(nm_code(one("ERROR"), file, cmt), file)
  losses <- union(losses, error$losses)

  parameters <- thetas
  parameters$role <- infer_roles(parameters$name, pk$individual, structure)
  model <- new_model(
    parameters = parameters,
    random_effects = list(
      omega = omega,
      sigma = sigma,
      attach = attach_etas(unlist(lapply(omega, `[[`, "names")), pk$individual),
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = pk$individual,
    structure = structure,
    dosing = pk$dosing,
    observations = list(
      Y = list(
        prediction = prediction,
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
  model
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

  if (as.character(advan) %in% names(nm_closed)) {
    if (trans != nm_closed[[as.character(advan)]]$trans) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = sprintf("ADVAN%d TRANS%d", advan, trans),
        expected = sprintf(
          "ADVAN%d TRANS%d",
          advan,
          nm_closed[[as.character(advan)]]$trans
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
  names <- vapply(words, `[[`, character(1), 1)
  defobs <- which(vapply(
    words,
    function(w) "DEFOBS" %in% toupper(w[-1]),
    logical(1)
  ))
  list(names = names, defobs = if (length(defobs) == 1) defobs else 1L)
}

# $PK: individual statements, scale factors and dosing attributes.
nm_pk <- function(stmts, compartments, file) {
  dosing <- list()
  scale <- list()
  individual <- list()
  attrs <- c(ALAG = "lag", F = "bioavailability", R = "rate", D = "duration")
  for (x in stmts) {
    target <- if (is_call(x, "<-")) x[[2]]
    name <- if (is.symbol(target)) toupper(as.character(target)) else ""
    m <- regmatches(name, regexec("^(S|ALAG|F|R|D)([0-9]+)$", name))[[1]]
    if (length(m) == 3) {
      n <- as.integer(m[[3]])
      if (m[[2]] == "S") {
        scale[[as.character(n)]] <- x[[3]]
        next
      }
      if (n < 1 || n > length(compartments)) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = name,
          expected = "a compartment of the model",
          call = NULL
        )
      }
      cmt <- compartments[[n]]
      dosing[[cmt]] <- c(
        dosing[[cmt]],
        stats::setNames(list(x[[3]]), attrs[[m[[2]]]])
      )
      next
    }
    if (is_call(target, "A_0")) {
      cmt <- compartments[[target[[2]]]]
      dosing[[cmt]] <- c(dosing[[cmt]], list(initial = x[[3]]))
      next
    }
    individual[[length(individual) + 1]] <- x
  }
  list(
    individual = individual,
    scale = scale,
    dosing = dosing[intersect(compartments, names(dosing))]
  )
}

nm_closed_structure <- function(sub, closed, individual, file) {
  targets <- stmt_targets(individual)
  found <- vapply(
    closed$names,
    function(name) {
      # An exact match first: C++ (mrgsolve) is case-sensitive, and `cl`
      # next to `CL` must not stand in for it.
      hit <- c(targets[targets == name], targets[toupper(targets) == name])
      if (length(hit) == 0) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = "$PK",
          expected = sprintf(
            "%s for ADVAN%d TRANS%d",
            name,
            sub$advan,
            sub$trans
          ),
          call = NULL
        )
      }
      hit[[1]]
    },
    character(1)
  )
  list(
    type = "closed",
    ncmt = sum(c("v", "vp", "vp2") %in% names(found)),
    depot = "ka" %in% names(found),
    parameters = found,
    advan = sub$advan,
    trans = sub$trans
  )
}

nm_ode_structure <- function(stmts, compartments, file) {
  derivatives <- list()
  statements <- list()
  for (x in stmts) {
    if (is_call(x, "<-") && is_call(x[[2]], "DADT")) {
      derivatives[[compartments[[x[[2]][[2]]]]]] <- x[[3]]
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
      seen = "$DES",
      expected = sprintf("DADT for %s", missing[[1]]),
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

# $ERROR: the residual model of Y over the prediction F. Statements Y does
# not use are outputs, recorded as losses.
nm_error <- function(stmts, file) {
  defs <- list()
  conditional <- character()
  for (x in stmts) {
    if (is_call(x, "<-") && is.symbol(x[[2]])) {
      defs[[as.character(x[[2]])]] <- x[[3]]
    } else {
      conditional <- c(conditional, stmt_targets(list(x)))
    }
  }
  y <- names(defs)[toupper(names(defs)) == "Y"]
  if (length(y) == 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = "$ERROR",
      expected = "Y = ...",
      call = NULL
    )
  }

  used <- character()
  subst <- function(e) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (name %in% conditional) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = name,
          expected = "Y to depend on unconditional statements",
          call = NULL
        )
      }
      if (name %in% names(defs) && name != "F") {
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
  expr <- subst(defs[[y]])
  outputs <- setdiff(c(names(defs), conditional), c(y, used))
  list(
    residual = nm_residual(strip_parens(expr), expr),
    losses = if (length(outputs) > 0) {
      sprintf(
        "$ERROR outputs not used by Y: %s",
        paste(unique(outputs), collapse = ", ")
      )
    } else {
      character()
    }
  )
}

nm_residual <- function(e, original) {
  f <- quote(F)
  is_eps <- function(x) is.symbol(x) && grepl("^EPS[0-9]+$", as.character(x))
  # F * (1 + EPS) or F + F * EPS
  prop <- function(x) {
    if (
      is_call(x, "*") &&
        identical(x[[2]], f) &&
        is_call(x[[3]], "+") &&
        identical(x[[3]][[2]], 1) &&
        is_eps(x[[3]][[3]])
    ) {
      return(as.character(x[[3]][[3]]))
    }
    if (
      is_call(x, "+") &&
        identical(x[[2]], f) &&
        is_call(x[[3]], "*") &&
        identical(x[[3]][[2]], f) &&
        is_eps(x[[3]][[3]])
    ) {
      return(as.character(x[[3]][[3]]))
    }
    NULL
  }
  if (!is.null(p <- prop(e))) {
    return(list(type = "proportional", parameters = p))
  }
  if (is_call(e, "+") && identical(e[[2]], f) && is_eps(e[[3]])) {
    return(list(type = "additive", parameters = as.character(e[[3]])))
  }
  if (
    is_call(e, "*") &&
      identical(e[[2]], f) &&
      is_call(e[[3]], "exp") &&
      is_eps(e[[3]][[2]])
  ) {
    return(list(type = "exponential", parameters = as.character(e[[3]][[2]])))
  }
  if (is_call(e, "+") && is_eps(e[[3]]) && !is.null(p <- prop(e[[2]]))) {
    return(list(type = "combined", parameters = c(as.character(e[[3]]), p)))
  }
  if (
    is_call(e, "+") &&
      is_call(e[[2]], "+") &&
      identical(e[[2]][[2]], f) &&
      is_call(e[[2]][[3]], "*") &&
      identical(e[[2]][[3]][[2]], f) &&
      is_eps(e[[2]][[3]][[3]]) &&
      is_eps(e[[3]])
  ) {
    return(list(
      type = "combined",
      parameters = c(as.character(e[[3]]), as.character(e[[2]][[3]][[3]]))
    ))
  }
  list(
    type = "expression",
    parameters = grep("^EPS[0-9]+$", all.vars(original), value = TRUE),
    expression = original
  )
}

strip_parens <- function(e) {
  if (is_call(e, "(")) {
    return(strip_parens(e[[2]]))
  }
  if (is.call(e)) {
    e[-1] <- lapply(as.list(e)[-1], strip_parens)
  }
  e
}

# $THETA: one row per theta, in order across records.
nm_inits <- function(records, kind, file) {
  items <- nm_init_items(records, file)
  rows <- lapply(seq_along(items), function(i) {
    v <- items[[i]]$values
    data.frame(
      name = paste0(kind, i),
      role = "other",
      init = if (length(v) == 1) v[[1]] else v[[2]],
      lower = if (length(v) > 1) v[[1]] else -Inf,
      upper = if (length(v) > 2) v[[3]] else Inf,
      fixed = items[[i]]$fixed
    )
  })
  out <- if (length(rows) == 0) {
    model_parameters_empty()[, 1:6]
  } else {
    do.call(rbind, rows)
  }
  out$estimate <- rep(NA_real_, nrow(out))
  out
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
    size <- regmatches(text, regexec("BLOCK\\s*\\(\\s*([0-9]+)\\s*\\)", text))[[
      1
    ]]
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
      names <- paste0(
        prefix,
        length(unlist(lapply(blocks, `[[`, "names"))) + seq_len(n)
      )
      m <- matrix(0, n, n, dimnames = list(names, names))
      m[upper.tri(m, diag = TRUE)] <- values
      m[lower.tri(m)] <- t(m)[lower.tri(m)]
      blocks[[length(blocks) + 1]] <- list(
        names = names,
        matrix = m,
        fixed = any(vapply(items, `[[`, logical(1), "fixed")) ||
          grepl("\\bFIX(ED)?\\b", text)
      )
    } else {
      for (i in seq_along(values)) {
        name <- paste0(
          prefix,
          length(unlist(lapply(blocks, `[[`, "names"))) + 1
        )
        blocks[[length(blocks) + 1]] <- list(
          names = name,
          matrix = matrix(values[[i]], dimnames = list(name, name)),
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
  tokens <- list()
  for (r in records) {
    for (k in seq_along(r$lines)) {
      for (t in nm_tokenize(r$lines[[k]], r$line[[k]], file)) {
        t$line <- r$line[[k]]
        tokens[[length(tokens) + 1]] <- t
      }
    }
  }
  i <- 1L
  peek <- function() if (i <= length(tokens)) tokens[[i]]
  fail <- function(expected) {
    t <- peek()
    parse_error(
      file,
      if (is.null(t)) NA_integer_ else t$line,
      if (is.null(t)) NA_integer_ else t$column,
      seen = if (is.null(t)) "end of record" else t$value,
      expected = expected,
      call = NULL
    )
  }
  is_fix <- function() {
    t <- peek()
    !is.null(t) && toupper(t$value) %in% c("FIX", "FIXED")
  }
  number <- function() {
    sign <- 1
    if (!is.null(peek()) && peek()$value %in% c("-", "+")) {
      sign <- if (peek()$value == "-") -1 else 1
      i <<- i + 1L
    }
    t <- peek()
    if (is.null(t)) {
      fail("a number")
    }
    i <<- i + 1L
    if (t$type == "number") {
      return(sign * as.numeric(sub("[Dd]", "E", t$value)))
    }
    if (toupper(t$value) == "INF") {
      return(sign * Inf)
    }
    i <<- i - 1L
    fail("a number")
  }
  items <- list()
  while (i <= length(tokens)) {
    fixed <- FALSE
    if (peek()$value == "(") {
      i <- i + 1L
      values <- number()
      while (!is.null(peek()) && peek()$value == ",") {
        i <- i + 1L
        values <- c(values, number())
      }
      if (is_fix()) {
        fixed <- TRUE
        i <- i + 1L
      }
      if (is.null(peek()) || peek()$value != ")") {
        fail(")")
      }
      i <- i + 1L
    } else {
      values <- number()
    }
    if (length(values) > 3) {
      fail("one to three values")
    }
    if (is_fix()) {
      fixed <- TRUE
      i <- i + 1L
    }
    items[[length(items) + 1]] <- list(values = values, fixed = fixed)
  }
  items
}
