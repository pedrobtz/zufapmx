# Helpers every reader shares: walking R syntax, attaching random effects,
# inferring parameter roles, and the shapes of closed-form models.

# A full symmetric matrix from lower-triangle values listed row by row.
lower_to_full <- function(values) {
  n <- (sqrt(8 * length(values) + 1) - 1) / 2
  m <- matrix(0, n, n)
  m[upper.tri(m, diag = TRUE)] <- values
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}

is_call <- function(x, name) {
  is.call(x) && identical(x[[1]], as.symbol(name))
}

# Source references are where code was typed, not what it says; a model
# read with them would differ from the same model read without.
strip_srcref <- function(x) {
  if (is.call(x)) {
    attributes(x) <- NULL
    x[] <- lapply(as.list(x), strip_srcref)
    x <- as.call(x)
  }
  x
}

# Each eta attaches to the first individual statement that reads it.
attach_etas <- function(etas, individual) {
  rows <- lapply(etas, function(eta) {
    for (x in individual) {
      if (is_call(x, "<-") && eta %in% all.vars(x[[3]])) {
        return(data.frame(
          eta = eta,
          parameter = as.character(x[[2]]),
          transform = eta_transform(x[[3]], eta)
        ))
      }
    }
    NULL
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(
      eta = character(),
      parameter = character(),
      transform = character()
    )
  }
  out
}

eta_transform <- function(x, eta) {
  e <- as.symbol(eta)
  if (is_call(x, "exp") && is_call(x[[2]], "+") && identical(x[[2]][[3]], e)) {
    return("exp")
  }
  if (is_call(x, "*") && is_call(x[[3]], "exp") && identical(x[[3]][[2]], e)) {
    return("exp")
  }
  if (is_call(x, "+") && identical(x[[3]], e)) {
    return("additive")
  }
  "expression"
}

# A fixed effect takes the role of the closed-form parameter whose
# statement reads it as the only fixed effect.
infer_roles <- function(params, individual, structure) {
  roles <- rep("other", length(params))
  if (!identical(structure$type, "closed")) {
    return(roles)
  }
  role_of <- c(
    cl = "clearance",
    v = "volume",
    vp = "volume",
    vp2 = "volume",
    ka = "ka"
  )
  for (canonical in intersect(names(structure$parameters), names(role_of))) {
    target <- structure$parameters[[canonical]]
    for (x in individual) {
      if (is_call(x, "<-") && identical(as.character(x[[2]]), target)) {
        read <- intersect(all.vars(x[[3]]), params)
        if (length(read) == 1) {
          roles[params == read] <- role_of[[canonical]]
        }
      }
    }
  }
  roles
}

model_parameters_empty <- function() {
  data.frame(
    name = character(),
    role = character(),
    init = numeric(),
    lower = numeric(),
    upper = numeric(),
    fixed = logical(),
    estimate = numeric()
  )
}

closed_compartments <- function(structure) {
  peripheral <- if (structure$ncmt > 1) {
    paste0("peripheral", seq_len(structure$ncmt - 1))
  }
  c(if (isTRUE(structure$depot)) "depot", "central", peripheral)
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

# Tokens of `lines` by `patterns`, tried in order; `space` is skipped. Each
# token is list(type, value, line, column). `upper` names the token types
# whose value is upper-cased (NM-TRAN is case-insensitive).
tokenize <- function(
  lines,
  line_numbers,
  patterns,
  file,
  expected,
  upper = character()
) {
  tokens <- list()
  for (k in seq_along(lines)) {
    text <- lines[[k]]
    pos <- 1L
    n <- nchar(text)
    while (pos <= n) {
      rest <- substr(text, pos, n)
      matched <- FALSE
      for (type in names(patterns)) {
        m <- regmatches(rest, regexpr(patterns[[type]], rest, perl = TRUE))
        if (length(m) == 1 && nzchar(m)) {
          if (type != "space") {
            value <- if (type %in% upper) toupper(m) else m
            tokens[[length(tokens) + 1]] <- list(
              type = type,
              value = value,
              line = line_numbers[[k]],
              column = pos
            )
          }
          pos <- pos + nchar(m)
          matched <- TRUE
          break
        }
      }
      if (!matched) {
        parse_error(
          file,
          line_numbers[[k]],
          pos,
          seen = substr(rest, 1, 1),
          expected = expected,
          call = NULL
        )
      }
    }
  }
  tokens
}

# A cursor over tokens for a recursive-descent parser. `end` is the token
# reported when input runs out: list(value, line).
token_stream <- function(tokens, file, end) {
  i <- 1L
  peek <- function(k = 0L) if (i + k <= length(tokens)) tokens[[i + k]]
  is <- function(value, k = 0L) {
    t <- peek(k)
    !is.null(t) && t$value %in% value
  }
  fail <- function(expected) {
    t <- peek() %||%
      list(value = end$value, line = end$line, column = NA_integer_)
    parse_error(
      file,
      t$line,
      t$column,
      seen = t$value,
      expected = expected,
      call = NULL
    )
  }
  take <- function(value = NULL) {
    t <- peek()
    if (is.null(t) || (!is.null(value) && !t$value %in% value)) {
      fail(
        if (is.null(value)) "more input" else paste(value, collapse = " or ")
      )
    }
    i <<- i + 1L
    t
  }
  # A left-associative binary level: `ops` are token values, `map` gives
  # each one's R operator (the token itself when NULL).
  binary <- function(next_level, ops, map = NULL) {
    function() {
      left <- next_level()
      while (is(ops)) {
        op <- take()$value
        left <- call(if (is.null(map)) op else map[[op]], left, next_level())
      }
      left
    }
  }
  list(
    peek = peek,
    is = is,
    fail = fail,
    take = take,
    binary = binary,
    done = function() i > length(tokens),
    expect_end = function() if (i <= length(tokens)) fail(end$value)
  )
}

# A model's compartments: an ODE model's own, or a closed form's canonical
# ones.
model_compartments <- function(structure) {
  if (identical(structure$type, "ode")) {
    structure$compartments
  } else {
    closed_compartments(structure)
  }
}

# The ADVAN of a closed form with `ncmt` compartments, with or without a
# depot.
closed_advan <- function(ncmt, depot) {
  if (depot) c(2L, 4L, 12L)[[ncmt]] else c(1L, 3L, 11L)[[ncmt]]
}

# Every statement inside an `if`, at any depth.
nested_statements <- function(x) {
  if (!is_call(x, "if")) {
    return(list())
  }
  out <- list()
  for (body in as.list(x)[-(1:2)]) {
    for (y in stmt_body(body)) {
      out <- c(out, list(y), nested_statements(y))
    }
  }
  out
}

# Split ODE code ($DES, $ODE) into structure statements and one derivative
# per compartment. `derivative_of(x)` names the compartment an assignment
# differentiates, or returns NULL; it refuses a malformed one itself.
split_ode <- function(stmts, compartments, derivative_of, file, record) {
  derivatives <- list()
  statements <- list()
  for (x in stmts) {
    for (y in nested_statements(x)) {
      if (is_call(y, "<-") && !is.null(derivative_of(y))) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = expr_text(y[[2]]),
          message = sprintf(
            "not translated: a derivative inside an if in %s",
            record
          ),
          call = NULL
        )
      }
    }
    cmt <- if (is_call(x, "<-")) derivative_of(x)
    if (is.null(cmt)) {
      statements[[length(statements) + 1]] <- x
    } else {
      derivatives[[cmt]] <- x[[3]]
    }
  }
  missing <- setdiff(compartments, names(derivatives))
  if (length(missing) > 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = record,
      expected = sprintf("a derivative of %s", missing[[1]]),
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

# The observation from $ERROR or $TABLE: the residual model of Y. Each
# definition is stored expanded as it stands where it is made, so Y sees
# the definitions in force where Y is assigned (design §6). `prediction` is
# NONMEM's F; without one (mrgsolve), the prediction is the term in F's
# place of a recognised residual form, and anything else is refused.
# `outputs` are other names the record makes visible ($CAPTURE).
read_observation <- function(
  stmts,
  file,
  record,
  prediction = NULL,
  outputs = character()
) {
  defs <- list()
  deps <- list()
  conditional <- character()
  y <- NULL
  used <- character()
  expand <- function(e, track) {
    if (is.symbol(e)) {
      name <- as.character(e)
      if (name %in% conditional) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = name,
          expected = sprintf(
            "Y in %s to depend on unconditional statements",
            record
          ),
          call = NULL
        )
      }
      if (!is.null(defs[[name]])) {
        track(c(name, deps[[name]]))
        return(call("(", defs[[name]]))
      }
      return(e)
    }
    if (is.call(e)) {
      e[-1] <- lapply(as.list(e)[-1], expand, track = track)
    }
    e
  }
  for (x in stmts) {
    if (is_call(x, "<-") && is.symbol(x[[2]])) {
      name <- as.character(x[[2]])
      if (identical(name, "Y")) {
        y <- expand(x[[3]], function(n) used <<- union(used, n))
      } else {
        seen <- character()
        defs[[name]] <- expand(x[[3]], function(n) seen <<- union(seen, n))
        deps[[name]] <- seen
        conditional <- setdiff(conditional, name)
      }
    } else {
      conditional <- union(conditional, stmt_targets(list(x)))
    }
  }
  if (is.null(y)) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = record,
      expected = "Y = ... over EPS(n)",
      call = NULL
    )
  }

  form <- residual_form(strip_parens(y), prediction)
  if (is.null(form)) {
    if (is.null(prediction)) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = expr_text(y),
        expected = "Y as an additive, proportional, combined or exponential model over one prediction",
        call = NULL
      )
    }
    form <- list(
      type = "expression",
      parameters = grep("^EPS[0-9]+$", all.vars(y), value = TRUE),
      expression = y,
      prediction = prediction
    )
  }
  unused <- setdiff(c(names(defs), conditional, outputs), c("Y", used))
  list(
    prediction = form$prediction,
    residual = form[setdiff(names(form), "prediction")],
    losses = if (length(unused) > 0) {
      sprintf(
        "%s outputs not used by Y: %s",
        record,
        paste(unique(unused), collapse = ", ")
      )
    } else {
      character()
    }
  )
}

# The residual model of Y over its prediction p: p * (1 + E) or p + p * E
# (proportional), p + E (additive), p * exp(E) (exponential), either
# proportional form plus E2 (combined). `prediction`, when given, must be p.
residual_form <- function(e, prediction = NULL) {
  is_eps <- function(x) is.symbol(x) && grepl("^EPS[0-9]+$", as.character(x))
  fits <- function(p) {
    !any(grepl("^EPS[0-9]+$", all.vars(p))) &&
      (is.null(prediction) || identical(p, prediction))
  }
  form <- function(type, p, parameters) {
    list(type = type, parameters = parameters, prediction = p)
  }
  prop <- function(x) {
    if (
      is_call(x, "*") &&
        is_call(x[[3]], "+") &&
        identical(x[[3]][[2]], 1) &&
        is_eps(x[[3]][[3]]) &&
        fits(x[[2]])
    ) {
      return(list(p = x[[2]], eps = as.character(x[[3]][[3]])))
    }
    if (
      is_call(x, "+") &&
        is_call(x[[3]], "*") &&
        is_eps(x[[3]][[3]]) &&
        identical(x[[2]], x[[3]][[2]]) &&
        fits(x[[2]])
    ) {
      return(list(p = x[[2]], eps = as.character(x[[3]][[3]])))
    }
    NULL
  }
  if (!is.null(r <- prop(e))) {
    return(form("proportional", r$p, r$eps))
  }
  if (is_call(e, "+") && is_eps(e[[3]]) && !is.null(r <- prop(e[[2]]))) {
    return(form("combined", r$p, c(as.character(e[[3]]), r$eps)))
  }
  if (is_call(e, "+") && is_eps(e[[3]]) && fits(e[[2]])) {
    return(form("additive", e[[2]], as.character(e[[3]])))
  }
  if (is_call(e, "+") && is_eps(e[[2]]) && fits(e[[3]])) {
    return(form("additive", e[[3]], as.character(e[[2]])))
  }
  if (
    is_call(e, "*") &&
      is_call(e[[3]], "exp") &&
      is_eps(e[[3]][[2]]) &&
      fits(e[[2]])
  ) {
    return(form("exponential", e[[2]], as.character(e[[3]][[2]])))
  }
  NULL
}

# Dosing and scale assignments are PREDPP's and mrgsolve's, not variables:
# refuse them inside an `if` (where the representation cannot hold them) and
# refuse reading them as variables. `reserved(name)` says whether a name is
# one of them.
check_dosing_names <- function(individual, reserved, file, record) {
  for (x in individual) {
    for (y in nested_statements(x)) {
      target <- if (is_call(y, "<-")) expr_text(y[[2]])
      if (!is.null(target) && reserved(target)) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = target,
          message = sprintf(
            "not translated: %s assigned inside an if in %s",
            target,
            record
          ),
          call = NULL
        )
      }
    }
  }
  reads <- unique(unlist(lapply(individual, function(x) {
    if (is_call(x, "<-")) all.vars(x[[3]]) else all.vars(x)
  })))
  bad <- reads[vapply(reads, reserved, logical(1))]
  if (length(bad) > 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = bad[[1]],
      message = sprintf(
        "not translated: %s is read as a variable in %s",
        bad[[1]],
        record
      ),
      call = NULL
    )
  }
}

# A value with optional bounds, as $THETA and ini() give it: `init`,
# `c(lower, init)` or `c(lower, init, upper)`. NULL when there are not one
# to three values or they are out of order.
read_bounds <- function(values) {
  n <- length(values)
  if (n < 1 || n > 3 || anyNA(values)) {
    return(NULL)
  }
  out <- list(
    init = values[[if (n == 1) 1 else 2]],
    lower = if (n > 1) values[[1]] else -Inf,
    upper = if (n > 2) values[[3]] else Inf
  )
  if (out$lower > out$init || out$init > out$upper) {
    return(NULL)
  }
  out
}

# new_model() for a reader: a model the validator rejects was read from
# input, so the reader reports it as a parse error of that input.
read_model <- function(file, ...) {
  parts <- list(...)
  individual <- explicit_zero(parts$individual)
  if (
    length(individual) > length(parts$individual) &&
      identical(parts$provenance$format, "nonmem")
  ) {
    parts$provenance$losses <- c(
      parts$provenance$losses,
      "values kept from earlier records by variables assigned only inside IF (D14)"
    )
    parts$provenance$fidelity <- "lossy"
  }
  parts$individual <- individual
  if (identical(parts$structure$type, "ode")) {
    parts$structure$statements <- explicit_zero(parts$structure$statements)
  }
  parts$provenance$covariates <- model_covariates(file, parts)
  tryCatch(
    do.call(new_model, parts),
    zupkpmx_model_error = function(e) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = "the model as read",
        message = conditionMessage(e),
        call = NULL
      )
    }
  )
}

# A variable an `if` assigns before any statement outside it does starts at
# 0 in every evaluation (D14). Readers make that explicit with `x <- 0`
# before the `if`, so every writer prints the same model.
explicit_zero <- function(stmts) {
  out <- list()
  assigned <- character()
  for (x in stmts) {
    if (is_call(x, "if")) {
      for (v in setdiff(stmt_targets(list(x)), assigned)) {
        out[[length(out) + 1]] <- call("<-", as.symbol(v), 0)
        assigned <- c(assigned, v)
      }
    } else if (is_call(x, "<-")) {
      assigned <- c(assigned, as.character(x[[2]]))
    }
    out[[length(out) + 1]] <- x
  }
  out
}

# Free symbols of a model about to be built: what it reads but does not
# define. Undeclared THETA, ETA and EPS indices are refused; the rest are
# covariates, recorded in provenance. `t` is time (D15).
model_covariates <- function(file, parts) {
  s <- parts$structure
  defined <- c(
    parts$parameters$name,
    unlist(lapply(
      c(parts$random_effects$omega, parts$random_effects$sigma),
      `[[`,
      "names"
    )),
    model_compartments(s),
    stmt_targets(parts$individual),
    stmt_targets(s$statements),
    "t",
    "F"
  )
  exprs <- c(
    parts$individual,
    s$statements,
    s$derivatives,
    unlist(parts$dosing, recursive = FALSE),
    lapply(parts$observations, `[[`, "prediction"),
    lapply(parts$observations, function(o) o$residual$expression)
  )
  free <- setdiff(unique(unlist(lapply(exprs, all.vars))), defined)
  undeclared <- grep("^(THETA|ETA|EPS)[0-9]+$", free, value = TRUE)
  if (length(undeclared) > 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = undeclared[[1]],
      expected = "THETA, ETA and EPS indices the model declares",
      call = NULL
    )
  }
  sort(free)
}

# Name blocks by position: PREFIX1, PREFIX2, ... across all of them.
renumber_blocks <- function(blocks, prefix) {
  at <- 0
  lapply(blocks, function(b) {
    names <- paste0(prefix, at + seq_along(b$names))
    at <<- at + length(b$names)
    b$names <- names
    dimnames(b$matrix) <- list(names, names)
    if (!is.null(b$estimate)) {
      dimnames(b$estimate) <- list(names, names)
    }
    b
  })
}
