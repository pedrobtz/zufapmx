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
