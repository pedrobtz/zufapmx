# Expressions (design §4.5) are R language objects restricted to numeric
# constants, names, the operators below and calls to the function table. One
# tree type serves every format: each reader builds it and each printer walks
# it. Statements are `name <- expr` or `if (cond) {...} else {...}`.

# Function table: name and the number of arguments it takes.
expr_functions <- list(
  exp = 1L,
  log = 1L,
  log10 = 1L,
  sqrt = 1L,
  abs = 1L,
  min = 2L,
  max = 2L
)

# Operators and the numbers of arguments each may take.
expr_operators <- list(
  `+` = 1:2,
  `-` = 1:2,
  `*` = 2L,
  `/` = 2L,
  `^` = 2L,
  `(` = 1L,
  `==` = 2L,
  `!=` = 2L,
  `<` = 2L,
  `<=` = 2L,
  `>` = 2L,
  `>=` = 2L,
  `&` = 2L,
  `|` = 2L,
  `!` = 1L
)

expr_check <- function(x) {
  if (is.symbol(x)) {
    return(invisible(x))
  }
  if (is.numeric(x)) {
    if (length(x) != 1 || is.na(x)) {
      zufapmx_abort("A constant must be one number.", "model")
    }
    return(invisible(x))
  }
  if (!is.call(x) || !is.symbol(x[[1]])) {
    zufapmx_abort(
      sprintf("`%s` is not an expression.", expr_text(x)),
      "model"
    )
  }

  fn <- as.character(x[[1]])
  arity <- c(expr_operators, expr_functions)[[fn, exact = TRUE]]
  if (is.null(arity)) {
    zufapmx_abort(
      sprintf("`%s()` is not in the function table.", fn),
      "model"
    )
  }
  args <- as.list(x)[-1]
  if (!length(args) %in% arity) {
    zufapmx_abort(
      sprintf(
        "`%s()` takes %s arguments, not %d.",
        fn,
        paste(arity, collapse = " or "),
        length(args)
      ),
      "model"
    )
  }
  lapply(args, expr_check)
  invisible(x)
}

stmt_check <- function(x) {
  if (is.call(x) && identical(x[[1]], as.symbol("<-"))) {
    if (!is.symbol(x[[2]])) {
      zufapmx_abort(
        sprintf("`%s` does not assign to a name.", expr_text(x)),
        "model"
      )
    }
    expr_check(x[[3]])
    return(invisible(x))
  }
  if (is.call(x) && identical(x[[1]], as.symbol("if"))) {
    expr_check(x[[2]])
    for (body in as.list(x)[-(1:2)]) {
      lapply(stmt_body(body), stmt_check)
    }
    return(invisible(x))
  }
  zufapmx_abort(
    sprintf("`%s` is not an assignment or an `if` statement.", expr_text(x)),
    "model"
  )
}

stmt_targets <- function(stmts) {
  targets <- lapply(stmts, function(x) {
    if (identical(x[[1]], as.symbol("<-"))) {
      as.character(x[[2]])
    } else {
      unlist(lapply(as.list(x)[-(1:2)], function(b) stmt_targets(stmt_body(b))))
    }
  })
  unique(unlist(targets, use.names = FALSE))
}

# The statements of an `if` branch, with or without braces.
stmt_body <- function(x) {
  if (is.call(x) && identical(x[[1]], as.symbol("{"))) {
    as.list(x)[-1]
  } else {
    list(x)
  }
}

expr_text <- function(x) {
  paste(deparse(x, width.cutoff = 500L), collapse = " ")
}
