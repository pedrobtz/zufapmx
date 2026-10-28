# Print an expression tree in another language's syntax. A dialect gives
# the operator spellings, the function names, how to write a number, and
# whether powers are an operator or a function (C++'s pow()). Parentheses
# are added where the target's precedence needs them; the precedence of
# R, Fortran and C++ agrees for every operator in the table.

expr_precedence <- function(x) {
  if (!is.call(x)) {
    return(9L)
  }
  op <- as.character(x[[1]])
  unary <- length(x) == 2
  switch(
    op,
    `|` = 1L,
    `&` = 2L,
    `!` = 3L,
    `==` = ,
    `!=` = ,
    `<` = ,
    `<=` = ,
    `>` = ,
    `>=` = 4L,
    `+` = ,
    `-` = if (unary) 7L else 5L,
    `*` = ,
    `/` = 6L,
    `^` = 8L,
    9L
  )
}

expr_print <- function(x, names, dialect) {
  refuse <- function(what) {
    write_error(what, "expression", formats = dialect$formats, call = NULL)
  }
  if (is.numeric(x)) {
    return(dialect$number(x))
  }
  if (is.symbol(x)) {
    name <- as.character(x)
    if (!name %in% names(names)) {
      refuse(sprintf("the name `%s`", name))
    }
    return(names[[name]])
  }
  op <- as.character(x[[1]])
  p <- expr_precedence(x)
  wrap <- function(e, needs) {
    text <- expr_print(e, names, dialect)
    if (needs) paste0("(", text, ")") else text
  }
  if (op == "(") {
    return(paste0("(", expr_print(x[[2]], names, dialect), ")"))
  }
  if (op == "!") {
    return(paste0(dialect$not, wrap(x[[2]], expr_precedence(x[[2]]) < p)))
  }
  if (op %in% c("-", "+") && length(x) == 2) {
    return(paste0(op, wrap(x[[2]], expr_precedence(x[[2]]) < p)))
  }
  if (op == "^" && !is.null(dialect$power)) {
    return(sprintf(
      "%s(%s, %s)",
      dialect$power,
      expr_print(x[[2]], names, dialect),
      expr_print(x[[3]], names, dialect)
    ))
  }
  if (op %in% names(dialect$operators)) {
    left <- expr_precedence(x[[2]])
    right <- expr_precedence(x[[3]])
    return(paste(
      wrap(x[[2]], left < p || (op == "^" && left <= p)),
      dialect$operators[[op]],
      wrap(x[[3]], right < p || (op %in% c("-", "/") && right == p))
    ))
  }
  if (!op %in% names(dialect$functions)) {
    refuse(sprintf("`%s()`, which %s has no function for", op, dialect$name))
  }
  args <- vapply(
    as.list(x)[-1],
    expr_print,
    character(1),
    names = names,
    dialect = dialect
  )
  sprintf("%s(%s)", dialect$functions[[op]], paste(args, collapse = ", "))
}
