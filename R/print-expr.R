# Print an expression tree in another language's syntax. A dialect gives
# the operator spellings, the function names, how to write a number, and
# whether powers are an operator or a function (C++'s pow()). Parentheses
# are added where the target's precedence needs them. The precedence of R,
# Fortran and C++ agrees for the binary operators in the table; two cases
# are made safe for every dialect by parenthesising: the operand of `!`
# when it is a call (C++'s `!` binds tighter than `==`), and a unary
# operand next to another operator, so no output holds two operators in a
# row (`--x` is C++'s decrement; Fortran forbids `X * -2`).

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

is_unary <- function(x) {
  is.call(x) && length(x) == 2 && as.character(x[[1]]) %in% c("-", "+")
}

expr_print <- function(x, spelling, dialect) {
  refuse <- function(what) {
    write_error(what, "expression", formats = dialect$formats, call = NULL)
  }
  if (is.numeric(x)) {
    return(dialect$number(x))
  }
  if (is.symbol(x)) {
    name <- as.character(x)
    if (!name %in% names(spelling)) {
      refuse(sprintf("the name `%s`", name))
    }
    return(spelling[[name]])
  }
  op <- as.character(x[[1]])
  p <- expr_precedence(x)
  wrap <- function(e, needs) {
    text <- expr_print(e, spelling, dialect)
    if (needs) paste0("(", text, ")") else text
  }
  if (op == "(") {
    return(paste0("(", expr_print(x[[2]], spelling, dialect), ")"))
  }
  if (op == "!") {
    return(paste0(
      dialect$not,
      wrap(x[[2]], is.call(x[[2]]) && !is_call(x[[2]], "("))
    ))
  }
  if (op %in% c("-", "+") && length(x) == 2) {
    return(paste0(op, wrap(x[[2]], expr_precedence(x[[2]]) <= p)))
  }
  if (op == "^" && !is.null(dialect$power)) {
    return(sprintf(
      "%s(%s, %s)",
      dialect$power,
      expr_print(x[[2]], spelling, dialect),
      expr_print(x[[3]], spelling, dialect)
    ))
  }
  if (op %in% names(dialect$operators)) {
    left <- expr_precedence(x[[2]])
    right <- expr_precedence(x[[3]])
    return(paste(
      wrap(x[[2]], left < p || (op == "^" && left <= p)),
      dialect$operators[[op]],
      wrap(
        x[[3]],
        right < p || (op %in% c("-", "/") && right == p) || is_unary(x[[3]])
      )
    ))
  }
  if (!op %in% names(dialect$functions)) {
    refuse(sprintf("`%s()`, which %s has no function for", op, dialect$name))
  }
  args <- vapply(
    as.list(x)[-1],
    expr_print,
    character(1),
    spelling = spelling,
    dialect = dialect
  )
  sprintf("%s(%s)", dialect$functions[[op]], paste(args, collapse = ", "))
}
