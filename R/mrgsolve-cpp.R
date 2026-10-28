# The C++ mrgsolve documents for model code (design §4.2): declarations,
# assignments and if/else over arithmetic, comparisons and a fixed set of
# functions, parsed into the package's expression tree. `ETA(n)` reads as
# `ETA1`, `EPS(n)` as `EPS1`, `THETA(n)` as `THETA1`; `pow(a, b)` as
# `a^b`; `fabs`, `fmin` and `fmax` as `abs`, `min` and `max`.

cpp_functions <- c(
  exp = "exp",
  log = "log",
  log10 = "log10",
  sqrt = "sqrt",
  fabs = "abs",
  abs = "abs",
  fmin = "min",
  fmax = "max"
)

cpp_tokenize <- function(lines, line_numbers, file) {
  tokenize(
    lines,
    line_numbers,
    c(
      space = "^\\s+",
      number = "^([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?[fF]?",
      name = "^[A-Za-z_][A-Za-z0-9_]*",
      op = "^(&&|\\|\\||==|!=|<=|>=|[-+*/()=<>!,;{}])"
    ),
    file,
    "C++ of mrgsolve's model code"
  )
}

# Parse a block's code into statements.
cpp_parse <- function(block, file) {
  tokens <- cpp_tokenize(block$lines, block$line, file)
  s <- token_stream(
    tokens,
    file,
    end = list(value = "end of block", line = max(block$line))
  )
  peek <- s$peek
  is <- s$is
  take <- s$take
  fail <- s$fail
  binary <- s$binary

  primary <- function() {
    t <- peek()
    if (is.null(t)) {
      fail("an expression")
    }
    if (t$type == "number") {
      take()
      # A literal without a point or exponent is an int in C++; it stays
      # integer until cpp_integers() has checked the arithmetic.
      if (grepl("^[0-9]+$", t$value)) {
        return(as.integer(t$value))
      }
      return(as.numeric(sub("[fF]$", "", t$value)))
    }
    if (t$value == "(") {
      take()
      inner <- or_expr()
      take(")")
      return(call("(", inner))
    }
    if (t$type != "name") {
      fail("an expression")
    }
    take()
    if (!is("(")) {
      return(as.symbol(t$value))
    }
    take("(")
    args <- list()
    if (!is(")")) {
      args <- list(or_expr())
      while (is(",")) {
        take()
        args[[length(args) + 1]] <- or_expr()
      }
    }
    take(")")
    name <- t$value
    if (name %in% c("ETA", "EPS", "THETA")) {
      if (length(args) != 1 || !is.numeric(args[[1]])) {
        parse_error(
          file,
          t$line,
          t$column,
          seen = name,
          expected = sprintf("%s(n)", name),
          call = NULL
        )
      }
      return(as.symbol(paste0(name, as.integer(args[[1]]))))
    }
    if (name == "pow" && length(args) == 2) {
      return(call("^", args[[1]], args[[2]]))
    }
    if (name %in% names(cpp_functions)) {
      arity <- expr_functions[[cpp_functions[[name]]]]
      if (length(args) != arity) {
        parse_error(
          file,
          t$line,
          t$column,
          seen = name,
          expected = sprintf("%s() with %d arguments", name, arity),
          call = NULL
        )
      }
      return(as.call(c(as.symbol(cpp_functions[[name]]), args)))
    }
    parse_error(
      file,
      t$line,
      t$column,
      seen = name,
      expected = "a function of mrgsolve's documented C++",
      call = NULL
    )
  }
  unary <- function() {
    if (is(c("-", "+", "!"))) {
      return(call(take()$value, unary()))
    }
    primary()
  }
  multiplicative <- binary(unary, c("*", "/"))
  additive <- binary(multiplicative, c("+", "-"))
  relational <- binary(additive, c("<", "<=", ">", ">="))
  equality <- binary(relational, c("==", "!="))
  and_expr <- binary(equality, "&&", c(`&&` = "&"))
  or_expr <- binary(and_expr, "||", c(`||` = "|"))

  body <- function() {
    if (is("{")) {
      take()
      stmts <- list()
      while (!is("}")) {
        if (is.null(peek())) {
          fail("}")
        }
        s <- statement()
        if (!is.null(s)) {
          stmts[[length(stmts) + 1]] <- s
        }
      }
      take("}")
      return(stmts)
    }
    s <- statement()
    if (is.null(s)) list() else list(s)
  }
  statement <- function() {
    if (is(";")) {
      take()
      return(NULL)
    }
    if (is("if")) {
      take()
      take("(")
      cond <- or_expr()
      take(")")
      yes <- as.call(c(as.symbol("{"), body()))
      if (is("else")) {
        take()
        no <- if (is("if")) statement() else as.call(c(as.symbol("{"), body()))
        return(call("if", cond, yes, no))
      }
      return(call("if", cond, yes))
    }
    if (is("int")) {
      fail("double: an int declaration truncates")
    }
    while (is(c("double", "bool", "capture", "const", "localdouble"))) {
      take()
    }
    t <- peek()
    if (is.null(t) || t$type != "name" || !is("=", 1L)) {
      fail("a declaration or an assignment")
    }
    take()
    take("=")
    value <- or_expr()
    take(";")
    call("<-", as.symbol(t$value), value)
  }

  out <- list()
  while (!is.null(peek())) {
    s <- statement()
    if (!is.null(s)) {
      out[[length(out) + 1]] <- cpp_integers(s, file)$expr
    }
  }
  out
}

# Integer arithmetic as C++ does it. Integer literals and +, -, * of them
# are int; dividing two ints truncates (3/4 is 0), which the representation
# cannot hold, so it is refused. Everything else becomes double.
cpp_integers <- function(x, file) {
  if (is.integer(x)) {
    return(list(expr = as.numeric(x), int = TRUE))
  }
  if (!is.call(x)) {
    return(list(expr = x, int = FALSE))
  }
  parts <- lapply(as.list(x)[-1], cpp_integers, file = file)
  x[-1] <- lapply(parts, `[[`, "expr")
  op <- as.character(x[[1]])
  ints <- vapply(parts, `[[`, logical(1), "int")
  if (op == "/" && all(ints)) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = expr_text(x),
      message = sprintf(
        "not translated: `%s` is integer division in C++",
        expr_text(x)
      ),
      call = NULL
    )
  }
  list(
    expr = x,
    int = op %in% c("+", "-", "*", "(") && length(ints) > 0 && all(ints)
  )
}
