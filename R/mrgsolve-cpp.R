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
    while (is(c("double", "int", "bool", "capture", "const", "localdouble"))) {
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
      out[[length(out) + 1]] <- s
    }
  }
  out
}
