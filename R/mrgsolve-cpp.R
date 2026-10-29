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
      # Increment, decrement and compound assignment are tokens of their own
      # so that the parser can refuse them: `--x` is not `-(-x)`.
      op = "^(&&|\\|\\||==|!=|<=|>=|--|\\+\\+|[-+*/]=|[-+*/()=<>!,;{}])"
    ),
    file,
    "C++ of mrgsolve's model code"
  )
}

# Parse a block's code into statements. Time is `SOLVERTIME` in $ODE and
# the record's `TIME` elsewhere (D15); the other one is refused.
cpp_parse <- function(block, file, ode = FALSE) {
  tokens <- cpp_tokenize(block$lines, block$line, file)
  wrong_time <- if (ode) "TIME" else "SOLVERTIME"
  for (tok in tokens) {
    if (tok$type == "name" && tok$value == wrong_time) {
      parse_error(
        file,
        tok$line,
        tok$column,
        seen = wrong_time,
        message = sprintf(
          "not translated: `%s` in $%s at line %d",
          wrong_time,
          block$name,
          tok$line
        ),
        call = NULL
      )
    }
  }
  s <- token_stream(
    tokens,
    file,
    end = list(value = "end of block", line = max(block$line))
  )
  peek <- s$peek
  at <- s$at
  take <- s$take
  fail <- s$fail
  binary <- s$binary

  primary <- function() {
    tok <- peek()
    if (is.null(tok)) {
      fail("an expression")
    }
    if (tok$type == "number") {
      take()
      # A literal without a point or exponent is an int in C++; it stays
      # integer until cpp_integers() has checked the arithmetic.
      if (grepl("^[0-9]+$", tok$value)) {
        if (as.numeric(tok$value) > .Machine$integer.max) {
          parse_error(
            file,
            tok$line,
            tok$column,
            seen = tok$value,
            message = sprintf(
              "not translated: `%s` is wider than an int at line %d",
              tok$value,
              tok$line
            ),
            call = NULL
          )
        }
        return(as.integer(tok$value))
      }
      return(as.numeric(sub("[fF]$", "", tok$value)))
    }
    if (tok$value == "(") {
      take()
      inner <- or_expr()
      take(")")
      return(call("(", inner))
    }
    if (tok$type != "name") {
      fail("an expression")
    }
    take()
    if (!at("(")) {
      return(as.symbol(tok$value))
    }
    take("(")
    args <- list()
    if (!at(")")) {
      args <- list(or_expr())
      while (at(",")) {
        take()
        args[[length(args) + 1]] <- or_expr()
      }
    }
    take(")")
    name <- tok$value
    if (name %in% c("ETA", "EPS", "THETA")) {
      n <- whole_index(args, name, file, tok$line, tok$column)
      return(as.symbol(paste0(name, n)))
    }
    if (name == "pow" && length(args) == 2) {
      return(call("^", args[[1]], args[[2]]))
    }
    if (name %in% names(cpp_functions)) {
      arity <- expr_functions[[cpp_functions[[name]]]]
      if (length(args) != arity) {
        parse_error(
          file,
          tok$line,
          tok$column,
          seen = name,
          expected = sprintf("%s() with %d arguments", name, arity),
          call = NULL
        )
      }
      return(as.call(c(as.symbol(cpp_functions[[name]]), args)))
    }
    parse_error(
      file,
      tok$line,
      tok$column,
      seen = name,
      expected = "a function of mrgsolve's documented C++",
      call = NULL
    )
  }
  unary <- function() {
    if (at(c("--", "++", "+=", "-=", "*=", "/="))) {
      fail("C++ without increment, decrement or compound assignment")
    }
    if (at(c("-", "+", "!"))) {
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

  block_body <- function() {
    if (at("{")) {
      take()
      stmts <- list()
      while (!at("}")) {
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
    if (at(";")) {
      take()
      return(NULL)
    }
    if (at("if")) {
      take()
      take("(")
      cond <- or_expr()
      take(")")
      yes <- as.call(c(as.symbol("{"), block_body()))
      if (at("else")) {
        take()
        no <- if (at("if")) {
          statement()
        } else {
          as.call(c(as.symbol("{"), block_body()))
        }
        return(call("if", cond, yes, no))
      }
      return(call("if", cond, yes))
    }
    if (at("int")) {
      fail("double: an int declaration truncates")
    }
    while (at(c("double", "bool", "capture", "const", "localdouble"))) {
      take()
    }
    tok <- peek()
    if (is.null(tok) || tok$type != "name" || !at("=", 1L)) {
      fail("a declaration or an assignment")
    }
    take()
    take("=")
    value <- or_expr()
    take(";")
    call("<-", as.symbol(tok$value), value)
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
# are int, and so is a comparison or logical result (a bool); dividing two
# ints truncates (3/4 is 0, `(1 > 0) / 2` too), which the representation
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
  bool <- op %in% c("==", "!=", "<", ">", "<=", ">=", "&", "|", "!")
  list(
    expr = x,
    int = bool || op %in% c("+", "-", "*", "(") && length(ints) > 0 && all(ints)
  )
}
