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
  patterns <- c(
    space = "^\\s+",
    number = "^([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?[fF]?",
    name = "^[A-Za-z_][A-Za-z0-9_]*",
    op = "^(&&|\\|\\||==|!=|<=|>=|[-+*/()=<>!,;{}])"
  )
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
            tokens[[length(tokens) + 1]] <- list(
              type = type,
              value = m,
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
          expected = "C++ of mrgsolve's model code",
          call = NULL
        )
      }
    }
  }
  tokens
}

# Parse a block's code into statements.
mrg_code <- function(block, file) {
  tokens <- cpp_tokenize(block$lines, block$line, file)
  i <- 1L
  peek <- function(k = 0L) if (i + k <= length(tokens)) tokens[[i + k]]
  is <- function(value, k = 0L) {
    t <- peek(k)
    !is.null(t) && t$value %in% value
  }
  fail <- function(expected) {
    t <- peek()
    if (is.null(t)) {
      t <- list(
        value = "end of block",
        line = max(block$line),
        column = NA_integer_
      )
    }
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
      fail(if (is.null(value)) "more code" else paste(value, collapse = " or "))
    }
    i <<- i + 1L
    t
  }
  binary <- function(next_level, ops, map) {
    function() {
      left <- next_level()
      while (is(ops)) {
        op <- take()$value
        left <- call(map[[op]], left, next_level())
      }
      left
    }
  }
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
  multiplicative <- binary(unary, c("*", "/"), c(`*` = "*", `/` = "/"))
  additive <- binary(multiplicative, c("+", "-"), c(`+` = "+", `-` = "-"))
  relational <- binary(
    additive,
    c("<", "<=", ">", ">="),
    c(`<` = "<", `<=` = "<=", `>` = ">", `>=` = ">=")
  )
  equality <- binary(relational, c("==", "!="), c(`==` = "==", `!=` = "!="))
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
