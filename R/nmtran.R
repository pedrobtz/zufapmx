# NM-TRAN control streams: records, a tokenizer and a recursive-descent
# parser for the abbreviated-code subset of design §4.2 (D6, D7). Parsed
# code is the package's expression tree (R/expr.R); tool names are kept as
# symbols, `THETA(1)` as `THETA1`, `ETA(1)` as `ETA1`, `EPS(1)` as `EPS1`.

nm_records <- c(
  "SIZES",
  "PROBLEM",
  "INPUT",
  "DATA",
  "SUBROUTINES",
  "MODEL",
  "PK",
  "DES",
  "ERROR",
  "THETA",
  "OMEGA",
  "SIGMA",
  "ESTIMATION",
  "COVARIANCE",
  "TABLE",
  "SCATTERPLOT",
  "SIMULATION",
  "ABBREVIATED",
  "PRED",
  "INFN",
  "MIX",
  "PRIOR"
)

nm_functions <- c(
  EXP = "exp",
  LOG = "log",
  LOG10 = "log10",
  SQRT = "sqrt",
  ABS = "abs"
)

# Split a control stream into records. Each record keeps its lines, with
# comments removed, and the file line number of each.
nm_split <- function(lines, file) {
  comments <- grepl(";", lines, fixed = TRUE)
  code <- sub(";.*$", "", lines)
  starts <- grep("^\\s*\\$[A-Za-z]", code)
  if (
    length(starts) == 0 || any(nzchar(trimws(code[seq_len(starts[[1]] - 1)])))
  ) {
    parse_error(
      file,
      1L,
      1L,
      seen = if (length(lines) > 0) trimws(lines[[1]]) else "an empty file",
      expected = "$PROBLEM",
      call = NULL
    )
  }
  ends <- c(starts[-1] - 1L, length(code))
  records <- lapply(seq_along(starts), function(i) {
    at <- starts[[i]]:ends[[i]]
    first <- sub("^\\s*\\$", "", code[[starts[[i]]]])
    word <- toupper(regmatches(first, regexpr("^[A-Za-z]+", first)))
    # NM-TRAN accepts a record name abbreviated to three letters or more;
    # $PK and $DES are whole names.
    name <- if (word %in% c("PK", "DES")) {
      word
    } else if (nchar(word) >= 3) {
      nm_records[startsWith(nm_records, word)]
    } else {
      character()
    }
    if (length(name) != 1) {
      parse_error(
        file,
        starts[[i]],
        1L,
        seen = paste0("$", word),
        expected = if (length(name) > 1) {
          sprintf(
            "one of %s, not an ambiguous abbreviation",
            paste0("$", name, collapse = ", ")
          )
        } else {
          "a supported record"
        },
        call = NULL
      )
    }
    text <- code[at]
    text[[1]] <- sub("^\\s*\\$[A-Za-z]+", "", text[[1]])
    # Options such as $ERROR (ONLY OBSERVATIONS) say when the record runs,
    # not what it computes.
    options <- character()
    if (name %in% c("PK", "DES", "ERROR")) {
      options <- regmatches(text[[1]], regexpr("^\\s*\\([^)]*\\)", text[[1]]))
      text[[1]] <- sub("^\\s*\\([^)]*\\)", "", text[[1]])
    }
    list(name = name, lines = text, line = at, options = trimws(options))
  })
  list(records = records, comments = any(comments))
}

# Tokens of one line: a list of list(type, value, column).
nm_tokenize <- function(text, line, file) {
  tokenize(
    text,
    line,
    c(
      space = "^\\s+",
      dotop = "(?i)^\\.(EQ|NE|LT|LE|GT|GE|AND|OR|NOT)\\.",
      number = "^([0-9]+(\\.(?![A-Za-z]{2,3}\\.)[0-9]*)?|\\.[0-9]+)([EeDd][+-]?[0-9]+)?",
      name = "^[A-Za-z_][A-Za-z0-9_]*",
      op = "^(\\*\\*|==|/=|<=|>=|[-+*/()=<>,])"
    ),
    file,
    "an operator, a name or a number",
    upper = c("dotop", "name")
  )
}

# A parser over one line's tokens. `cmt` maps a compartment number to its
# name, for `A(n)`.
nm_parser <- function(tokens, line, file, cmt) {
  s <- token_stream(
    tokens,
    file,
    end = list(value = "end of line", line = line)
  )
  peek <- s$peek
  at <- s$at
  take <- s$take
  fail <- s$fail
  binary <- s$binary

  compare_map <- c(
    `.EQ.` = "==",
    `==` = "==",
    `.NE.` = "!=",
    `/=` = "!=",
    `.LT.` = "<",
    `<` = "<",
    `.LE.` = "<=",
    `<=` = "<=",
    `.GT.` = ">",
    `>` = ">",
    `.GE.` = ">=",
    `>=` = ">="
  )

  primary <- function() {
    tok <- peek()
    if (is.null(tok)) {
      fail("an expression")
    }
    if (tok$type == "number") {
      take()
      return(as.numeric(sub("[Dd]", "E", tok$value)))
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
    name <- take()$value
    if (!at("(")) {
      return(as.symbol(tok$value))
    }
    take("(")
    args <- list(or_expr())
    while (at(",")) {
      take()
      args[[length(args) + 1]] <- or_expr()
    }
    take(")")
    index <- function() {
      if (
        length(args) != 1 ||
          !is.numeric(args[[1]]) ||
          args[[1]] != round(args[[1]]) ||
          args[[1]] < 1
      ) {
        parse_error(
          file,
          line,
          tok$column,
          seen = name,
          expected = sprintf("%s(n) with a whole number n", name),
          call = NULL
        )
      }
      as.integer(args[[1]])
    }
    switch(
      name,
      THETA = as.symbol(paste0("THETA", index())),
      ETA = as.symbol(paste0("ETA", index())),
      EPS = ,
      ERR = as.symbol(paste0("EPS", index())),
      A = as.symbol(cmt(index(), line, tok$column)),
      if (name %in% names(nm_functions) && length(args) == 1) {
        as.call(c(as.symbol(nm_functions[[name]]), args))
      } else {
        parse_error(
          file,
          line,
          tok$column,
          seen = name,
          expected = "a function of the table",
          call = NULL
        )
      }
    )
  }
  unary <- function() {
    if (at(c("-", "+"))) {
      return(call(take()$value, unary()))
    }
    power()
  }
  power <- function() {
    base <- primary()
    if (at("**")) {
      take()
      return(call("^", base, unary()))
    }
    base
  }
  multiplicative <- binary(unary, c("*", "/"))
  additive <- binary(multiplicative, c("+", "-"))
  comparison <- function() {
    left <- additive()
    if (at(names(compare_map))) {
      op <- compare_map[[take()$value]]
      left <- call(op, left, additive())
    }
    left
  }
  not_expr <- function() {
    if (at(".NOT.")) {
      take()
      return(call("!", not_expr()))
    }
    comparison()
  }
  and_expr <- binary(not_expr, ".AND.", c(`.AND.` = "&"))
  or_expr <- binary(and_expr, ".OR.", c(`.OR.` = "|"))

  list(
    peek = peek,
    at = at,
    take = take,
    fail = fail,
    expr = or_expr,
    done = s$done,
    expect_end = s$expect_end
  )
}

# Parse a record's abbreviated code into statements. Returns a list of
# R statements, with `DADT(n) = e` as `DADT(n) <- e` and dosing names such
# as `ALAG1` kept as symbols.
nm_code <- function(record, file, cmt) {
  out <- list()
  stack <- list()
  push <- function(stmt) {
    if (length(stack) == 0) {
      out[[length(out) + 1]] <<- stmt
    } else {
      top <- length(stack)
      branch <- stack[[top]]$branch
      stack[[top]]$bodies[[branch]][[
        length(stack[[top]]$bodies[[branch]]) + 1
      ]] <<- stmt
    }
  }

  for (k in seq_along(record$lines)) {
    text <- record$lines[[k]]
    line <- record$line[[k]]
    if (!nzchar(trimws(text))) {
      next
    }
    if (grepl("^\\s*\"", text)) {
      parse_error(
        file,
        line,
        1L,
        seen = "\"",
        message = sprintf("not translated: verbatim Fortran at line %d", line),
        call = NULL
      )
    }
    p <- nm_parser(nm_tokenize(text, line, file), line, file, cmt)

    if (p$at(c("ENDIF", "END"))) {
      p$take()
      if (p$at("IF")) {
        p$take()
      }
      p$expect_end()
      if (length(stack) == 0) {
        p$fail("a statement")
      }
      frame <- stack[[length(stack)]]
      stack[[length(stack)]] <- NULL
      push(nm_if_chain(frame))
      next
    }
    if (p$at("ELSE")) {
      p$take()
      if (length(stack) == 0) {
        p$fail("a statement")
      }
      top <- length(stack)
      if (p$at("IF")) {
        p$take()
        p$take("(")
        cond <- p$expr()
        p$take(")")
        p$take("THEN")
        stack[[top]]$conditions[[length(stack[[top]]$conditions) + 1]] <- cond
      }
      p$expect_end()
      stack[[top]]$branch <- stack[[top]]$branch + 1L
      stack[[top]]$bodies[[stack[[top]]$branch]] <- list()
      next
    }
    if (p$at("IF") && p$at("(", 1L)) {
      p$take()
      p$take("(")
      cond <- p$expr()
      p$take(")")
      if (p$at("THEN")) {
        p$take()
        p$expect_end()
        stack[[length(stack) + 1]] <- list(
          conditions = list(cond),
          bodies = list(list()),
          branch = 1L
        )
      } else {
        stmt <- nm_assignment(p, file, line)
        push(call("if", cond, as.call(list(as.symbol("{"), stmt))))
      }
      next
    }
    push(nm_assignment(p, file, line))
  }
  if (length(stack) > 0) {
    parse_error(
      file,
      max(record$line),
      NA_integer_,
      seen = "end of record",
      expected = "ENDIF",
      call = NULL
    )
  }
  # T ($DES) and TIME (data) are the representation's time, t (D15).
  lapply(out, rename_symbols, map = c(T = "t", TIME = "t"))
}

nm_assignment <- function(p, file, line) {
  tok <- p$peek()
  if (is.null(tok) || tok$type != "name") {
    p$fail("a statement")
  }
  name <- toupper(tok$value)
  if (name %in% c("CALL", "EXIT", "RETURN")) {
    p$fail("a statement of abbreviated code")
  }
  p$take()
  target <- if (p$at("(")) {
    p$take("(")
    n <- p$take()
    p$take(")")
    if (n$type != "number" || !name %in% c("DADT", "A_0")) {
      parse_error(
        file,
        line,
        tok$column,
        seen = name,
        expected = "DADT(n) or A_0(n)",
        call = NULL
      )
    }
    call(name, as.integer(n$value))
  } else {
    as.symbol(tok$value)
  }
  p$take("=")
  value <- p$expr()
  p$expect_end()
  call("<-", target, value)
}

# Build `if (c1) {...} else if (c2) {...} else {...}` from a frame.
nm_if_chain <- function(frame) {
  conditions <- frame$conditions
  bodies <- lapply(frame$bodies, function(b) as.call(c(as.symbol("{"), b)))
  build <- function(k) {
    if (k > length(conditions)) {
      return(if (k <= length(bodies)) bodies[[k]])
    }
    rest <- build(k + 1)
    if (is.null(rest)) {
      call("if", conditions[[k]], bodies[[k]])
    } else {
      call("if", conditions[[k]], bodies[[k]], rest)
    }
  }
  build(1)
}
