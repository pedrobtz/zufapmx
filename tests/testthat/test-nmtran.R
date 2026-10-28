test_that("assignments parse into the expression tree", {
  expect_equal(
    nm_parse_lines("CL = THETA(1) * EXP(ETA(1))"),
    list(quote(CL <- THETA1 * exp(ETA1)))
  )
  expect_equal(
    nm_parse_lines("  C2=A(2)/V"),
    list(quote(C2 <- CENTRAL / V))
  )
  expect_equal(
    nm_parse_lines("X = 1.5D-2 + .5E1 + 3"),
    list(quote(X <- 0.015 + 5 + 3))
  )
})

test_that("operator precedence follows Fortran", {
  expect_equal(nm_parse_lines("X = -A**2"), list(quote(X <- -A^2)))
  expect_equal(nm_parse_lines("X = A**B**C"), list(quote(X <- A^B^C)))
  expect_equal(nm_parse_lines("X = A - B - C"), list(quote(X <- A - B - C)))
  expect_equal(nm_parse_lines("X = A / (B + C)"), list(quote(X <- A / (B + C))))
  expect_equal(nm_parse_lines("X = -KA * A(1)"), list(quote(X <- -KA * DEPOT)))
})

test_that("conditions use Fortran and modern comparison operators", {
  stmts <- nm_parse_lines(c(
    "IF (WT.GT.70.AND..NOT.SEX.EQ.1) V = 2",
    "IF (WT >= 70 .OR. AGE /= 30) V = 3"
  ))
  expect_equal(
    stmts[[1]],
    quote(
      if (WT > 70 & !SEX == 1) {
        V <- 2
      }
    )
  )
  expect_equal(
    stmts[[2]],
    quote(
      if (WT >= 70 | AGE != 30) {
        V <- 3
      }
    )
  )
})

test_that("block IF, ELSE IF and ELSE nest", {
  stmts <- nm_parse_lines(c(
    "IF (SEX.EQ.1) THEN",
    "  V = 1",
    "ELSE IF (SEX.EQ.2) THEN",
    "  V = 2",
    "  K = 3",
    "ELSE",
    "  V = 4",
    "END IF"
  ))
  expect_equal(
    stmts,
    list(quote(
      if (SEX == 1) {
        V <- 1
      } else if (SEX == 2) {
        V <- 2
        K <- 3
      } else {
        V <- 4
      }
    ))
  )
  expect_silent(stmt_check(stmts[[1]]))
})

test_that("DADT and A_0 are statement targets", {
  expect_equal(
    nm_parse_lines(c("DADT(1) = -KA*A(1)", "A_0(2) = 10")),
    list(quote(DADT(1L) <- -KA * DEPOT), quote(A_0(2L) <- 10))
  )
})

test_that("verbatim code is refused with its line", {
  err <- expect_error(
    nm_parse_lines(c("CL = 1", "\"  CALL FOO")),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$line, 2L)
  expect_snapshot(cat(conditionMessage(err)))
})

test_that("a function outside the table is refused with its position", {
  err <- expect_error(
    nm_parse_lines("X = SIN(T)"),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$line, 1L)
  expect_equal(err$column, 5L)
  expect_equal(err$seen, "SIN")
})

test_that("unbalanced IF blocks and stray tokens are refused", {
  expect_error(
    nm_parse_lines(c("IF (A.GT.1) THEN", "B = 1")),
    class = "zupkpmx_parse_error"
  )
  expect_error(nm_parse_lines("ENDIF"), class = "zupkpmx_parse_error")
  expect_error(nm_parse_lines("X = (A + B"), class = "zupkpmx_parse_error")
  expect_error(nm_parse_lines("X = A B"), class = "zupkpmx_parse_error")
  expect_error(nm_parse_lines("CALL PASS(MODE)"), class = "zupkpmx_parse_error")
  expect_error(nm_parse_lines("X = A # B"), class = "zupkpmx_parse_error")
})

test_that("records split on $ and accept abbreviations", {
  split <- nm_split(
    c(
      "$PROB test ; title",
      "$INPUT ID TIME",
      "$SUB ADVAN2 TRANS2",
      "$PK",
      "CL = 1",
      "$EST METHOD=1"
    ),
    "test.ctl"
  )
  names <- vapply(split$records, `[[`, character(1), "name")

  expect_equal(names, c("PROBLEM", "INPUT", "SUBROUTINES", "PK", "ESTIMATION"))
  expect_true(split$comments)
  expect_equal(split$records[[4]]$lines, c("", "CL = 1"))
  expect_equal(split$records[[4]]$line, 4:5)
  expect_error(
    nm_split(c("$PROB x", "$FOO"), "t.ctl"),
    class = "zupkpmx_parse_error"
  )
})

test_that("NM-TRAN names are case-insensitive", {
  expect_equal(
    nm_parse_lines(c("Cl = Theta(2) * exp(Eta(1))", "k = CL / v")),
    list(quote(CL <- THETA2 * exp(ETA1)), quote(K <- CL / V))
  )
  expect_equal(
    nm_parse_lines(c("if (wt.gt.70) then", "x = 1", "endif")),
    list(quote(
      if (WT > 70) {
        X <- 1
      }
    ))
  )
})

test_that("record names need three letters, and an ambiguous one says so", {
  expect_error(
    nm_split(c("$PROB x", "$OM 0.1"), "t.ctl"),
    class = "zupkpmx_parse_error"
  )
  err <- expect_error(
    nm_split(c("$PROB x", "$S 0.1"), "t.ctl"),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "$S")
  err <- expect_error(
    nm_split(c("$PROB x", "$SI 0.1"), "t.ctl"),
    class = "zupkpmx_parse_error"
  )
  expect_equal(err$seen, "$SI")
})
