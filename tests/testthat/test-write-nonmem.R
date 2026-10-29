test_that("corpus control streams round-trip exactly", {
  for (path in zufapmx_corpus()$path) {
    w <- pmx_write_nonmem(pmx_read_nonmem(file.path(path, "model.ctl")))
    expect_equal(pmx_fidelity(w), "exact", label = basename(path))
    expect_equal(pmx_losses(w), character(), label = basename(path))
  }
})

test_that("rxode2 corpus models write as equivalent control streams", {
  for (id in zufapmx_corpus()$id) {
    w <- pmx_write_nonmem(corpus_rxode2_model(id))
    expect_equal(pmx_fidelity(w), "equivalent", label = id)
  }
})

test_that("models round-trip between rxode2 and NONMEM and still predict", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    path <- withr::local_tempfile(fileext = ".ctl")
    expected <- corpus_expected(corpus$path[[i]])$pred

    # rxode2 -> NONMEM -> rxode2
    rx <- corpus_rxode2_model(corpus$id[[i]])
    to_nm <- pmx_write_nonmem(rx)
    writeLines(to_nm$text, path)
    back <- pmx_write_rxode2(pmx_read_nonmem(path))
    expect_true(
      pmx_fidelity(back) %in% c("exact", "equivalent"),
      label = corpus$id[[i]]
    )
    got <- corpus_solve_rxode2(
      eval(parse(text = back$text)),
      corpus$regimen[[i]],
      "Y"
    )
    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])

    # NONMEM -> rxode2 -> NONMEM
    nm <- pmx_read_nonmem(file.path(corpus$path[[i]], "model.ctl"))
    to_rx <- pmx_write_rxode2(nm)
    again <- pmx_write_nonmem(pmx_read_rxode2(eval(parse(text = to_rx$text))))
    expect_equal(pmx_fidelity(to_rx), "equivalent", label = corpus$id[[i]])
    expect_true(
      pmx_fidelity(again) %in% c("exact", "equivalent"),
      label = corpus$id[[i]]
    )
  }
})

test_that("estimates are written as final or initial values, and the other is lost", {
  ctl <- test_path("fixtures", "nonmem", "run1.ctl")
  m <- pmx_read_nonmem(ctl, "ext")

  final <- pmx_write_nonmem(m)
  expect_equal(pmx_fidelity(final), "lossy")
  expect_match(pmx_losses(final), "^initial values")
  expect_true("$THETA (0, 1.12) ; THETA1" %in% final$text)
  expect_true("$OMEGA 0.084" %in% final$text)

  initial <- pmx_write_nonmem(m, estimates = "initial")
  expect_match(pmx_losses(initial), "^final estimates")
  expect_true("$THETA (0, 1) ; THETA1" %in% initial$text)
})

test_that("a model without estimates writes its initial values and loses nothing", {
  w <- pmx_write_nonmem(pmx_read_nonmem(corpus_file(
    "onecmt-oral",
    "model.ctl"
  )))
  expect_equal(pmx_losses(w), character())
})

test_that("IF blocks, dosing and bounds round-trip", {
  m <- read_ctl(ctl_onecmt(
    pk = c(
      "ALAG1 = THETA(1) / 10",
      "F1 = 0.8",
      "IF (WT.GT.70.AND.SEX.EQ.1) THEN",
      "  WTF = 1.2",
      "ELSE IF (WT.GT.50) THEN",
      "  WTF = 1.1",
      "ELSE",
      "  WTF = 1",
      "ENDIF",
      "IF (AGE.LT.18) WTF = WTF * 0.9"
    ),
    error = c("IPRED = F", "Y = IPRED * (1 + EPS(1)) + EPS(2)"),
    extra = "$SIGMA 0.01 FIX"
  ))

  expect_equal(pmx_fidelity(pmx_write_nonmem(m)), "exact")
})

test_that("expressions print with Fortran precedence", {
  names <- c(a = "A", b = "B", c = "C")
  expect_equal(nm_expr(quote(a / (b * c)), names), "A / (B * C)")
  expect_equal(
    nm_expr(call("/", quote(a), call("*", quote(b), quote(c))), names),
    "A / (B * C)"
  )
  expect_equal(
    nm_expr(call("-", quote(a), call("-", quote(b), quote(c))), names),
    "A - (B - C)"
  )
  expect_equal(
    nm_expr(call("^", call("^", quote(a), quote(b)), quote(c)), names),
    "(A ** B) ** C"
  )
  expect_equal(nm_expr(quote(a^-b), names), "A ** (-B)")
  expect_equal(
    nm_expr(quote(!(a > 1) | b <= c), names),
    ".NOT.(A .GT. 1.0) .OR. B .LE. C"
  )
  expect_equal(nm_expr(quote(exp(log10(a))), names), "EXP(LOG10(A))")
})

test_that("constructs NM-TRAN cannot hold are refused", {
  m <- corpus_rxode2_model("twocmt-oral")
  parts <- unclass(m)

  two <- parts
  two$observations$cp2 <- two$observations$cp
  expect_error(
    pmx_write_nonmem(do.call(new_model, two)),
    class = "zufapmx_write_error"
  )

  pow <- parts
  pow$observations$cp$residual <- list(
    type = "power",
    parameters = c("prop.sd", "tq")
  )
  expect_error(
    pmx_write_nonmem(do.call(new_model, pow)),
    class = "zufapmx_write_error"
  )

  mx <- parts
  mx$individual[[1]] <- quote(ka <- max(tka, 0.1))
  expect_error(
    pmx_write_nonmem(do.call(new_model, mx)),
    class = "zufapmx_write_error"
  )

  clash <- parts
  clash$individual <- c(
    clash$individual,
    list(quote(cl.a <- 1), quote(cl_a <- 2))
  )
  expect_error(
    pmx_write_nonmem(do.call(new_model, clash)),
    class = "zufapmx_write_error"
  )

  iov <- parts
  iov$random_effects$iov <- list(present = TRUE, occasion = "OCC")
  expect_error(
    pmx_write_nonmem(do.call(new_model, iov)),
    class = "zufapmx_write_error"
  )
})

test_that("variables PREDPP reserves are renamed", {
  m <- pmx_read_rxode2(function() {
    ini({
      tka <- c(0, 1)
      tcl <- c(0, 4)
      tv <- c(0, 50)
      tf <- 0.1
      sd <- c(0, 0.2)
    })
    model({
      f1 <- tf
      mtime1 <- 2
      ka <- tka * (1 + f1)
      cl <- tcl
      v <- tv
      cp <- linCmt()
      cp ~ prop(sd)
    })
  })
  text <- pmx_write_nonmem(m)$text

  expect_true(all(
    c("F1_ = THETA(4)", "MTIME1_ = 2.0", "KA = THETA(1) * (1.0 + F1_)") %in%
      text
  ))
  expect_false(any(grepl("^F1 =", text)))
})

test_that("mrgsolve models written to NONMEM and on to rxode2 still predict", {
  skip_on_cran()
  skip_if_not_installed("rxode2")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    path <- withr::local_tempfile(fileext = ".ctl")
    writeLines(
      pmx_write_nonmem(pmx_read_mrgsolve(file.path(
        corpus$path[[i]],
        "model.cpp"
      )))$text,
      path
    )
    fun <- eval(parse(text = pmx_write_rxode2(pmx_read_nonmem(path))$text))
    got <- corpus_solve_rxode2(fun, corpus$regimen[[i]], "Y")
    expected <- corpus_expected(corpus$path[[i]])$pred

    expect_lte(corpus_max_rel_dev(got, expected), 1e-6, label = corpus$id[[i]])
  }
})

test_that("whole numbers in NM-TRAN expressions carry a point", {
  expect_equal(nm_expr(quote(3 / 4 * x), c(x = "X")), "3.0 / 4.0 * X")
})

test_that("written text is stable", {
  expect_snapshot(print(pmx_write_nonmem(corpus_rxode2_model("twocmt-oral"))))
  expect_snapshot(print(pmx_write_nonmem(corpus_rxode2_model(
    "onecmt-oral-mm"
  ))))
})
