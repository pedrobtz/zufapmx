test_that("corpus mrgsolve models round-trip exactly", {
  for (path in zupkpmx_corpus()$path) {
    w <- pmx_write_mrgsolve(pmx_read_mrgsolve(file.path(path, "model.cpp")))
    expect_equal(pmx_fidelity(w), "exact", label = basename(path))
  }
  expect_snapshot(print(pmx_write_mrgsolve(corpus_rxode2_model("twocmt-oral"))))
  expect_snapshot(print(pmx_write_mrgsolve(corpus_rxode2_model(
    "onecmt-oral-mm"
  ))))
})

test_that("every corpus model writes to every format and back", {
  writers <- list(
    rxode2 = pmx_write_rxode2,
    nonmem = pmx_write_nonmem,
    mrgsolve = pmx_write_mrgsolve
  )
  readers <- list(
    rxode2 = function(text) pmx_read_rxode2(eval(parse(text = text))),
    nonmem = function(text) {
      path <- withr::local_tempfile(fileext = ".ctl")
      writeLines(text, path)
      pmx_read_nonmem(path)
    },
    mrgsolve = function(text) {
      path <- withr::local_tempfile(fileext = ".cpp")
      writeLines(text, path)
      pmx_read_mrgsolve(path)
    }
  )
  corpus <- zupkpmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    models <- corpus_models(corpus$path[[i]], corpus$id[[i]])
    for (from in names(models)) {
      for (to in names(writers)) {
        w <- writers[[to]](models[[from]])
        back <- writers[[from]](readers[[to]](w$text))
        label <- sprintf("%s: %s -> %s -> %s", corpus$id[[i]], from, to, from)
        expect_true(
          pmx_fidelity(w) %in% c("exact", "equivalent"),
          label = label
        )
        expect_true(
          pmx_fidelity(back) %in% c("exact", "equivalent"),
          label = label
        )
      }
    }
  }
})

test_that("written mrgsolve models simulate to their expected predictions", {
  skip_on_cran()
  skip_if_not_installed("mrgsolve")
  corpus <- zupkpmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    models <- corpus_models(corpus$path[[i]], corpus$id[[i]])
    expected <- corpus_expected(corpus$path[[i]])$pred
    for (from in names(models)) {
      path <- withr::local_tempfile(fileext = ".cpp")
      writeLines(pmx_write_mrgsolve(models[[from]])$text, path)
      got <- corpus_solve_mrgsolve(path, corpus$regimen[[i]])
      expect_lte(
        corpus_max_rel_dev(got, expected),
        1e-6,
        label = paste(corpus$id[[i]], from)
      )
    }
  }
})

test_that("C++ is printed with double literals, pow() and else if", {
  m <- read_cpp(cpp_onecmt(
    main = c(
      "double WTF = 0.0;",
      "if (WT > 70 && !(SEX == 1)) {",
      "  WTF = pow(WT / 70.0, 0.75) + 1 / 2;",
      "} else if (WT > 50) {",
      "  WTF = fmin(fabs(WT), 1.0);",
      "} else {",
      "  WTF = 1;",
      "}"
    )
  ))
  text <- pmx_write_mrgsolve(m)$text

  expect_true("  WTF = pow(WT / 70.0, 0.75) + 1.0 / 2.0;" %in% text)
  expect_true("} else if (WT > 50.0) {" %in% text)
  expect_true("  WTF = fmin(fabs(WT), 1.0);" %in% text)
  expect_equal(pmx_fidelity(pmx_write_mrgsolve(m)), "exact")
})

test_that("estimates and fixed blocks are recorded as losses", {
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run1.ctl"), "ext")
  expect_match(pmx_losses(pmx_write_mrgsolve(m)), "^initial values")

  fixed <- read_ctl(ctl_onecmt(extra = character()))
  re <- unclass(fixed)$random_effects
  re$omega[[1]]$fixed <- TRUE
  w <- pmx_write_mrgsolve(model_with(fixed, random_effects = re))
  expect_true("fixed flags of $OMEGA and $SIGMA blocks" %in% pmx_losses(w))
})

test_that("constructs mrgsolve cannot hold are refused", {
  parts <- unclass(corpus_rxode2_model("twocmt-oral"))

  two <- parts
  two$observations$cp2 <- two$observations$cp
  expect_error(
    pmx_write_mrgsolve(do.call(new_model, two)),
    class = "zupkpmx_write_error"
  )

  pow <- parts
  pow$observations$cp$residual <- list(
    type = "power",
    parameters = c("prop.sd", "tq")
  )
  expect_error(
    pmx_write_mrgsolve(do.call(new_model, pow)),
    class = "zupkpmx_write_error"
  )

  reserved <- parts
  reserved$individual <- c(reserved$individual, list(quote(TIME <- 1)))
  expect_error(
    pmx_write_mrgsolve(do.call(new_model, reserved)),
    class = "zupkpmx_write_error"
  )
})

test_that("every dosing attribute round-trips through every writer", {
  m <- pmx_read_rxode2(function() {
    ini({
      tka <- c(0, 1)
      tcl <- c(0, 4)
      tv <- c(0, 50)
      sd <- c(0, 0.2)
    })
    model({
      ka <- tka
      cl <- tcl
      v <- tv
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      alag(depot) <- 0.5
      f(depot) <- 0.8
      dur(central) <- 2
      rate(central) <- 3
      central(0) <- 1
      cp <- central / v
      cp ~ prop(sd)
    })
  })
  dosing <- unclass(m)$dosing

  for (write in list(pmx_write_rxode2, pmx_write_nonmem, pmx_write_mrgsolve)) {
    w <- write(m)
    expect_true(pmx_fidelity(w) %in% c("exact", "equivalent"))
  }
  nm <- pmx_write_nonmem(m)$text
  expect_true(all(
    c("ALAG1 = 0.5", "F1 = 0.8", "D2 = 2", "R2 = 3", "A_0(2) = 1") %in% nm
  ))
  mrg <- pmx_write_mrgsolve(m)$text
  expect_true(all(
    c(
      "ALAG_depot = 0.5;",
      "F_depot = 0.8;",
      "D_central = 2.0;",
      "R_central = 3.0;",
      "central_0 = 1.0;"
    ) %in%
      mrg
  ))
  back <- read_cpp(mrg)
  expect_equal(names(unclass(back)$dosing), names(dosing))
})
