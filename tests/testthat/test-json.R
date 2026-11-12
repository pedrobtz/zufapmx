json_round_trip <- function(model, env = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  pmx_write_json(model, file)
  pmx_read_json(file)
}

test_that("every corpus model round-trips through JSON exactly", {
  skip_if_no_engine("zujson")
  corpus <- zufapmx_corpus()

  for (i in seq_len(nrow(corpus))) {
    models <- corpus_models(corpus$path[[i]], corpus$id[[i]])
    for (from in names(models)) {
      expect_identical(
        json_round_trip(models[[from]]),
        models[[from]],
        label = paste(corpus$id[[i]], from)
      )
    }
  }
})

test_that("estimates, blocks, bounds and fixed flags survive JSON", {
  skip_if_no_engine("zujson")
  m <- pmx_read_nonmem(test_path("fixtures", "nonmem", "run2.ctl"))
  expect_true(any(!is.na(pmx_parameters(m)$estimate)))

  expect_identical(json_round_trip(m), m)
  expect_identical(
    json_round_trip(model_twocmt_oral(estimates = TRUE)),
    model_twocmt_oral(estimates = TRUE)
  )
})

test_that("statements, constants and covariates survive JSON", {
  skip_if_no_engine("zujson")
  m <- read_ctl(ctl_onecmt(
    pk = c(
      "IF (WT .GT. 70) THEN",
      "  CLI = CL * 1.1",
      "ELSE",
      "  CLI = CL * 0.30000000000000004",
      "ENDIF",
      "ALAG1 = 0.1 + 0.2",
      "F1 = -1 + 2"
    ),
    error = c("IPRED = F", "Y = IPRED + EPS(1) * IPRED + EPS(2)"),
    extra = "$SIGMA 0.01"
  ))
  m <- model_with(
    m,
    dosing = list(depot = list(lag = 0.5, bioavailability = -0.25))
  )

  back <- json_round_trip(m)

  expect_identical(back, m)
  expect_identical(unclass(back)$provenance$covariates, "WT")
})

test_that("a model with no estimate, dosing or covariate keeps its empty parts", {
  skip_if_no_engine("zujson")
  m <- model_onecmt_mm()

  expect_identical(json_round_trip(m), m)
})

test_that("the JSON holds the representation as plain data", {
  skip_if_no_engine("zujson")
  m <- pmx_read_nonmem(corpus_file("onecmt-oral-lag", "model.ctl"))

  x <- zujson::json_parse(pmx_write_json(m), simplify = FALSE)

  expect_identical(x$zufapmx, "model")
  expect_identical(x$version, 1L)
  expect_identical(x$parameters[[1]]$upper, "Inf")
  expect_null(x$parameters[[1]]$estimate)
  expect_identical(x$individual[[2]], "CL <- THETA2 * exp(ETA1)")
  expect_identical(x$dosing$depot$lag, "THETA4")
  expect_identical(x$structure$parameters$cl, "CL")
})

test_that("JSON text is the same under any options", {
  skip_if_no_engine("zujson")
  m <- read_ctl(ctl_onecmt(pk = "ALAG1 = 0.123456789012345"))
  ref <- pmx_write_json(m)

  withr::local_options(OutDec = ",", scipen = -5, digits = 3)
  expect_identical(pmx_write_json(m), ref)
  expect_identical(pmx_write_json(m, pretty = FALSE), {
    withr::local_options(OutDec = ".", scipen = 0, digits = 7)
    pmx_write_json(m, pretty = FALSE)
  })
})

test_that("pmx_write_json() writes a file and returns the text", {
  skip_if_no_engine("zujson")
  m <- model_onecmt_mm()
  file <- withr::local_tempfile(fileext = ".json")

  expect_invisible(out <- pmx_write_json(m, file))
  expect_identical(paste(readLines(file), collapse = "\n"), out)
  expect_identical(pmx_write_json(m), out)
  expect_false(grepl("\n", pmx_write_json(m, pretty = FALSE)))
})

test_that("JSON needs zujson", {
  testthat::local_mocked_bindings(engine_available = function(engine) FALSE)
  m <- model_onecmt_mm()
  file <- withr::local_tempfile(fileext = ".json")
  writeLines("{}", file)

  expect_error(pmx_write_json(m), class = "zufapmx_engine_error")
  expect_error(pmx_read_json(file), class = "zufapmx_engine_error")
})

test_that("a file that is not a zufapmx JSON model is a parse error", {
  skip_if_no_engine("zujson")
  good <- pmx_write_json(model_onecmt_mm(), pretty = FALSE)
  edit <- function(pattern, replacement) {
    sub(pattern, replacement, good, fixed = TRUE)
  }
  bad <- c(
    not_json = "{ not json",
    not_object = "[1, 2]",
    not_model = '{"zufapmx": "design"}',
    version = edit('"version":1', '"version":2'),
    role = edit('"role":"other"', '"role":"rate"'),
    number = edit('"init":1', '"init":"one"'),
    infinity = edit('"init":1', '"init":"Infinity"'),
    boolean = edit('"fixed":false', '"fixed":"no"'),
    expression = edit('"ka <- tka"', '"ka <- "'),
    matrix = edit('"matrix":[[0.04]]', '"matrix":[[0.04, 0]]'),
    type = edit('"type":"ode"', '"type":"pde"')
  )
  for (case in names(bad)) {
    expect_false(identical(bad[[case]], good), label = case)
    file <- withr::local_tempfile(fileext = ".json")
    writeLines(bad[[case]], file)
    expect_error(
      pmx_read_json(file),
      class = "zufapmx_parse_error",
      label = case
    )
  }
})

test_that("the JSON functions refuse malformed arguments", {
  m <- model_onecmt_mm()

  expect_error(pmx_write_json(list()), class = "zufapmx_model_error")
  expect_error(pmx_write_json(m, file = 1), class = "zufapmx_model_error")
  expect_error(
    pmx_write_json(m, file = NA_character_),
    class = "zufapmx_model_error"
  )
  expect_error(pmx_write_json(m, file = ""), class = "zufapmx_model_error")
  expect_error(pmx_write_json(m, pretty = NA), class = "zufapmx_model_error")
  expect_error(pmx_write_json(m, pretty = "yes"), class = "zufapmx_model_error")
  expect_error(
    pmx_read_json("no-such-file.json"),
    class = "zufapmx_model_error"
  )
  expect_error(pmx_read_json(c("a", "b")), class = "zufapmx_model_error")
})

test_that("a model without an endpoint round-trips through JSON", {
  skip_if_no_engine("zujson")
  m <- pmx_read_rxode2(function() {
    ini({
      tcl <- c(0, 4)
      tv <- c(0, 10)
    })
    model({
      cl <- tcl
      v <- tv
      cp <- linCmt()
    })
  })
  expect_length(unclass(m)$observations, 0)

  expect_identical(json_round_trip(m), m)
})
