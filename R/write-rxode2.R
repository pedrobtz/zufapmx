#' Write a model as an rxode2 model function
#'
#' Prints a model as the text of an rxode2/nlmixr2 model function, with its
#' fidelity (see [pmx_fidelity()]). Evaluate the text to get the function.
#'
#' rxode2's `ini()` holds one value per parameter: a model with estimates
#' is written with its estimates, and its initial values are recorded as
#' lost. Constructs rxode2 cannot hold, such as residual error written as
#' an expression over epsilons, stop with an error of class
#' `zupkpmx_write_error`.
#'
#' @param model A model read by one of the `pmx_read_*()` functions.
#' @return A written model: the text as lines, its fidelity and its losses.
#' @export
#' @examples
#' corpus <- zupkpmx_corpus()
#' fun <- eval(parse(file.path(corpus$path[[2]], "model-rxode2.R")))
#' pmx_write_rxode2(pmx_read_rxode2(fun))
pmx_write_rxode2 <- function(model) {
  check_model(model)
  m <- unclass(model)
  losses <- character()

  if (length(m$random_effects$sigma) > 0) {
    write_error(
      "residual error as sigma blocks over epsilons",
      "random_effects$sigma",
      formats = c("nonmem", "mrgsolve"),
      call = NULL
    )
  }
  if (isTRUE(m$random_effects$iov$present)) {
    write_error(
      "inter-occasion variability",
      "random_effects$iov",
      formats = character(),
      call = NULL
    )
  }
  if (any(!is.na(m$parameters$estimate))) {
    losses <- c(
      losses,
      "initial values: rxode2's ini() holds one value per parameter, so estimates were written"
    )
  }

  text <- c(
    "function() {",
    "  ini({",
    indent(rxode2_ini_lines(m), 4),
    "  })",
    "  model({",
    indent(rxode2_model_lines(m), 4),
    "  })",
    "}"
  )

  back <- read_rxode2_body(body(eval(parse(text = text))), "written")
  new_written(
    text = text,
    format = "rxode2",
    fidelity = written_fidelity(losses, model, back),
    losses = losses
  )
}

rxode2_ini_lines <- function(m) {
  p <- m$parameters
  params <- vapply(
    seq_len(nrow(p)),
    function(i) {
      value <- if (is.na(p$estimate[[i]])) p$init[[i]] else p$estimate[[i]]
      values <- if (is.infinite(p$upper[[i]]) && p$upper[[i]] > 0) {
        if (is.infinite(p$lower[[i]]) && p$lower[[i]] < 0) {
          value
        } else {
          c(p$lower[[i]], value)
        }
      } else {
        c(p$lower[[i]], value, p$upper[[i]])
      }
      paste(p$name[[i]], "<-", rxode2_values(values, p$fixed[[i]]))
    },
    character(1)
  )

  etas <- vapply(
    m$random_effects$omega,
    function(block) {
      values <- block$matrix[upper.tri(block$matrix, diag = TRUE)]
      paste(
        paste(block$names, collapse = " + "),
        "~",
        rxode2_values(values, block$fixed)
      )
    },
    character(1)
  )
  c(params, etas)
}

rxode2_values <- function(values, fixed) {
  text <- vapply(values, num_text, character(1))
  if (fixed) {
    return(sprintf("fix(%s)", paste(text, collapse = ", ")))
  }
  if (length(text) == 1) {
    text
  } else {
    sprintf("c(%s)", paste(text, collapse = ", "))
  }
}

rxode2_model_lines <- function(m) {
  s <- m$structure
  lines <- stmt_lines(m$individual)

  if (identical(s$type, "ode")) {
    lines <- c(
      lines,
      stmt_lines(s$statements),
      vapply(
        s$compartments,
        function(cmt) {
          sprintf("d/dt(%s) <- %s", cmt, expr_line(s$derivatives[[cmt]]))
        },
        character(1),
        USE.NAMES = FALSE
      )
    )
  }

  for (cmt in names(m$dosing)) {
    for (attr in names(m$dosing[[cmt]])) {
      value <- expr_line(m$dosing[[cmt]][[attr]])
      lines <- c(
        lines,
        if (attr == "initial") {
          sprintf("%s(0) <- %s", cmt, value)
        } else {
          fn <- c(
            lag = "alag",
            bioavailability = "f",
            rate = "rate",
            duration = "dur"
          )[[attr]]
          sprintf("%s(%s) <- %s", fn, cmt, value)
        }
      )
    }
  }

  for (endpoint in names(m$observations)) {
    obs <- m$observations[[endpoint]]
    if (obs$transform != "none") {
      write_error(
        sprintf("a %s-transformed endpoint", obs$transform),
        sprintf("observations$%s", endpoint),
        formats = character(),
        call = NULL
      )
    }
    lines <- c(lines, rxode2_prediction(s, endpoint, obs$prediction))
  }
  for (endpoint in names(m$observations)) {
    lines <- c(
      lines,
      sprintf(
        "%s ~ %s",
        endpoint,
        rxode2_residual_text(m$observations[[endpoint]]$residual, endpoint)
      )
    )
  }
  lines
}

rxode2_prediction <- function(s, endpoint, prediction) {
  if (identical(prediction, as.symbol(endpoint))) {
    return(character())
  }
  if (identical(s$type, "closed")) {
    if (
      identical(
        prediction,
        call("/", quote(central), as.symbol(s$parameters[["v"]]))
      )
    ) {
      return(sprintf("%s <- linCmt()", endpoint))
    }
    if (any(all.vars(prediction) %in% closed_compartments(s))) {
      write_error(
        "a closed-form prediction other than the central concentration",
        sprintf("observations$%s", endpoint),
        formats = character(),
        call = NULL
      )
    }
  }
  sprintf("%s <- %s", endpoint, expr_line(prediction))
}

rxode2_residual_text <- function(residual, endpoint) {
  p <- residual$parameters
  switch(
    residual$type,
    additive = sprintf("add(%s)", p[[1]]),
    proportional = sprintf("prop(%s)", p[[1]]),
    combined = sprintf("add(%s) + prop(%s)", p[[1]], p[[2]]),
    exponential = sprintf("lnorm(%s)", p[[1]]),
    power = sprintf("pow(%s)", paste(p, collapse = ", ")),
    write_error(
      sprintf("a residual model of type %s", residual$type),
      sprintf("observations$%s$residual", endpoint),
      formats = "nonmem",
      call = NULL
    )
  )
}

stmt_lines <- function(stmts) {
  unlist(lapply(stmts, function(x) {
    if (is_call(x, "if")) expr_deparse(x) else expr_line(x)
  }))
}

expr_line <- function(x) {
  paste(expr_deparse(x), collapse = " ")
}

# Deparse with 15 significant digits unless a constant needs 17 to read
# back as the same number.
expr_deparse <- function(x) {
  numbers <- expr_numbers(x)
  exact <- all(vapply(
    numbers,
    function(n) identical(as.numeric(as.character(n)), n),
    logical(1)
  ))
  opts <- if (exact) "keepNA" else c("keepNA", "digits17")
  deparse(x, width.cutoff = 500L, control = opts)
}

expr_numbers <- function(x) {
  if (is.numeric(x)) {
    return(as.list(x))
  }
  if (is.call(x)) {
    return(unlist(lapply(as.list(x)[-1], expr_numbers), recursive = FALSE))
  }
  list()
}

num_text <- function(x) {
  text <- as.character(x)
  if (identical(as.numeric(text), x)) text else sprintf("%.17g", x)
}

indent <- function(lines, n) {
  paste0(strrep(" ", n), lines)
}
