#' Write a model as an rxode2 model function
#'
#' Prints a model as the text of an rxode2/nlmixr2 model function, with its
#' fidelity (see [pmx_fidelity()]). Evaluate the text to get the function.
#'
#' rxode2's `ini()` holds one value per parameter: a model with estimates
#' is written with its estimates, and its initial values are recorded as
#' lost. Residual error held as sigma variances, as NONMEM holds it, is
#' written as standard-deviation parameters named after the epsilons. Constructs rxode2 cannot hold,
#' such as residual error written as an expression over epsilons, stop
#' with an error of class `zupkpmx_write_error`.
#'
#' @param model A model read by one of the `pmx_read_*()` functions.
#' @return A written model: the text as lines, its fidelity and its losses.
#' @export
#' @examples
#' path <- system.file("corpus", "onecmt-oral-mm", "model-rxode2.R", package = "zupkpmx")
#' fun <- eval(parse(path))
#' pmx_write_rxode2(pmx_read_rxode2(fun))
pmx_write_rxode2 <- function(model) {
  check_model(model)
  m <- unclass(model)
  losses <- character()

  m <- rxode2_write_sd_from_sigma(m)
  m <- rxode2_write_free_lincmt(m)
  check_writable(m, one_endpoint = FALSE)
  if (any(!is.na(m$parameters$estimate))) {
    losses <- c(
      losses,
      "initial values: rxode2's ini() holds one value per parameter, so estimates were written"
    )
  }

  text <- c(
    "function() {",
    "  ini({",
    indent(rxode2_write_ini(m), 4),
    "  })",
    "  model({",
    indent(rxode2_write_model(m), 4),
    "  })",
    "}"
  )

  back <- rxode2_read_body(body(eval(parse(text = text))), "written")
  new_written(
    text = text,
    format = "rxode2",
    fidelity = written_fidelity(losses, model, back),
    losses = losses
  )
}

# Residual error held as sigma variances (as NONMEM holds it) becomes
# standard-deviation parameters, named after their epsilons, whose values
# are the square roots of the variances.
rxode2_write_sd_from_sigma <- function(m) {
  blocks <- m$random_effects$sigma
  if (length(blocks) == 0) {
    return(m)
  }
  if (any(vapply(blocks, function(b) length(b$names) > 1, logical(1)))) {
    write_error(
      "correlated residual errors",
      "random_effects$sigma",
      formats = c("nonmem", "mrgsolve"),
      call = NULL
    )
  }
  for (e in names(m$observations)) {
    if (m$observations[[e]]$residual$type == "expression") {
      write_error(
        "a residual model written as an expression over epsilons",
        sprintf("observations$%s$residual", e),
        formats = c("nonmem", "mrgsolve"),
        call = NULL
      )
    }
  }
  sd <- lapply(blocks, function(b) {
    data.frame(
      name = b$names,
      role = "other",
      init = sqrt(b$matrix[[1]]),
      lower = 0,
      upper = Inf,
      fixed = b$fixed,
      estimate = if (is.null(b$estimate)) NA_real_ else sqrt(b$estimate[[1]])
    )
  })
  m$parameters <- do.call(rbind, c(list(m$parameters), sd))
  rownames(m$parameters) <- NULL
  m$random_effects$sigma <- list()
  m
}

# Names linCmt() reads as parameters. In a closed form, any other variable
# with one of them (V next to V1 and V2, say, after a NONMEM ADVAN alias)
# makes rxode2 refuse the model, so it is renamed with an `x_` prefix
# (rxode2 matches these names by prefix, so a suffix is not enough).
lincmt_reserved <- c(
  "cl",
  "v",
  "vc",
  "v1",
  "v2",
  "v3",
  "v4",
  "vp",
  "vp1",
  "vp2",
  "vp3",
  "vss",
  "q",
  "q1",
  "q2",
  "q3",
  "q4",
  "ka",
  "k",
  "kel",
  "k12",
  "k21",
  "k13",
  "k31",
  "alpha",
  "beta",
  "gamma"
)

rxode2_write_free_lincmt <- function(m) {
  if (!identical(m$structure$type, "closed")) {
    return(m)
  }
  vars <- stmt_targets(m$individual)
  clash <- vars[
    tolower(vars) %in% lincmt_reserved & !vars %in% m$structure$parameters
  ]
  if (length(clash) == 0) {
    return(m)
  }
  taken <- unique(c(
    vars,
    unlist(lapply(m$individual, all.vars)),
    m$parameters$name
  ))
  map <- stats::setNames(
    make.unique(c(taken, paste0("x_", clash)))[
      length(taken) + seq_along(clash)
    ],
    clash
  )
  rename <- function(x) rename_symbols(x, map)
  m$individual <- lapply(m$individual, rename)
  m$dosing <- lapply(m$dosing, function(d) lapply(d, rename))
  m$observations <- lapply(m$observations, function(o) {
    o$prediction <- rename(o$prediction)
    o
  })
  m
}

rxode2_write_ini <- function(m) {
  p <- m$parameters
  params <- vapply(
    seq_len(nrow(p)),
    function(i) {
      value <- if (is.na(p$estimate[[i]])) p$init[[i]] else p$estimate[[i]]
      values <- bounded_values(p$lower[[i]], value, p$upper[[i]])
      paste(p$name[[i]], "<-", rxode2_write_values(values, p$fixed[[i]]))
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
        rxode2_write_values(values, block$fixed)
      )
    },
    character(1)
  )
  c(params, etas)
}

rxode2_write_values <- function(values, fixed) {
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

rxode2_write_model <- function(m) {
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
    lines <- c(lines, rxode2_write_prediction(s, endpoint, obs$prediction))
  }
  for (endpoint in names(m$observations)) {
    lines <- c(
      lines,
      sprintf(
        "%s ~ %s",
        endpoint,
        rxode2_write_residual(m$observations[[endpoint]]$residual, endpoint)
      )
    )
  }
  lines
}

rxode2_write_prediction <- function(s, endpoint, prediction) {
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

rxode2_write_residual <- function(residual, endpoint) {
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
