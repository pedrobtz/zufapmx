#' Write a model as an rxode2 model function
#'
#' Prints a model as the text of an rxode2/nlmixr2 model function, with its
#' fidelity (see [pmx_fidelity()]). Evaluate the text to get the function.
#'
#' rxode2's `ini()` holds one value per parameter: a model with estimates
#' is written with its estimates, and its initial values are recorded as
#' lost. Residual error held as sigma variances, as NONMEM holds it, is
#' written as standard-deviation parameters named after the epsilons.
#' Variables `linCmt()` would read as parameters are renamed with an `x_`
#' prefix. `linCmt()` reads its parameters from `model()` only, so a
#' closed-form parameter held directly as a fixed effect (mrgsolve's
#' `$PKMODEL` over `$PARAM`) is renamed with a `t` prefix (`tCL`) and
#' assigned in `model()` (`CL <- tCL`). Constructs rxode2 cannot hold,
#' such as residual error written as an expression over epsilons, or an
#' individual parameter that reads the record's time, stop with an error of
#' class `zupmxio_write_error`.
#'
#' @inheritParams pmx_inspect
#' @return A `zupmxio_written` object: the text as lines, with its fidelity
#'   and losses (see [pmx_fidelity()]). `format()` and `as.character()`
#'   give the text.
#' @family writers
#' @export
#' @examples
#' path <- system.file(
#'   "corpus", "onecmt-oral-mm", "model-rxode2.R",
#'   package = "zupmxio"
#' )
#' fun <- eval(parse(path))
#' pmx_write_rxode2(pmx_read_rxode2(fun))
pmx_write_rxode2 <- function(model) {
  check_model(model)
  m <- unclass(model)
  losses <- character()

  m <- rxode2_write_sd_from_sigma(m)
  m <- model_map(m, rxode2_write_powers)
  m <- rxode2_write_lincmt_parameters(m)
  m <- rxode2_write_lincmt_numbering(m)
  m <- rxode2_write_free_lincmt(m)
  m <- rxode2_write_free_special(m)
  refuse_unwritable(m, one_endpoint = FALSE)
  # The record's time in individual statements (D15) has no rxode2
  # spelling: rxode2 evaluates every statement in continuous time.
  for (x in m$individual) {
    if ("t" %in% all.vars(x)) {
      write_error(
        "an individual parameter that reads the record's time",
        "individual",
        formats = c("nonmem", "mrgsolve"),
        call = NULL
      )
    }
  }
  if (any(!is.na(m$parameters$estimate))) {
    losses <- c(
      losses,
      paste(
        "initial values: rxode2's ini() holds one value per parameter,",
        "so estimates were written"
      )
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

  back <- read_back(
    rxode2_read_body(body(eval(rxode2_write_parse_text(text))), "written"),
    "rxode2"
  )
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
        formats = "nonmem",
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

# linCmt() finds its parameters among model() variables, never ini()
# parameters. A closed form whose CL is a parameter (mrgsolve's $PKMODEL
# over $PARAM) gets the parameter renamed tCL and `CL <- tCL` in model().
rxode2_write_lincmt_parameters <- function(m) {
  if (!identical(m$structure$type, "closed")) {
    return(m)
  }
  direct <- intersect(m$structure$parameters, m$parameters$name)
  if (length(direct) == 0) {
    return(m)
  }
  taken <- unique(c(
    m$parameters$name,
    stmt_targets(m$individual),
    unlist(lapply(model_expressions(m), all.vars))
  ))
  renamed <- make.unique(c(taken, paste0("t", direct)))[
    length(taken) + seq_along(direct)
  ]
  map <- stats::setNames(renamed, direct)
  m$parameters$name <- map[m$parameters$name] %|NA|% m$parameters$name
  # The old names stay, assigned from the new ones, so the closed form and
  # the predictions keep reading them.
  m <- model_rename(m, map, observations = FALSE)
  m$individual <- c(
    lapply(seq_along(direct), function(i) {
      call("<-", as.symbol(direct[[i]]), as.symbol(renamed[[i]]))
    }),
    m$individual
  )
  m
}

# NONMEM's ADVAN12 numbering (V2 central, Q3 and Q4 to V3 and V4) is one
# linCmt() refuses: "can find peripheral volume but not distributional
# clearance". With V2 central it reads the clearances as Q and Q2, so they
# are renamed (#176).
rxode2_write_lincmt_numbering <- function(m) {
  s <- m$structure
  if (
    !identical(s$type, "closed") || !all(c("q", "q2") %in% names(s$parameters))
  ) {
    return(m)
  }
  q <- s$parameters[c("q", "q2")]
  if (!identical(unname(tolower(q)), c("q3", "q4"))) {
    return(m)
  }
  to <- ifelse(q == toupper(q), c("Q", "Q2"), c("q", "q2"))
  taken <- unique(c(
    m$parameters$name,
    stmt_targets(m$individual),
    unlist(lapply(model_expressions(m), all.vars))
  ))
  if (any(to %in% taken)) {
    return(m)
  }
  map <- stats::setNames(unname(to), unname(q))
  m$parameters$name <- map[m$parameters$name] %|NA|% m$parameters$name
  m$structure$parameters[c("q", "q2")] <- unname(to)
  model_rename(m, map)
}

# Names linCmt() reads as parameters. In a closed form, any other variable
# with one of them (V next to V1 and V2, say, after a NONMEM ADVAN alias)
# makes rxode2 refuse the model, so it is renamed with an `x_` prefix.
# rxode2 also reads some stems as prefixes: VPX or VP_ next to V1 "mixes
# volume styles", and KE or K10 is "ambiguous kel"; a suffix is not enough,
# while VMAX, CLR or KAPPA are left alone (probed with rxode2 5.1.8).
rxode2_lincmt_reserved <- c(
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
  "ke",
  "k10",
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
  lower <- tolower(vars)
  reads <- (lower %in% rxode2_lincmt_reserved) |
    grepl("^(vp|ke|k10)", lower)
  clash <- vars[reads & !vars %in% m$structure$parameters]
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
  model_rename(m, map)
}

# Names rxode2 gives a meaning of its own, matched without regard to case:
# event and data items it reads from the records, time since dose, and
# constants it parses. A variable or parameter with one is renamed with an
# `x_` prefix; a covariate cannot be, since it names a data column (#181).
rxode2_special_names <- c(
  "tlast",
  "tad",
  "tafd",
  "tfirst",
  "podo",
  "dose",
  "dosenum",
  "newind",
  "rate",
  "dur",
  "cmt",
  "ss",
  "addl",
  "ii",
  "amt",
  "evid",
  "id",
  "dv",
  "mdv",
  "time",
  "m_e",
  "m_pi",
  "na",
  "true",
  "false",
  "inf",
  "nan"
)

rxode2_write_free_special <- function(m) {
  covariates <- m$provenance$covariates
  special_cov <- covariates[tolower(covariates) %in% rxode2_special_names]
  if (length(special_cov) > 0) {
    write_error(
      sprintf(
        "the covariate `%s`, a name rxode2 reads as its own",
        special_cov[[1]]
      ),
      "provenance$covariates",
      formats = c("nonmem", "mrgsolve"),
      call = NULL
    )
  }
  names <- unique(c(
    m$parameters$name,
    stmt_targets(m$individual),
    stmt_targets(m$structure$statements)
  ))
  clash <- names[tolower(names) %in% rxode2_special_names]
  if (length(clash) == 0) {
    return(m)
  }
  taken <- unique(c(names, unlist(lapply(model_expressions(m), all.vars))))
  map <- stats::setNames(
    make.unique(c(taken, paste0("x_", clash)))[
      length(taken) + seq_along(clash)
    ],
    clash
  )
  m$parameters$name <- map[m$parameters$name] %|NA|% m$parameters$name
  if (identical(m$structure$type, "closed")) {
    m$structure$parameters[] <- map[m$structure$parameters] %|NA|%
      m$structure$parameters
  }
  m$observations <- lapply(m$observations, function(o) {
    o$residual$parameters <- unname(
      map[o$residual$parameters] %|NA|% o$residual$parameters
    )
    o
  })
  model_rename(m, map)
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
      # Estimates where the model has them, as for thetas (#182).
      m <- block$estimate %||% block$matrix
      values <- m[upper.tri(m, diag = TRUE)]
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

# Exact digits: a standard deviation written for a sigma is squared when
# read back, which doubles its rounding error (#182).
rxode2_write_values <- function(values, fixed) {
  text <- vapply(
    values,
    function(v) {
      t <- as.character(v)
      if (identical(as.numeric(t), v)) t else sprintf("%.17g", v)
    },
    character(1)
  )
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
  lines <- rxode2_write_stmt_lines(m$individual)

  if (identical(s$type, "ode")) {
    lines <- c(
      lines,
      rxode2_write_stmt_lines(s$statements),
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
    for (field in names(m$dosing[[cmt]])) {
      value <- expr_line(m$dosing[[cmt]][[field]])
      lines <- c(
        lines,
        if (field == "initial") {
          sprintf("%s(0) <- %s", cmt, value)
        } else {
          # The first spelling of each attribute is the one written.
          fn <- invert_names(rxode2_dosing_attributes)[[field]]
          sprintf("%s(%s) <- %s", fn, cmt, value)
        }
      )
    }
  }

  for (endpoint in names(m$observations)) {
    obs <- m$observations[[endpoint]]
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
      closed_prediction_error(endpoint)
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
      sprintf("the %s residual model", residual$type),
      sprintf("observations$%s$residual", endpoint),
      formats = residual_formats(residual$type),
      call = NULL
    )
  )
}

# rxode2's grammar refuses a^b^c, which R deparses for a^(b^c) since `^`
# is right-associative; an explicit `(` keeps the parentheses (#187).
rxode2_write_powers <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  x[-1] <- lapply(as.list(x)[-1], rxode2_write_powers)
  if (is_call(x, "^") && is_call(x[[3]], "^")) {
    x[[3]] <- call("(", x[[3]])
  }
  x
}

rxode2_write_stmt_lines <- function(stmts) {
  unlist(lapply(stmts, function(x) {
    if (is_call(x, "if")) expr_deparse(x) else expr_line(x)
  }))
}

# Written text as R code; a syntax error is a refusal of the text.
rxode2_write_parse_text <- function(text) {
  tryCatch(
    parse(text = text, keep.source = FALSE)[[1]],
    error = function(e) {
      parse_error(
        "written",
        NA_integer_,
        NA_integer_,
        seen = "text",
        message = conditionMessage(e),
        call = NULL
      )
    }
  )
}
