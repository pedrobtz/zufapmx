#' Read an rxode2 or nlmixr2 model function
#'
#' Reads the `ini({})` and `model({})` blocks of a model function into the
#' package's representation. The function is read as R syntax, so neither
#' rxode2 nor nlmixr2 needs to be installed.
#'
#' Supported: fixed effects with bounds and `fix()`; random effects,
#' alone or in blocks; statements built from the function table;
#' `linCmt()`; `d/dt()`; `alag()`, `lag()`, `f()`, `rate()`, `dur()` and
#' initial amounts (`depot(0) <-`); and residual models `add()`, `prop()`,
#' `add() + prop()`, `lnorm()` and `pow()`. Anything else is refused with
#' an error of class `zupkpmx_parse_error`. Labels, `description` and
#' comments are dropped and recorded as losses.
#'
#' @param model A model function, as written for rxode2 or nlmixr2.
#' @return A `zupkpmx_model`.
#' @export
#' @examples
#' one_cmt <- function() {
#'   ini({
#'     tka <- 0.45
#'     tcl <- 1
#'     tv <- 3.45
#'     eta.cl ~ 0.3
#'     prop.sd <- 0.1
#'   })
#'   model({
#'     ka <- exp(tka)
#'     cl <- exp(tcl + eta.cl)
#'     v <- exp(tv)
#'     cp <- linCmt()
#'     cp ~ prop(prop.sd)
#'   })
#' }
#' pmx_read_rxode2(one_cmt)
pmx_read_rxode2 <- function(model) {
  source <- paste(deparse(substitute(model)), collapse = " ")
  if (!is.function(model)) {
    zupkpmx_abort("`model` must be a model function.", "model")
  }
  rxode2_read_body(body(model), source)
}

rxode2_read_body <- function(body, source) {
  body <- strip_srcref(body)
  reader <- rxode2_read_context(source)
  blocks <- if (is_call(body, "{")) as.list(body)[-1] else list(body)

  ini <- NULL
  model <- NULL
  for (x in blocks) {
    if (is_call(x, "ini")) {
      ini <- x[[2]]
    } else if (is_call(x, "model")) {
      model <- x[[2]]
    } else if (is_call(x, "<-") && identical(x[[2]], quote(description))) {
      reader$lose("description")
    } else if (
      (is_call(x, "<-") || is_call(x, "=")) &&
        is.symbol(x[[2]]) &&
        rxode2_read_is_metadata(x[[3]])
    ) {
      reader$lose(sprintf("metadata `%s`", as.character(x[[2]])))
    } else {
      reader$refuse(x, "`ini({})`, `model({})` or `description`")
    }
  }
  if (is.null(ini) || is.null(model)) {
    reader$refuse(body, "both `ini({})` and `model({})`")
  }

  ini <- rxode2_read_ini(stmt_body(ini), reader)
  parts <- rxode2_read_model(stmt_body(model), reader)
  for (obs in parts$observations) {
    unknown <- setdiff(obs$residual$parameters, ini$parameters$name)
    if (length(unknown) > 0) {
      reader$refuse(
        as.symbol(unknown[[1]]),
        "residual error parameters defined in ini()"
      )
    }
  }
  attach <- attach_etas(ini$etas, parts$individual)
  structure <- parts$structure
  parameters <- ini$parameters
  parameters$role <- infer_roles(parameters$name, parts$individual, structure)

  read_model(
    source,
    parameters = parameters,
    random_effects = list(
      omega = ini$omega,
      sigma = list(),
      attach = attach,
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = parts$individual,
    structure = structure,
    dosing = parts$dosing,
    observations = parts$observations,
    provenance = list(
      format = "rxode2",
      file = source,
      estimates = NA_character_,
      hash = NA_character_,
      fidelity = if (length(reader$losses()) == 0) "exact" else "lossy",
      losses = reader$losses()
    )
  )
}

# Collects losses and raises parse errors for one read.
rxode2_read_context <- function(source) {
  losses <- character()
  list(
    lose = function(what) losses <<- union(losses, what),
    losses = function() losses,
    # Check an expression or statement; a construct outside the table is a
    # parse error of this read, not an invalid model.
    check = function(x, stmt = x, fn = expr_check) {
      tryCatch(
        fn(x),
        zupkpmx_model_error = function(e) {
          parse_error(
            source,
            NA_integer_,
            NA_integer_,
            seen = expr_text(stmt),
            message = sprintf(
              "in `%s`: %s",
              expr_text(stmt),
              conditionMessage(e)
            ),
            call = NULL
          )
        }
      )
      x
    },
    refuse = function(x, expected) {
      parse_error(
        source,
        NA_integer_,
        NA_integer_,
        seen = expr_text(x),
        message = sprintf("found `%s`, expected %s", expr_text(x), expected),
        call = NULL
      )
    }
  )
}

rxode2_read_ini <- function(stmts, reader) {
  params <- list()
  omega <- list()
  for (x in stmts) {
    if (is_call(x, "label")) {
      reader$lose("labels")
    } else if (is_call(x, "<-") && is.symbol(x[[2]])) {
      value <- rxode2_read_value(x[[3]], reader)
      bounds <- read_bounds(value$values)
      if (is.null(bounds)) {
        reader$refuse(x, "one to three values, lower <= initial <= upper")
      }
      params[[length(params) + 1]] <- data.frame(
        name = as.character(x[[2]]),
        role = "other",
        init = bounds$init,
        lower = bounds$lower,
        upper = bounds$upper,
        fixed = value$fixed,
        estimate = NA_real_
      )
    } else if (is_call(x, "~")) {
      names <- all.vars(x[[2]])
      value <- rxode2_read_value(x[[3]], reader)
      if (!is.symbol(x[[2]]) && !is_call(x[[2]], "+")) {
        reader$refuse(x, "a random effect name or a sum of names")
      }
      omega[[length(omega) + 1]] <- list(
        names = names,
        matrix = rxode2_read_block(names, value$values, x, reader),
        fixed = value$fixed
      )
    } else {
      reader$refuse(x, "a parameter, a random effect or `label()`")
    }
  }
  parameters <- if (length(params) == 0) {
    model_parameters_empty()
  } else {
    do.call(rbind, params)
  }
  list(
    parameters = parameters,
    omega = omega,
    etas = unlist(lapply(omega, `[[`, "names"))
  )
}

# A numeric ini value: a number, `c(...)` of numbers, or either inside
# `fix()`/`fixed()`.
rxode2_read_value <- function(x, reader) {
  fixed <- FALSE
  if (is_call(x, "fix") || is_call(x, "fixed")) {
    fixed <- TRUE
    args <- as.list(x)[-1]
    x <- if (length(args) == 1) args[[1]] else as.call(c(quote(c), args))
  }
  values <- if (is_call(x, "c")) as.list(x)[-1] else list(x)
  values <- vapply(values, rxode2_read_number, numeric(1), reader = reader)
  list(values = unname(values), fixed = fixed)
}

rxode2_read_number <- function(x, reader) {
  if (is.numeric(x) && length(x) == 1) {
    return(as.numeric(x))
  }
  if (identical(x, quote(Inf))) {
    return(Inf)
  }
  if (is_call(x, "-") && length(x) == 2) {
    return(-rxode2_read_number(x[[2]], reader))
  }
  # A constant expression such as `log(90)` is evaluated; its text is lost.
  if (is.call(x) && length(all.vars(x)) == 0) {
    ok <- tryCatch(
      {
        expr_check(x)
        TRUE
      },
      zupkpmx_model_error = function(e) FALSE
    )
    if (ok) {
      reader$lose("initial values written as expressions, evaluated")
      return(eval(x, baseenv()))
    }
  }
  reader$refuse(x, "a number or a constant expression of the function table")
}

# Top-level assignments of text, such as `reference <- "..."` or
# `dosing <- c("central", "depot")`, describe a model without being part of
# it.
rxode2_read_is_metadata <- function(x) {
  if (is.character(x)) {
    return(TRUE)
  }
  (is_call(x, "c") || is_call(x, "list")) &&
    all(vapply(as.list(x)[-1], rxode2_read_is_metadata, logical(1)))
}

# A block's values are its lower triangle, row by row.
rxode2_read_block <- function(names, values, x, reader) {
  n <- length(names)
  if (length(values) != n * (n + 1) / 2) {
    reader$refuse(x, sprintf("%d values for a block of %d", n * (n + 1) / 2, n))
  }
  m <- lower_to_full(values)
  dimnames(m) <- list(names, names)
  m
}

# linCmt() names its parameters in one of two styles: rxode2's (cl, v or
# vc, q, vp, q2, vp2) or NONMEM's numbered TRANS4 names, where the central
# volume is V1 without a depot or V2 with one, and the volumes and
# clearances after it are numbered on (V2/V3/V4, Q2/Q3/Q4).
lincmt_aliases <- function(lower) {
  if ("v1" %in% lower) {
    return(list(
      cl = "cl",
      v = "v1",
      q = c("q", "q2"),
      vp = "v2",
      q2 = "q3",
      vp2 = "v3",
      ka = "ka"
    ))
  }
  if ("v2" %in% lower && !any(c("v", "vc") %in% lower)) {
    return(list(
      cl = "cl",
      v = "v2",
      q = c("q", "q3"),
      vp = "v3",
      q2 = "q4",
      vp2 = "v4",
      ka = "ka"
    ))
  }
  list(
    cl = "cl",
    v = c("v", "vc"),
    q = "q",
    vp = "vp",
    q2 = "q2",
    vp2 = "vp2",
    ka = "ka"
  )
}

rxode2_dosing_attributes <- c(
  alag = "lag",
  lag = "lag",
  f = "bioavailability",
  rate = "rate",
  dur = "duration"
)

rxode2_read_model <- function(stmts, reader) {
  derivatives <- list()
  dosing <- list()
  residuals <- list()
  assigns <- list()
  lincmt <- NULL

  for (x in stmts) {
    if (is_call(x, "~")) {
      if (!is.symbol(x[[2]])) {
        reader$refuse(x, "an endpoint name on the left of `~`")
      }
      residuals[[as.character(x[[2]])]] <- rxode2_read_residual(
        x[[3]],
        x,
        reader
      )
    } else if (is_call(x, "<-") && rxode2_read_is_ddt(x[[2]])) {
      derivatives[[as.character(x[[2]][[3]][[2]])]] <- reader$check(x[[3]], x)
    } else if (is_call(x, "<-") && rxode2_read_is_dosing(x[[2]])) {
      # `alag(depot) <- e` names the compartment inside the call;
      # `depot(0) <- e` names it as the call.
      initial <- is.numeric(x[[2]][[2]])
      cmt <- as.character(x[[2]][[if (initial) 1 else 2]])
      attr <- if (initial) {
        "initial"
      } else {
        rxode2_dosing_attributes[[as.character(x[[2]][[1]])]]
      }
      if (is.null(dosing[[cmt]])) {
        dosing[[cmt]] <- list()
      }
      dosing[[cmt]][[attr]] <- reader$check(x[[3]], x)
    } else if (is_call(x, "<-") && is_call(x[[3]], "linCmt")) {
      lincmt <- as.character(x[[2]])
    } else if (is_call(x, "<-") || is_call(x, "if")) {
      reader$check(x, fn = stmt_check)
      assigns[[length(assigns) + 1]] <- x
    } else if (is_call(x, "label")) {
      reader$lose("labels")
    } else {
      reader$refuse(x, "a statement, `d/dt()`, a dosing attribute or `~`")
    }
  }

  if (!is.null(lincmt) && length(derivatives) > 0) {
    reader$refuse(quote(linCmt()), "`linCmt()` or `d/dt()`, not both")
  }
  if (is.null(lincmt) && length(derivatives) == 0) {
    reader$refuse(quote(model()), "`linCmt()` or `d/dt()`")
  }

  # An endpoint's last assignment is its prediction.
  endpoints <- names(residuals)
  targets <- vapply(
    assigns,
    function(x) if (is_call(x, "<-")) as.character(x[[2]]) else "",
    character(1)
  )
  is_prediction <- targets %in%
    endpoints &
    !duplicated(targets, fromLast = TRUE)
  predictions <- lapply(assigns[is_prediction], `[[`, 3)
  names(predictions) <- targets[is_prediction]
  rest <- assigns[!is_prediction]

  if (!is.null(lincmt)) {
    individual <- rest
    structure <- rxode2_read_lincmt(stmt_targets(individual), reader)
    compartments <- closed_compartments(structure)
    predictions[[lincmt]] <- call(
      "/",
      quote(central),
      as.symbol(structure$parameters[["v"]])
    )
  } else {
    # A statement belongs to the structure when it reads a compartment
    # amount, or a name another structure statement assigns.
    compartments <- names(derivatives)
    state <- compartments
    in_structure <- logical(length(rest))
    for (i in seq_along(rest)) {
      reads <- if (is_call(rest[[i]], "<-")) {
        all.vars(rest[[i]][[3]])
      } else {
        all.vars(rest[[i]])
      }
      if (any(reads %in% state)) {
        in_structure[[i]] <- TRUE
        state <- c(state, stmt_targets(rest[i]))
      }
    }
    individual <- rest[!in_structure]
    structure <- list(
      type = "ode",
      compartments = compartments,
      statements = rest[in_structure],
      derivatives = derivatives
    )
  }

  # An endpoint that is a compartment observes its amount directly.
  for (e in intersect(setdiff(endpoints, names(predictions)), compartments)) {
    predictions[[e]] <- as.symbol(e)
  }
  missing <- setdiff(endpoints, names(predictions))
  if (length(missing) > 0) {
    reader$refuse(
      as.symbol(missing[[1]]),
      "a prediction assigned to every endpoint"
    )
  }
  unknown <- setdiff(names(dosing), compartments)
  if (length(unknown) > 0) {
    reader$refuse(
      as.symbol(unknown[[1]]),
      "dosing on a compartment of the model"
    )
  }

  observations <- lapply(endpoints, function(e) {
    list(
      prediction = predictions[[e]],
      residual = residuals[[e]],
      transform = "none"
    )
  })
  names(observations) <- endpoints

  list(
    individual = individual,
    structure = structure,
    dosing = dosing[intersect(compartments, names(dosing))],
    observations = observations
  )
}

rxode2_read_is_ddt <- function(x) {
  is_call(x, "/") &&
    identical(x[[2]], quote(d)) &&
    is_call(x[[3]], "dt") &&
    is.symbol(x[[3]][[2]])
}

rxode2_read_is_dosing <- function(x) {
  is.call(x) &&
    length(x) == 2 &&
    is.symbol(x[[1]]) &&
    ((as.character(x[[1]]) %in%
      names(rxode2_dosing_attributes) &&
      is.symbol(x[[2]])) ||
      identical(x[[2]], 0) ||
      identical(x[[2]], 0L))
}

rxode2_read_residual <- function(x, stmt, reader) {
  one <- function(call, fn) {
    if (
      is_call(call, fn) && all(vapply(as.list(call)[-1], is.symbol, logical(1)))
    ) {
      vapply(as.list(call)[-1], as.character, character(1))
    }
  }
  if (!is.null(p <- one(x, "prop"))) {
    return(list(type = "proportional", parameters = p))
  }
  if (!is.null(p <- one(x, "add"))) {
    return(list(type = "additive", parameters = p))
  }
  if (!is.null(p <- one(x, "lnorm"))) {
    return(list(type = "exponential", parameters = p))
  }
  if (!is.null(p <- one(x, "pow"))) {
    return(list(type = "power", parameters = p))
  }
  if (is_call(x, "+") && length(x) == 3) {
    a <- one(x[[2]], "add") %||% one(x[[3]], "add")
    p <- one(x[[3]], "prop") %||% one(x[[2]], "prop")
    if (!is.null(a) && !is.null(p)) {
      return(list(type = "combined", parameters = c(a, p)))
    }
  }
  reader$refuse(
    stmt,
    "`add()`, `prop()`, `add() + prop()`, `lnorm()` or `pow()`"
  )
}

rxode2_read_lincmt <- function(targets, reader) {
  lower <- tolower(targets)
  found <- lapply(lincmt_aliases(lower), function(alias) {
    hit <- targets[lower %in% alias]
    if (length(hit) > 0) hit[[1]]
  })
  found <- unlist(found)
  if (!all(c("cl", "v") %in% names(found))) {
    reader$refuse(
      quote(linCmt()),
      "`linCmt()` with a clearance and a central volume"
    )
  }
  ncmt <- 1L +
    all(c("q", "vp") %in% names(found)) +
    all(c("q2", "vp2") %in% names(found))
  keep <- c("cl", "v", "q", "vp", "q2", "vp2")[seq_len(2 * ncmt)]
  depot <- "ka" %in% names(found)
  list(
    type = "closed",
    ncmt = ncmt,
    depot = depot,
    parameters = found[c(keep, if (depot) "ka")],
    advan = closed_advan(ncmt, depot),
    trans = if (ncmt == 1) 2L else 4L
  )
}
