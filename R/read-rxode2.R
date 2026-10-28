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
  read_rxode2_body(body(model), source)
}

read_rxode2_body <- function(body, source) {
  body <- strip_srcref(body)
  reader <- rxode2_reader(source)
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
    } else {
      reader$refuse(x, "`ini({})`, `model({})` or `description`")
    }
  }
  if (is.null(ini) || is.null(model)) {
    reader$refuse(body, "both `ini({})` and `model({})`")
  }

  ini <- read_rxode2_ini(stmt_body(ini), reader)
  parts <- read_rxode2_model(stmt_body(model), reader)
  attach <- rxode2_attach(ini$etas, parts$individual)
  structure <- parts$structure
  parameters <- ini$parameters
  parameters$role <- rxode2_roles(parameters$name, parts$individual, structure)

  new_model(
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
rxode2_reader <- function(source) {
  losses <- character()
  list(
    lose = function(what) losses <<- union(losses, what),
    losses = function() losses,
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

read_rxode2_ini <- function(stmts, reader) {
  params <- list()
  omega <- list()
  for (x in stmts) {
    if (is_call(x, "label")) {
      reader$lose("labels")
    } else if (is_call(x, "<-") && is.symbol(x[[2]])) {
      value <- rxode2_value(x[[3]], reader)
      params[[length(params) + 1]] <- data.frame(
        name = as.character(x[[2]]),
        role = "other",
        init = value$values[[if (length(value$values) == 1) 1 else 2]],
        lower = if (length(value$values) > 1) value$values[[1]] else -Inf,
        upper = if (length(value$values) > 2) value$values[[3]] else Inf,
        fixed = value$fixed,
        estimate = NA_real_
      )
    } else if (is_call(x, "~")) {
      names <- all.vars(x[[2]])
      value <- rxode2_value(x[[3]], reader)
      if (!is.symbol(x[[2]]) && !is_call(x[[2]], "+")) {
        reader$refuse(x, "a random effect name or a sum of names")
      }
      omega[[length(omega) + 1]] <- list(
        names = names,
        matrix = rxode2_block(names, value$values, x, reader),
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
rxode2_value <- function(x, reader) {
  fixed <- FALSE
  if (is_call(x, "fix") || is_call(x, "fixed")) {
    fixed <- TRUE
    args <- as.list(x)[-1]
    x <- if (length(args) == 1) args[[1]] else as.call(c(quote(c), args))
  }
  values <- if (is_call(x, "c")) as.list(x)[-1] else list(x)
  values <- vapply(values, rxode2_number, numeric(1), reader = reader)
  list(values = unname(values), fixed = fixed)
}

rxode2_number <- function(x, reader) {
  if (is.numeric(x) && length(x) == 1) {
    return(as.numeric(x))
  }
  if (identical(x, quote(Inf))) {
    return(Inf)
  }
  if (is_call(x, "-") && length(x) == 2) {
    return(-rxode2_number(x[[2]], reader))
  }
  reader$refuse(x, "a number")
}

# A block's values are its lower triangle, row by row.
rxode2_block <- function(names, values, x, reader) {
  n <- length(names)
  if (length(values) != n * (n + 1) / 2) {
    reader$refuse(x, sprintf("%d values for a block of %d", n * (n + 1) / 2, n))
  }
  m <- matrix(0, n, n, dimnames = list(names, names))
  m[upper.tri(m, diag = TRUE)] <- values
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}

model_parameters_empty <- function() {
  data.frame(
    name = character(),
    role = character(),
    init = numeric(),
    lower = numeric(),
    upper = numeric(),
    fixed = logical(),
    estimate = numeric()
  )
}

# Canonical closed-form parameters and the names linCmt() accepts for each.
lincmt_names <- list(
  cl = "cl",
  v = c("v", "vc", "v1"),
  q = c("q", "q1"),
  vp = c("vp", "v2", "vp1"),
  q2 = "q2",
  vp2 = c("vp2", "v3"),
  ka = "ka"
)

rxode2_dosing <- c(
  alag = "lag",
  lag = "lag",
  f = "bioavailability",
  rate = "rate",
  dur = "duration"
)

read_rxode2_model <- function(stmts, reader) {
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
      residuals[[as.character(x[[2]])]] <- rxode2_residual(x[[3]], x, reader)
    } else if (is_call(x, "<-") && is_ddt(x[[2]])) {
      derivatives[[as.character(x[[2]][[3]][[2]])]] <- x[[3]]
    } else if (is_call(x, "<-") && rxode2_is_dosing(x[[2]])) {
      # `alag(depot) <- e` names the compartment inside the call;
      # `depot(0) <- e` names it as the call.
      initial <- is.numeric(x[[2]][[2]])
      cmt <- as.character(x[[2]][[if (initial) 1 else 2]])
      attr <- if (initial) {
        "initial"
      } else {
        rxode2_dosing[[as.character(x[[2]][[1]])]]
      }
      if (is.null(dosing[[cmt]])) {
        dosing[[cmt]] <- list()
      }
      dosing[[cmt]][[attr]] <- x[[3]]
    } else if (is_call(x, "<-") && is_call(x[[3]], "linCmt")) {
      lincmt <- as.character(x[[2]])
    } else if (is_call(x, "<-") || is_call(x, "if")) {
      tryCatch(
        stmt_check(x),
        zupkpmx_model_error = function(e) reader$refuse(x, conditionMessage(e))
      )
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
    structure <- rxode2_lincmt(stmt_targets(individual), reader)
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

is_ddt <- function(x) {
  is_call(x, "/") &&
    identical(x[[2]], quote(d)) &&
    is_call(x[[3]], "dt") &&
    is.symbol(x[[3]][[2]])
}

rxode2_is_dosing <- function(x) {
  is.call(x) &&
    length(x) == 2 &&
    is.symbol(x[[1]]) &&
    ((as.character(x[[1]]) %in% names(rxode2_dosing) && is.symbol(x[[2]])) ||
      identical(x[[2]], 0) ||
      identical(x[[2]], 0L))
}

rxode2_residual <- function(x, stmt, reader) {
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
    a <- one(x[[2]], "add")
    p <- one(x[[3]], "prop")
    if (!is.null(a) && !is.null(p)) {
      return(list(type = "combined", parameters = c(a, p)))
    }
  }
  reader$refuse(
    stmt,
    "`add()`, `prop()`, `add() + prop()`, `lnorm()` or `pow()`"
  )
}

rxode2_lincmt <- function(targets, reader) {
  lower <- tolower(targets)
  found <- lapply(lincmt_names, function(alias) {
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
    advan = if (depot) c(2L, 4L, 12L)[[ncmt]] else c(1L, 3L, 11L)[[ncmt]],
    trans = if (ncmt == 1) 2L else 4L
  )
}

closed_compartments <- function(structure) {
  peripheral <- if (structure$ncmt > 1) {
    paste0("peripheral", seq_len(structure$ncmt - 1))
  }
  c(if (isTRUE(structure$depot)) "depot", "central", peripheral)
}

# Each eta attaches to the first individual statement that reads it.
rxode2_attach <- function(etas, individual) {
  rows <- lapply(etas, function(eta) {
    for (x in individual) {
      if (is_call(x, "<-") && eta %in% all.vars(x[[3]])) {
        return(data.frame(
          eta = eta,
          parameter = as.character(x[[2]]),
          transform = eta_transform(x[[3]], eta)
        ))
      }
    }
    NULL
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(
      eta = character(),
      parameter = character(),
      transform = character()
    )
  }
  out
}

eta_transform <- function(x, eta) {
  e <- as.symbol(eta)
  if (is_call(x, "exp") && is_call(x[[2]], "+") && identical(x[[2]][[3]], e)) {
    return("exp")
  }
  if (is_call(x, "*") && is_call(x[[3]], "exp") && identical(x[[3]][[2]], e)) {
    return("exp")
  }
  if (is_call(x, "+") && identical(x[[3]], e)) {
    return("additive")
  }
  "expression"
}

# A fixed effect takes the role of the closed-form parameter whose
# statement reads it as the only fixed effect.
rxode2_roles <- function(params, individual, structure) {
  roles <- rep("other", length(params))
  if (!identical(structure$type, "closed")) {
    return(roles)
  }
  role_of <- c(
    cl = "clearance",
    v = "volume",
    vp = "volume",
    vp2 = "volume",
    ka = "ka"
  )
  for (canonical in intersect(names(structure$parameters), names(role_of))) {
    target <- structure$parameters[[canonical]]
    for (x in individual) {
      if (is_call(x, "<-") && identical(as.character(x[[2]]), target)) {
        read <- intersect(all.vars(x[[3]]), params)
        if (length(read) == 1) {
          roles[params == read] <- role_of[[canonical]]
        }
      }
    }
  }
  roles
}

# Source references are where code was typed, not what it says; a model
# read with them would differ from the same model read without.
strip_srcref <- function(x) {
  if (is.call(x)) {
    attributes(x) <- NULL
    x[] <- lapply(as.list(x), strip_srcref)
    x <- as.call(x)
  }
  x
}

is_call <- function(x, name) {
  is.call(x) && identical(x[[1]], as.symbol(name))
}
