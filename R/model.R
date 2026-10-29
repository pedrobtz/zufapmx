# The representation (design §4.1): a classed list of seven components, each
# a documented data structure and none holding code. Readers build it with
# new_model(), which validates every component; nothing else constructs one.

model_components <- c(
  "parameters",
  "random_effects",
  "individual",
  "structure",
  "dosing",
  "observations",
  "provenance"
)

parameter_roles <- c("clearance", "volume", "ka", "other")
eta_transforms <- c("exp", "additive", "logit", "expression")
residual_types <- c(
  "additive",
  "proportional",
  "combined",
  "exponential",
  "power",
  "expression"
)
observation_transforms <- c("none", "log")
dosing_attributes <- c("lag", "bioavailability", "rate", "duration", "initial")
closed_parameters <- c("cl", "v", "q", "vp", "q2", "vp2", "ka")

new_model <- function(
  parameters,
  random_effects,
  individual,
  structure,
  dosing,
  observations,
  provenance
) {
  model <- structure(
    list(
      parameters = parameters,
      random_effects = random_effects,
      individual = individual,
      structure = structure,
      dosing = dosing,
      observations = observations,
      provenance = provenance
    ),
    class = "zufapmx_model"
  )
  validate_model(model)
}

#' Inspect a model
#'
#' `pmx_parameters()` returns the fixed effects, `pmx_omega()` the
#' between-subject variability matrix, and `pmx_ode()` the differential
#' equations of an ODE model.
#'
#' @param model A model read by one of the `pmx_read_*()` functions.
#' @param values For `pmx_omega()`: `"estimate"` (the default) returns the
#'   final estimates where the model was read with them, and `"init"` the
#'   initial values.
#' @return
#' * `pmx_parameters()`: a data frame with one row per fixed effect and
#'   columns `name` (the tool's name), `role` (`"clearance"`, `"volume"`,
#'   `"ka"` or `"other"`), `init`, `lower`, `upper`, `fixed`, and
#'   `estimate` (`NA` when the model was read without estimates).
#' * `pmx_omega()`: a symmetric matrix with the random effects' names as
#'   dimnames, built block-diagonally from the model's omega blocks. With
#'   `values = "estimate"`, a block read with estimates contributes them
#'   and any other block its initial values.
#' * `pmx_ode()`: a named list of expressions, one derivative per
#'   compartment, or `NULL` for a closed-form model.
#' @family models
#' @examples
#' path <- system.file(
#'   "corpus", "onecmt-oral-mm", "model.ctl",
#'   package = "zufapmx"
#' )
#' model <- pmx_read_nonmem(path)
#' pmx_parameters(model)
#' pmx_omega(model, "init")
#' pmx_ode(model)
#' @name pmx_inspect
NULL

#' @rdname pmx_inspect
#' @export
pmx_parameters <- function(model) {
  check_model(model)
  model$parameters
}

#' @rdname pmx_inspect
#' @export
pmx_omega <- function(model, values = c("estimate", "init")) {
  check_model(model)
  values <- check_choice(values)
  blocks <- model$random_effects$omega
  etas <- unlist(lapply(blocks, `[[`, "names"))
  out <- matrix(0, length(etas), length(etas), dimnames = list(etas, etas))
  at <- 0
  for (block in blocks) {
    i <- at + seq_along(block$names)
    out[i, i] <- if (values == "estimate" && !is.null(block$estimate)) {
      block$estimate
    } else {
      block$matrix
    }
    at <- at + length(block$names)
  }
  out
}

#' @rdname pmx_inspect
#' @export
pmx_ode <- function(model) {
  check_model(model)
  if (identical(model$structure$type, "ode")) model$structure$derivatives
}

#' @export
print.zufapmx_model <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
  invisible(x)
}

#' @export
format.zufapmx_model <- function(x, ...) {
  check_model(x)
  p <- x$parameters
  estimated <- sum(!is.na(p$estimate))
  attached <- x$random_effects$attach
  obs <- x$observations

  c(
    sprintf("<zufapmx_model> read from %s", x$provenance$format),
    paste("Structure:", format_structure(x$structure)),
    sprintf(
      "Parameters: %d, %s",
      nrow(p),
      if (estimated == 0) {
        "initial values only"
      } else {
        sprintf("%d with estimates", estimated)
      }
    ),
    paste(
      "Random effects:",
      if (nrow(attached) == 0) {
        "none"
      } else {
        paste(
          sprintf(
            "%s on %s (%s)",
            attached$eta,
            attached$parameter,
            attached$transform
          ),
          collapse = ", "
        )
      }
    ),
    if (length(x$dosing) > 0) {
      paste(
        "Dosing:",
        paste(
          vapply(
            names(x$dosing),
            function(cmt) {
              paste(cmt, paste(names(x$dosing[[cmt]]), collapse = ", "))
            },
            character(1)
          ),
          collapse = "; "
        )
      )
    },
    paste(
      "Endpoints:",
      paste(
        sprintf(
          "%s (%s)",
          names(obs),
          vapply(obs, function(o) o$residual$type, character(1))
        ),
        collapse = ", "
      )
    )
  )
}

format_structure <- function(x) {
  if (identical(x$type, "ode")) {
    return(sprintf(
      "ODE system over %d compartments: %s",
      length(x$compartments),
      paste(x$compartments, collapse = ", ")
    ))
  }
  sprintf(
    "%s-compartment closed form, %s input%s",
    c("one", "two", "three")[[x$ncmt]],
    if (isTRUE(x$depot)) "first-order" else "direct",
    if (!is.null(x$advan)) {
      sprintf(" (ADVAN%d TRANS%d)", x$advan, x$trans)
    } else {
      ""
    }
  )
}

validate_model <- function(model) {
  absent <- model_components[vapply(
    model_components,
    function(x) is.null(model[[x]]),
    logical(1)
  )]
  if (length(absent) > 0) {
    model_abort("The model has no %s.", paste(absent, collapse = ", "))
  }

  validate_parameters(model$parameters)
  lapply(model$individual, stmt_check)
  targets <- stmt_targets(model$individual)
  validate_random_effects(model$random_effects, targets)
  compartments <- validate_structure(
    model$structure,
    c(targets, model$parameters$name)
  )
  validate_dosing(model$dosing, compartments)
  validate_observations(model$observations)
  validate_assignments(model, compartments)
  model
}

# Who may assign what. Parameters, random effects, compartments and time
# are inputs, never assigned (#82). ODE code re-runs at every solver step
# in NONMEM's $DES and mrgsolve's $ODE, so it may not reassign an
# individual name (#81). Individual statements run once per record, so they
# may not read compartment amounts (D18). A name an `if` assigns must hold
# a value before it, so every evaluation starts from the same one (D14).
validate_assignments <- function(model, compartments) {
  individual <- model$individual
  ode_code <- model$structure$statements
  assigned <- intersect(
    stmt_targets(c(individual, ode_code)),
    model_inputs(model)
  )
  if (length(assigned) > 0) {
    model_abort(
      paste(
        "`%s` is a parameter, random effect, compartment or time;",
        "it cannot be assigned."
      ),
      assigned[[1]]
    )
  }
  shared <- intersect(stmt_targets(ode_code), stmt_targets(individual))
  if (length(shared) > 0) {
    model_abort(
      "`%s` is assigned both by individual statements and by the ODE code.",
      shared[[1]]
    )
  }
  amounts <- intersect(stmt_reads(individual), compartments)
  if (length(amounts) > 0) {
    model_abort(
      "Individual statements read the compartment amount `%s`.",
      amounts[[1]]
    )
  }
  check_zero_start(individual)
  check_zero_start(ode_code, stmt_targets(individual))
}

check_zero_start <- function(stmts, assigned = character()) {
  for (x in stmts) {
    if (is_call(x, "if")) {
      unset <- setdiff(stmt_targets(list(x)), assigned)
      if (length(unset) > 0) {
        model_abort(
          "`%s` is assigned only inside an `if`; give it a value before.",
          unset[[1]]
        )
      }
    } else {
      assigned <- c(assigned, as.character(x[[2]]))
    }
  }
}

# Names a statement list reads, including in `if` conditions.
stmt_reads <- function(stmts) {
  unique(unlist(lapply(stmts, function(x) {
    if (is_call(x, "<-")) all.vars(x[[3]]) else all.vars(x)
  })))
}

validate_parameters <- function(x) {
  columns <- c("name", "role", "init", "lower", "upper", "fixed", "estimate")
  if (!is.data.frame(x) || !identical(names(x), columns)) {
    model_abort(
      "Parameters must be a data frame with columns %s.",
      paste(columns, collapse = ", ")
    )
  }
  types <- list(
    name = is.character,
    role = is.character,
    init = is.numeric,
    lower = is.numeric,
    upper = is.numeric,
    fixed = is.logical,
    estimate = is.numeric
  )
  for (col in names(types)) {
    # Only an estimate may be missing: a parameter read without one.
    if (!types[[col]](x[[col]]) || (col != "estimate" && anyNA(x[[col]]))) {
      model_abort(
        "Parameter column `%s` must be %s with no missing values.",
        col,
        switch(
          col,
          name = ,
          role = "character",
          fixed = "logical",
          "numeric"
        )
      )
    }
  }
  if (anyDuplicated(x$name)) {
    model_abort(
      "Parameter `%s` is defined twice.",
      x$name[duplicated(x$name)][[1]]
    )
  }
  bad <- setdiff(x$role, parameter_roles)
  if (length(bad) > 0) {
    model_abort("`%s` is not a parameter role.", bad[[1]])
  }
}

validate_random_effects <- function(x, targets) {
  blocks <- c(x$omega, x$sigma)
  lapply(blocks, validate_block)

  etas <- unlist(lapply(x$omega, `[[`, "names"))
  unknown_eta <- setdiff(x$attach$eta, etas)
  if (length(unknown_eta) > 0) {
    model_abort("`%s` is attached but not in omega.", unknown_eta[[1]])
  }
  unknown_param <- setdiff(x$attach$parameter, targets)
  if (length(unknown_param) > 0) {
    model_abort(
      "A random effect is attached to `%s`, which no statement assigns.",
      unknown_param[[1]]
    )
  }
  bad <- setdiff(x$attach$transform, eta_transforms)
  if (length(bad) > 0) {
    model_abort("`%s` is not a random-effect transformation.", bad[[1]])
  }
}

validate_block <- function(block) {
  m <- block$matrix
  n <- length(block$names)
  if (!is.matrix(m) || !is.numeric(m) || nrow(m) != n || ncol(m) != n) {
    model_abort(
      "The block for %s must be a %d by %d numeric matrix.",
      paste(block$names, collapse = ", "),
      n,
      n
    )
  }
  if (!isSymmetric(unname(m))) {
    model_abort(
      "The block for %s is not symmetric.",
      paste(block$names, collapse = ", ")
    )
  }
  est <- block$estimate
  if (
    !is.null(est) && (!identical(dim(est), dim(m)) || !isSymmetric(unname(est)))
  ) {
    model_abort(
      "The estimate for %s must be a symmetric matrix like its block.",
      paste(block$names, collapse = ", ")
    )
  }
}

# Returns the structure's compartment names.
validate_structure <- function(x, targets) {
  if (identical(x$type, "closed")) {
    if (!x$ncmt %in% 1:3) {
      model_abort(
        "A closed-form model has 1 to 3 compartments, not %s.",
        x$ncmt
      )
    }
    unknown <- setdiff(names(x$parameters), closed_parameters)
    if (length(unknown) > 0) {
      model_abort("`%s` is not a closed-form parameter.", unknown[[1]])
    }
    unassigned <- setdiff(x$parameters, targets)
    if (length(unassigned) > 0) {
      model_abort(
        "The closed form uses `%s`, which is neither a parameter nor assigned.",
        unassigned[[1]]
      )
    }
    return(closed_compartments(x))
  }

  if (identical(x$type, "ode")) {
    lapply(x$statements, stmt_check)
    if (!setequal(names(x$derivatives), x$compartments)) {
      model_abort(
        "An ODE model needs one derivative for each of %s.",
        paste(x$compartments, collapse = ", ")
      )
    }
    lapply(x$derivatives, expr_check)
    return(x$compartments)
  }

  model_abort("The structure type must be \"closed\" or \"ode\".")
}

validate_dosing <- function(x, compartments) {
  unknown <- setdiff(names(x), compartments)
  if (length(unknown) > 0) {
    model_abort("Dosing names `%s`, which is not a compartment.", unknown[[1]])
  }
  for (cmt in names(x)) {
    bad <- setdiff(names(x[[cmt]]), dosing_attributes)
    if (length(bad) > 0) {
      model_abort("`%s` is not a dosing attribute.", bad[[1]])
    }
    lapply(Filter(Negate(is.null), x[[cmt]]), expr_check)
  }
}

validate_observations <- function(x) {
  if (length(x) > 0 && (is.null(names(x)) || !all(nzchar(names(x))))) {
    model_abort("Every endpoint needs a name.")
  }
  for (endpoint in names(x)) {
    obs <- x[[endpoint]]
    expr_check(obs$prediction)
    if (!obs$residual$type %in% residual_types) {
      model_abort("`%s` is not a residual model.", obs$residual$type)
    }
    if (!obs$transform %in% observation_transforms) {
      model_abort("`%s` is not an observation transform.", obs$transform)
    }
  }
}

model_abort <- function(fmt, ...) {
  zufapmx_abort(sprintf(fmt, ...), "model", call = sys.call(-1))
}

# A model handed to an exported function is validated again: it is data
# (D3), and may have been edited since it was read.
check_model <- function(x) {
  if (!inherits(x, "zufapmx_model")) {
    model_abort("Expected a zufapmx model, not %s.", class(x)[[1]])
  }
  validate_model(x)
}

# Argument checks shared by the exported functions. Both raise a model
# error, the class for invalid input that is not a model file.
check_choice <- function(
  arg,
  choices = eval(formals(sys.function(-1))[[name]], envir = sys.frame(-1)),
  name = deparse(substitute(arg))
) {
  if (identical(arg, choices)) {
    return(choices[[1]])
  }
  if (!is.character(arg) || length(arg) != 1 || !arg %in% choices) {
    zufapmx_abort(
      sprintf(
        "`%s` must be one of %s.",
        name,
        paste0("\"", choices, "\"", collapse = ", ")
      ),
      "model",
      call = sys.call(-1)
    )
  }
  arg
}

check_file <- function(file) {
  if (
    !is.character(file) ||
      length(file) != 1 ||
      is.na(file) ||
      !file.exists(file) ||
      dir.exists(file)
  ) {
    zufapmx_abort(
      "`file` must be the path of an existing file.",
      "model",
      call = sys.call(-1)
    )
  }
}

# Every expression of a model: statements, derivatives, dosing, predictions.
model_expressions <- function(m) {
  c(
    m$individual,
    m$structure$statements,
    m$structure$derivatives,
    unlist(m$dosing, recursive = FALSE),
    lapply(m$observations, `[[`, "prediction"),
    lapply(m$observations, function(o) o$residual$expression)
  )
}

# The names a model takes as given: parameters, random effects,
# compartments and time. Statements never assign them.
model_inputs <- function(m) {
  c(
    m$parameters$name,
    unlist(lapply(
      c(m$random_effects$omega, m$random_effects$sigma),
      `[[`,
      "names"
    )),
    model_compartments(m$structure),
    "t"
  )
}

# Rename symbols throughout a model's expressions: statements, ODE code,
# dosing, predictions and residual expressions. `individual = FALSE` and
# `observations = FALSE` leave those parts as they are, for a caller that
# renamed them itself or keeps the old names as aliases.
model_rename <- function(m, map, individual = TRUE, observations = TRUE) {
  rename <- function(x) rename_symbols(x, map)
  if (individual) {
    m$individual <- lapply(m$individual, rename)
  }
  if (identical(m$structure$type, "ode")) {
    m$structure$statements <- lapply(m$structure$statements, rename)
    m$structure$derivatives <- lapply(m$structure$derivatives, rename)
  }
  m$dosing <- lapply(m$dosing, function(d) lapply(d, rename))
  if (observations) {
    m$observations <- lapply(m$observations, function(o) {
      o$prediction <- rename(o$prediction)
      if (!is.null(o$residual$expression)) {
        o$residual$expression <- rename(o$residual$expression)
      }
      o
    })
  }
  m
}
