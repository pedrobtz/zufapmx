# Helpers every reader shares: walking R syntax, attaching random effects,
# inferring parameter roles, and the shapes of closed-form models.

# A full symmetric matrix from lower-triangle values listed row by row.
lower_to_full <- function(values) {
  n <- (sqrt(8 * length(values) + 1) - 1) / 2
  m <- matrix(0, n, n)
  m[upper.tri(m, diag = TRUE)] <- values
  m[lower.tri(m)] <- t(m)[lower.tri(m)]
  m
}

is_call <- function(x, name) {
  is.call(x) && identical(x[[1]], as.symbol(name))
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

# Each eta attaches to the first individual statement that reads it.
attach_etas <- function(etas, individual) {
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
infer_roles <- function(params, individual, structure) {
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

closed_compartments <- function(structure) {
  peripheral <- if (structure$ncmt > 1) {
    paste0("peripheral", seq_len(structure$ncmt - 1))
  }
  c(if (isTRUE(structure$depot)) "depot", "central", peripheral)
}

strip_parens <- function(e) {
  if (is_call(e, "(")) {
    return(strip_parens(e[[2]]))
  }
  if (is.call(e)) {
    e[-1] <- lapply(as.list(e)[-1], strip_parens)
  }
  e
}
