#' Check that two models are the same model
#'
#' Simulates both models on one design, each through its own engine, and
#' compares their predictions. This is the check a translation exists to
#' pass: a model read from one format and written to another should
#' predict the same values for the same subjects.
#'
#' Each side runs on an engine (rxode2 or mrgsolve):
#'
#' * A written rxode2 or mrgsolve model runs as written, on its own
#'   format's engine. That is the text under test.
#' * A model, or a written NONMEM model, is written by zufapmx to an engine
#'   and runs there: its source format's engine when that is rxode2 or
#'   mrgsolve, and rxode2 otherwise.
#'
#' A side never runs on the engine of the other side's written text, so a
#' translation is never checked by the writer that made it: `a` compared
#' with `pmx_write_rxode2(a)` runs `a` on mrgsolve. `engines` overrides the
#' choice, including this rule.
#'
#' Predictions are compared after aligning on regimen, subject and time.
#' A point agrees when its relative deviation is at most `tol` or its
#' absolute deviation at most `atol`, which covers predictions near zero.
#' The relative deviation is `abs(a - b) / max(abs(a), abs(b))`. A missing
#' or non-finite prediction never agrees. Residual error is off.
#'
#' A comparison that fails is a value, not an error: `pass` is `FALSE` and
#' the result says where the models differ. Models that cannot be compared
#' point by point, because their numbers of random effects differ, fail
#' with a `reason` and no predictions.
#'
#' @param a,b The models to compare: each a model read by a `pmx_read_*()`
#'   function, or a model written by a `pmx_write_*()` function.
#' @param design A design from [pmx_design()].
#' @param tol The relative tolerance, a positive number.
#' @param atol The absolute tolerance, a non-negative number.
#' @param engines `NULL` to choose engines as described above, or a
#'   character vector of two engines, `"rxode2"` or `"mrgsolve"`, for `a`
#'   and `b`. `NA` in either place chooses that side's engine as usual.
#' @return A `zufapmx_comparison`, a list with:
#'   \describe{
#'     \item{pass}{`TRUE` when every prediction agrees.}
#'     \item{reason}{Why the models could not be compared point by point,
#'       or `NULL`.}
#'     \item{worst}{A one-row data frame of the worst deviation, or `NULL`
#'       without predictions: the first missing or non-finite prediction,
#'       or else the largest relative deviation among points outside
#'       `atol`, or else the largest absolute deviation.}
#'     \item{regimens}{One row per regimen: the number of points, the
#'       number that disagree, the largest absolute and relative
#'       deviations, and the reason a regimen was skipped.}
#'     \item{predictions}{One row per point: `regimen`, `id`, `time`, the
#'       two predictions `a` and `b`, the deviations `abs` and `rel`, and
#'       whether it agrees (`pass`).}
#'     \item{engines}{The engine and its version for each side.}
#'     \item{sides}{For each side, how it was simulated, its provenance and
#'       the model text the engine ran.}
#'     \item{design, tol, atol}{The arguments.}
#'   }
#' @seealso [pmx_report()] writes a comparison as Markdown.
#' @family comparison
#' @export
#' @examplesIf requireNamespace("rxode2", quietly = TRUE)
#' path <- system.file(
#'   "corpus", "onecmt-oral", "model.ctl",
#'   package = "zufapmx"
#' )
#' model <- pmx_read_nonmem(path)
#' # Check the NONMEM writer. Both sides run on rxode2, since NONMEM is not
#' # an engine here; pmx_write_rxode2(model) would run `model` on mrgsolve.
#' written <- pmx_write_nonmem(model)
#' cmp <- pmx_compare(model, written, pmx_design(n = 2, regimens = "single"))
#' cmp
pmx_compare <- function(
  a,
  b,
  design = pmx_design(),
  tol = 1e-6,
  atol = 1e-9,
  engines = NULL
) {
  check_comparable(a, "a")
  check_comparable(b, "b")
  check_design(design)
  check_tolerance(tol, positive = TRUE)
  check_tolerance(atol, positive = FALSE)
  engines <- check_engines(engines)

  sides <- list(
    a = compare_side(a, engines[[1]], written_engine(b)),
    b = compare_side(b, engines[[2]], written_engine(a))
  )
  out <- compare_sides(sides, design, tol, atol)
  structure(
    c(
      out,
      list(
        engines = data.frame(
          side = c("a", "b"),
          engine = c(sides$a$engine, sides$b$engine),
          version = c(
            engine_version(sides$a$engine),
            engine_version(sides$b$engine)
          )
        ),
        sides = lapply(sides, function(s) {
          s[c("engine", "origin", "provenance", "text")]
        }),
        design = design,
        tol = tol,
        atol = atol
      )
    ),
    class = "zufapmx_comparison"
  )
}

compare_sides <- function(sides, design, tol, atol) {
  models <- lapply(sides, function(s) unclass(s$model))
  for (m in models) {
    if (length(m$observations) != 1) {
      zufapmx_abort(
        "pmx_compare() compares models with one endpoint.",
        "model",
        call = NULL
      )
    }
  }
  omegas <- lapply(sides, function(s) pmx_omega(s$model, "estimate"))
  if (ncol(omegas$a) != ncol(omegas$b)) {
    return(compare_failure(sprintf(
      "The models have %d and %d random effects.",
      ncol(omegas$a),
      ncol(omegas$b)
    )))
  }
  covariates <- lapply(models, function(m) {
    compare_covariates(m$provenance$covariates, design$covariates)
  })
  etas <- compare_etas(omegas, design)

  rows <- list()
  regimens <- list()
  for (regimen in design$regimens) {
    skip <- compare_skip(models, regimen, design$cmt)
    events <- design$events[design$events$regimen == regimen, ]
    obs <- events[events$evid == 0, ]
    if (!is.null(skip)) {
      regimens[[regimen]] <- compare_regimen(regimen, NULL, skip)
      next
    }
    pred <- lapply(c(a = "a", b = "b"), function(s) {
      data <- side_events(models[[s]], events, design$cmt, sides[[s]]$engine)
      if (!is.null(covariates[[s]])) {
        data <- cbind(data, covariates[[s]])
      }
      engine_simulate(sides[[s]], data, etas[[s]])
    })
    points <- compare_points(regimen, obs, pred$a, pred$b, tol, atol)
    rows[[regimen]] <- points
    regimens[[regimen]] <- compare_regimen(regimen, points, NA_character_)
  }
  predictions <- do.call(rbind, c(unname(rows), make.row.names = FALSE))
  list(
    pass = !is.null(predictions) &&
      nrow(predictions) > 0 &&
      all(predictions$pass),
    reason = NULL,
    worst = compare_worst(predictions, atol),
    regimens = do.call(rbind, c(unname(regimens), make.row.names = FALSE)),
    predictions = predictions
  )
}

compare_failure <- function(reason) {
  list(
    pass = FALSE,
    reason = reason,
    worst = NULL,
    regimens = NULL,
    predictions = NULL
  )
}

# Each covariate of a model with its design value, named in the model's
# spelling. A covariate the design lacks is the caller's to supply.
compare_covariates <- function(names, values) {
  at <- match(toupper(names), toupper(names(values)))
  if (anyNA(at)) {
    zufapmx_abort(
      sprintf(
        "The design has no value for the covariate%s %s; add %s to `covariates`.",
        if (sum(is.na(at)) > 1) "s" else "",
        paste0("`", names[is.na(at)], "`", collapse = ", "),
        if (sum(is.na(at)) > 1) "them" else "it"
      ),
      "model",
      call = NULL
    )
  }
  out <- stats::setNames(as.data.frame(values[at]), names)
  if (length(names) == 0) NULL else out
}

# The random effects of every subject on both sides: the typical subject
# at zero, then the design's standard normal draws times each side's
# omega square root. Both sides use the same draws, so equal omegas give
# equal etas. The draw leaves the caller's random number stream as it was.
compare_etas <- function(omegas, design) {
  k <- ncol(omegas$a)
  z <- with_seed(design$seed, stats::rnorm(design$n * k))
  z <- rbind(rep(0, k), matrix(z, design$n, k))
  lapply(omegas, function(omega) {
    out <- z %*% omega_root(omega)
    colnames(out) <- colnames(omega)
    out
  })
}

# The symmetric square root of a covariance matrix, defined for a
# semi-definite one (an eta fixed at zero variance).
omega_root <- function(omega) {
  if (length(omega) == 0) {
    return(omega)
  }
  e <- eigen(omega, symmetric = TRUE)
  if (any(e$values < -sqrt(.Machine$double.eps) * max(abs(e$values), 1))) {
    zufapmx_abort(
      "A model's omega is not positive semi-definite.",
      "model",
      call = NULL
    )
  }
  e$vectors %*% diag(sqrt(pmax(e$values, 0)), length(e$values)) %*% t(e$vectors)
}

with_seed <- function(seed, expr) {
  env <- globalenv()
  had <- exists(".Random.seed", envir = env, inherits = FALSE)
  if (had) {
    old <- get(".Random.seed", envir = env, inherits = FALSE)
  }
  old_kind <- RNGkind()
  on.exit({
    RNGkind(old_kind[[1]], old_kind[[2]], old_kind[[3]])
    if (had) {
      assign(".Random.seed", old, envir = env)
    } else {
      rm(".Random.seed", envir = env)
    }
  })
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  expr
}

# Why a regimen cannot be given to a model, or NULL. A model that sets
# the dose compartment's rate or duration doses at that rate whatever
# the regimen, so the infusion regimen would test nothing new.
compare_skip <- function(models, regimen, cmt) {
  for (m in models) {
    compartments <- model_compartments(m$structure)
    if (cmt > length(compartments)) {
      zufapmx_abort(
        sprintf(
          "The design doses compartment %d, but a model has %d.",
          cmt,
          length(compartments)
        ),
        "model",
        call = NULL
      )
    }
    dosing <- names(m$dosing[[compartments[[cmt]]]])
    if (regimen == "infusion" && any(c("rate", "duration") %in% dosing)) {
      return("a model sets the dose's rate or duration")
    }
  }
  NULL
}

# A regimen's records for one side: the dose compartment in the engine's
# terms (rxode2 by name, mrgsolve by number), and NONMEM's rate -1 or -2
# where the model sets the dose's rate or duration.
side_events <- function(m, events, cmt, engine) {
  compartments <- model_compartments(m$structure)
  dosing <- names(m$dosing[[compartments[[cmt]]]])
  data <- events[, setdiff(names(events), "regimen")]
  data$cmt <- if (engine == "rxode2") compartments[[cmt]] else cmt
  dose <- data$evid == 1
  if ("rate" %in% dosing) {
    data$rate[dose] <- -1
  } else if ("duration" %in% dosing) {
    data$rate[dose] <- -2
  }
  data
}

# A missing or non-finite prediction deviates infinitely: it never agrees.
compare_points <- function(regimen, obs, a, b, tol, atol) {
  finite <- is.finite(a) & is.finite(b)
  dev <- ifelse(finite, abs(a - b), Inf)
  rel <- ifelse(finite & dev == 0, 0, dev / pmax(abs(a), abs(b)))
  rel[!finite] <- Inf
  data.frame(
    regimen = regimen,
    id = obs$id,
    time = obs$time,
    a = a,
    b = b,
    abs = dev,
    rel = rel,
    pass = finite & (rel <= tol | dev <= atol)
  )
}

compare_regimen <- function(regimen, points, skipped) {
  if (is.null(points)) {
    return(data.frame(
      regimen = regimen,
      points = 0L,
      failed = 0L,
      max_abs = NA_real_,
      max_rel = NA_real_,
      skipped = skipped
    ))
  }
  data.frame(
    regimen = regimen,
    points = nrow(points),
    failed = sum(!points$pass),
    max_abs = max(points$abs),
    max_rel = max(points$rel),
    skipped = skipped
  )
}

compare_worst <- function(p, atol) {
  if (is.null(p) || nrow(p) == 0) {
    return(NULL)
  }
  bad <- which(!is.finite(p$a) | !is.finite(p$b))
  i <- if (length(bad) > 0) {
    bad[[1]]
  } else if (any(p$abs > atol)) {
    outside <- which(p$abs > atol)
    outside[[which.max(p$rel[outside])]]
  } else {
    which.max(p$abs)
  }
  out <- p[i, c("regimen", "id", "time", "a", "b", "abs", "rel")]
  rownames(out) <- NULL
  out
}

#' @export
print.zufapmx_comparison <- function(x, ...) {
  cat(sprintf(
    "<zufapmx comparison> %s\n",
    if (x$pass) "pass" else "FAIL"
  ))
  for (s in c("a", "b")) {
    side <- x$sides[[s]]
    cat(sprintf(
      "%s: %s, on %s %s\n",
      s,
      side$origin,
      side$engine,
      x$engines$version[x$engines$side == s]
    ))
  }
  cat(sprintf(
    "Tolerance: relative %s, absolute %s.\n",
    format(x$tol),
    format(x$atol)
  ))
  if (!is.null(x$reason)) {
    cat(x$reason, "\n", sep = "")
    return(invisible(x))
  }
  p <- x$predictions
  cat(sprintf(
    "%d of %d predictions disagree.\n",
    sum(!p$pass),
    nrow(p)
  ))
  w <- x$worst
  if (!is.null(w)) {
    cat(sprintf(
      "Worst: %s, subject %d, time %s: %s against %s (abs %s, rel %s).\n",
      w$regimen,
      w$id,
      format(w$time),
      format(w$a, digits = 8),
      format(w$b, digits = 8),
      format(w$abs, digits = 3),
      format(w$rel, digits = 3)
    ))
  }
  r <- x$regimens
  r$max_abs <- signif(r$max_abs, 3)
  r$max_rel <- signif(r$max_rel, 3)
  r$skipped[is.na(r$skipped)] <- ""
  print(r, row.names = FALSE)
  invisible(x)
}

check_comparable <- function(x, name) {
  if (inherits(x, "zufapmx_model")) {
    validate_model(x)
    return(invisible())
  }
  if (inherits(x, "zufapmx_written")) {
    return(invisible())
  }
  zufapmx_abort(
    sprintf(
      "`%s` must be a model from pmx_read_*() or pmx_write_*(), not %s.",
      name,
      class(x)[[1]]
    ),
    "model",
    call = sys.call(-1)
  )
}

check_tolerance <- function(x, positive, name = deparse(substitute(x))) {
  if (
    !is.numeric(x) ||
      length(x) != 1 ||
      !is.finite(x) ||
      x < 0 ||
      (positive && x == 0)
  ) {
    zufapmx_abort(
      sprintf(
        "`%s` must be a %s number.",
        name,
        if (positive) "positive" else "non-negative"
      ),
      "model",
      call = sys.call(-1)
    )
  }
}

check_engines <- function(engines) {
  if (is.null(engines)) {
    return(c(NA_character_, NA_character_))
  }
  if (
    !(is.character(engines) || all(is.na(engines))) ||
      length(engines) != 2 ||
      !all(is.na(engines) | engines %in% engine_names)
  ) {
    zufapmx_abort(
      sprintf(
        "`engines` must be NULL or two of %s or NA, for `a` and `b`.",
        paste0("\"", engine_names, "\"", collapse = ", ")
      ),
      "model",
      call = sys.call(-1)
    )
  }
  as.character(engines)
}
