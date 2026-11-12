#' A simulation design for comparing models
#'
#' Describes what [pmx_compare()] simulates: the regimens, the subjects and
#' the covariate values. Each regimen doses compartment `cmt` (by position,
#' as NONMEM numbers compartments) with 100 units and observes at fixed
#' times:
#'
#' * `"single"`: one dose at time 0. Into a depot this is a first-order
#'   dose, into the central compartment a bolus. Observed at 0.5, 1, 2, 4,
#'   6, 8, 12 and 24.
#' * `"infusion"`: one dose at time 0 over 2 time units. Observed at the
#'   same times as `"single"`.
#' * `"multiple"`: five doses 12 time units apart (`ii = 12`, `addl = 4`).
#'   Observed at 1, 6, 11.5, 13, 18, 24.5, 36.5, 48.5, 50, 54, 60 and 72,
#'   never at a dose time.
#' * `"ss"`: a dose at steady state (`ss = 1`, `ii = 12`). Observed at
#'   0.5, 1, 2, 4, 6, 8 and 11.5.
#'
#' A model that sets the dose compartment's rate or duration itself is
#' dosed with `rate = -1` or `rate = -2`, as NONMEM's data sets do, and its
#' `"infusion"` regimen is skipped.
#'
#' Subject 1 is the typical subject, with every random effect at zero.
#' Subjects 2 to `n + 1` take random effects drawn from each model's omega
#' with `seed`. Both models get the same standard normal draws, so models
#' with the same omega get the same random effects. Residual error is
#' always off: a comparison is of predictions, not of noise.
#'
#' @param n The number of subjects with random effects, besides the
#'   typical subject. From 0 to 10000.
#' @param seed The seed of the random effects, a whole number. It is part
#'   of the design, so the same design always draws the same values.
#' @param regimens The regimens to simulate, a subset of `"single"`,
#'   `"infusion"`, `"multiple"` and `"ss"`.
#' @param cmt The compartment every regimen doses, by position. The
#'   default, 1, is NONMEM's default dose compartment: the depot of a model
#'   with first-order absorption.
#' @param covariates A named list or numeric vector of covariate values,
#'   one number each, given to every subject. Names match a model's
#'   covariates without regard to case, since NONMEM upper-cases them.
#' @return A `zupmxio_design`: a list with the arguments and `events`, a
#'   data frame of the dose and observation records of every regimen with
#'   columns `regimen`, `id`, `time`, `evid`, `amt`, `rate`, `ii`, `addl`
#'   and `ss`.
#' @family comparison
#' @export
#' @examples
#' pmx_design()
#' pmx_design(n = 5, regimens = c("single", "ss"), covariates = list(WT = 70))
pmx_design <- function(
  n = 20,
  seed = 1,
  regimens = c("single", "infusion", "multiple", "ss"),
  cmt = 1,
  covariates = list()
) {
  check_count(n, 0, design_max_subjects)
  check_whole(seed)
  if (
    !is.character(regimens) ||
      length(regimens) == 0 ||
      anyNA(regimens) ||
      !all(regimens %in% names(design_regimens)) ||
      anyDuplicated(regimens)
  ) {
    zupmxio_abort(
      sprintf(
        "`regimens` must be distinct values among %s.",
        paste0("\"", names(design_regimens), "\"", collapse = ", ")
      ),
      "model"
    )
  }
  check_count(cmt, 1, design_max_cmt)
  covariates <- check_covariates(covariates)

  n <- as.integer(n)
  ids <- seq_len(n + 1L)
  events <- do.call(
    rbind,
    c(
      lapply(regimens, function(r) design_events(r, ids)),
      make.row.names = FALSE
    )
  )
  structure(
    list(
      n = n,
      seed = as.numeric(seed),
      regimens = regimens,
      cmt = as.integer(cmt),
      covariates = covariates,
      events = events
    ),
    class = "zupmxio_design"
  )
}

design_max_subjects <- 10000
design_max_cmt <- 1000

# The regimens of design §4.4 as dose records and observation times. Every
# regimen doses 100 units at time 0; observation times avoid dose times,
# where engines could order an observation and a dose differently.
design_regimens <- list(
  single = list(
    dose = list(rate = 0, ii = 0, addl = 0L, ss = 0L),
    times = c(0.5, 1, 2, 4, 6, 8, 12, 24)
  ),
  infusion = list(
    dose = list(rate = 50, ii = 0, addl = 0L, ss = 0L),
    times = c(0.5, 1, 2, 4, 6, 8, 12, 24)
  ),
  multiple = list(
    dose = list(rate = 0, ii = 12, addl = 4L, ss = 0L),
    times = c(1, 6, 11.5, 13, 18, 24.5, 36.5, 48.5, 50, 54, 60, 72)
  ),
  ss = list(
    dose = list(rate = 0, ii = 12, addl = 0L, ss = 1L),
    times = c(0.5, 1, 2, 4, 6, 8, 11.5)
  )
)

design_dose <- 100

design_events <- function(regimen, ids) {
  r <- design_regimens[[regimen]]
  one <- data.frame(
    time = c(0, r$times),
    evid = c(1L, rep(0L, length(r$times))),
    amt = c(design_dose, rep(0, length(r$times))),
    rate = c(r$dose$rate, rep(0, length(r$times))),
    ii = c(r$dose$ii, rep(0, length(r$times))),
    addl = c(r$dose$addl, rep(0L, length(r$times))),
    ss = c(r$dose$ss, rep(0L, length(r$times)))
  )
  out <- one[rep(seq_len(nrow(one)), length(ids)), ]
  out <- cbind(
    regimen = regimen,
    id = rep(ids, each = nrow(one)),
    out
  )
  rownames(out) <- NULL
  out
}

#' @export
print.zupmxio_design <- function(x, ...) {
  cat(sprintf(
    "<zupmxio design> %s, dosing compartment %d\n",
    paste(x$regimens, collapse = ", "),
    x$cmt
  ))
  cat(sprintf(
    "The typical subject and %d with random effects (seed %s).\n",
    x$n,
    format(x$seed)
  ))
  if (length(x$covariates) > 0) {
    cat(sprintf(
      "Covariates: %s.\n",
      paste(names(x$covariates), x$covariates, sep = " = ", collapse = ", ")
    ))
  }
  invisible(x)
}

check_design <- function(x) {
  if (!inherits(x, "zupmxio_design")) {
    zupmxio_abort(
      sprintf(
        "`design` must be a design from pmx_design(), not %s.",
        class(x)[[1]]
      ),
      "model",
      call = sys.call(-1)
    )
  }
}

# A whole number from `lower` to `upper`.
check_count <- function(x, lower, upper, name = deparse(substitute(x))) {
  if (
    !is.numeric(x) ||
      length(x) != 1 ||
      !is.finite(x) ||
      x != round(x) ||
      x < lower ||
      x > upper
  ) {
    zupmxio_abort(
      sprintf("`%s` must be a whole number from %d to %d.", name, lower, upper),
      "model",
      call = sys.call(-1)
    )
  }
}

check_whole <- function(x, name = deparse(substitute(x))) {
  if (
    !is.numeric(x) ||
      length(x) != 1 ||
      !is.finite(x) ||
      x != round(x) ||
      abs(x) > .Machine$integer.max
  ) {
    zupmxio_abort(
      sprintf("`%s` must be a whole number.", name),
      "model",
      call = sys.call(-1)
    )
  }
}

check_covariates <- function(x) {
  if (is.numeric(x) && !is.null(names(x))) {
    x <- as.list(x)
  }
  ok <- is.list(x) &&
    (length(x) == 0 ||
      (!is.null(names(x)) &&
        all(nzchar(names(x))) &&
        !anyNA(names(x)) &&
        !anyDuplicated(toupper(names(x))) &&
        all(vapply(
          x,
          function(v) is.numeric(v) && length(v) == 1 && is.finite(v),
          logical(1)
        ))))
  if (!ok) {
    zupmxio_abort(
      paste(
        "`covariates` must be a named list of numbers, one per covariate,",
        "with names distinct without regard to case."
      ),
      "model",
      call = sys.call(-1)
    )
  }
  lapply(x, as.numeric)
}
