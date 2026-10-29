# Closed-form (PREDPP ADVAN) models, shared by every format: the ADVANs
# and TRANS this package reads, their parameter names, compartments and
# how a model's names fill them.

closed_forms <- list(
  `1` = list(
    trans = 2L,
    cmt = "central",
    central = 1L,
    names = c(cl = "CL", v = "V")
  ),
  `2` = list(
    trans = 2L,
    cmt = c("depot", "central"),
    central = 2L,
    names = c(cl = "CL", v = "V", ka = "KA")
  ),
  `3` = list(
    trans = 4L,
    cmt = c("central", "peripheral1"),
    central = 1L,
    names = c(cl = "CL", v = "V1", q = "Q", vp = "V2")
  ),
  `4` = list(
    trans = 4L,
    cmt = c("depot", "central", "peripheral1"),
    central = 2L,
    names = c(cl = "CL", v = "V2", q = "Q", vp = "V3", ka = "KA")
  ),
  `11` = list(
    trans = 4L,
    cmt = c("central", "peripheral1", "peripheral2"),
    central = 1L,
    names = c(cl = "CL", v = "V1", q = "Q2", vp = "V2", q2 = "Q3", vp2 = "V3")
  ),
  `12` = list(
    trans = 4L,
    cmt = c("depot", "central", "peripheral1", "peripheral2"),
    central = 2L,
    names = c(
      cl = "CL",
      v = "V2",
      q = "Q3",
      vp = "V3",
      q2 = "Q4",
      vp2 = "V4",
      ka = "KA"
    )
  )
)

# The variables a closed form's ADVAN parameters are. `names` are the
# candidates: $PK targets, and for mrgsolve also $PARAM names. `form` is
# list(advan, trans).
closed_structure <- function(form, closed, candidates, file, record) {
  targets <- candidates
  found <- vapply(
    closed$names,
    function(name) {
      # An exact match first: C++ (mrgsolve) is case-sensitive, and `cl`
      # next to `CL` must not stand in for it.
      hit <- c(targets[targets == name], targets[toupper(targets) == name])
      if (length(hit) == 0) {
        parse_error(
          file,
          record_line(record),
          NA_integer_,
          seen = as.vector(record),
          expected = sprintf(
            "%s for ADVAN%d TRANS%d",
            name,
            form$advan,
            form$trans
          ),
          call = NULL
        )
      }
      hit[[1]]
    },
    character(1)
  )
  list(
    type = "closed",
    ncmt = sum(c("v", "vp", "vp2") %in% names(found)),
    depot = "ka" %in% names(found),
    parameters = found,
    advan = form$advan,
    trans = form$trans
  )
}

closed_compartments <- function(structure) {
  peripheral <- if (structure$ncmt > 1) {
    paste0("peripheral", seq_len(structure$ncmt - 1))
  }
  c(if (isTRUE(structure$depot)) "depot", "central", peripheral)
}

# The ADVAN of a closed form with `ncmt` compartments, with or without a
# depot.
closed_advan <- function(ncmt, depot) {
  if (depot) c(2L, 4L, 12L)[[ncmt]] else c(1L, 3L, 11L)[[ncmt]]
}

# A closed form's ADVAN names its parameters (CL, V2, ...). `spelled` holds
# the target's spelling of `vars`; a variable holding one of these names for
# another purpose (rxode2's q2 is ADVAN11's Q3) gets a `_` suffix, and
# closed_alias_lines() assigns the name.
closed_free_names <- function(vars, spelled, structure) {
  if (!identical(structure$type, "closed")) {
    return(spelled)
  }
  required <- closed_forms[[as.character(structure$advan)]]$names
  owner <- unname(structure$parameters[names(required)])
  taken <- (spelled %in% required) & (vars != owner[match(spelled, required)])
  spelled[taken] <- paste0(spelled[taken], "_")
  spelled
}

# `NAME = var` lines, in `format`, for each parameter name the ADVAN needs
# that the model spells differently (a variable, or a parameter such as
# THETA(2)).
closed_alias_lines <- function(structure, spelling, format) {
  if (!identical(structure$type, "closed")) {
    return(character())
  }
  required <- closed_forms[[as.character(structure$advan)]]$names
  out <- character()
  for (canonical in names(required)) {
    spelled <- spelling[[structure$parameters[[canonical]]]]
    if (spelled != required[[canonical]]) {
      out <- c(out, sprintf(format, required[[canonical]], spelled))
    }
  }
  out
}
