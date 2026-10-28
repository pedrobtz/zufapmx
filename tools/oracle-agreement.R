# Run the NONMEM-to-R converters on every corpus entry, simulate the typical
# subject in each target engine, and compare with expected.csv. Writes
# .agents/oracle-agreement.md. Run from the package root:
#
#   Rscript tools/oracle-agreement.R
#
# A converter that is not installed or that fails on an entry is recorded,
# never fatal. nonmem2mrgsolve is a development-only cross-check (design
# D12): it is run when installed and is not a package dependency.

pkgload::load_all(quiet = TRUE)
source("tests/testthat/helper-corpus.R")

tol <- 1e-6

oracles <- list(
  nonmem2rx = list(pkg = "nonmem2rx", run = corpus_oracle_nonmem2rx),
  amp.sim = list(pkg = "amp.sim", run = corpus_oracle_amp_sim),
  nonmem2mrgsolve = list(
    pkg = "nonmem2mrgsolve",
    run = function(path, regimen) {
      stop("no runner written: not installed when this script was written")
    }
  ),
  `model.cpp` = list(
    pkg = "mrgsolve",
    run = function(path, regimen) {
      corpus_solve_mrgsolve(file.path(path, "model.cpp"), regimen)
    }
  )
)

# Distinct error messages, numbered in the order they occur. A cell shows
# only the number, so the table stays narrow.
errors <- character()

cell <- function(oracle, entry, expected) {
  if (!requireNamespace(oracle$pkg, quietly = TRUE)) {
    return("absent")
  }
  pred <- tryCatch(
    oracle$run(entry$path, entry$regimen),
    error = function(e) e
  )
  if (inherits(pred, "error")) {
    msg <- trimws(gsub("[[:space:]|]+", " ", conditionMessage(pred)))
    if (!msg %in% errors) {
      errors <<- c(errors, msg)
    }
    return(sprintf("error [%d]", match(msg, errors)))
  }
  if (length(pred) != length(expected) || anyNA(pred)) {
    return(sprintf(
      "differs: %d predictions for %d times",
      length(pred),
      length(expected)
    ))
  }
  dev <- corpus_max_rel_dev(pred, expected)
  sprintf("%s %.0e", if (dev <= tol) "agrees" else "differs", dev)
}

corpus <- zupkpmx_corpus()
rows <- lapply(seq_len(nrow(corpus)), function(i) {
  entry <- corpus[i, ]
  expected <- corpus_expected(entry$path)$pred
  vapply(oracles, cell, character(1), entry = entry, expected = expected)
})

version_of <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    as.character(utils::packageVersion(pkg))
  } else {
    "not installed"
  }
}
pkgs <- c("rxode2", "mrgsolve", "nonmem2rx", "amp.sim", "nonmem2mrgsolve")

lines <- c(
  "# Oracle agreement",
  "",
  sprintf("Generated %s by `tools/oracle-agreement.R`.", format(Sys.Date())),
  "",
  "Each cell compares the typical-subject predictions of one translation",
  "of the entry's `model.ctl` with `expected.csv` (rxode2 on",
  "`model-rxode2.R`). The last column simulates the entry's own `model.cpp`.",
  sprintf(
    "A cell agrees when the largest relative deviation is at most %g;",
    tol
  ),
  "the number after it is that deviation. `absent`: the converter is",
  "not installed.",
  "",
  "Versions:",
  "",
  paste0("- ", pkgs, " ", vapply(pkgs, version_of, character(1))),
  "",
  paste0("| Entry | ", paste(names(oracles), collapse = " | "), " |"),
  paste0("| --- |", strrep(" --- |", length(oracles))),
  vapply(
    seq_along(rows),
    function(i) {
      paste0(
        "| ",
        corpus$id[[i]],
        " | ",
        paste(rows[[i]], collapse = " | "),
        " |"
      )
    },
    character(1)
  )
)
if (length(errors) > 0) {
  lines <- c(
    lines,
    "",
    "Errors:",
    "",
    sprintf("%d. %s", seq_along(errors), strtrim(errors, 300))
  )
}
# Hand-written findings below the marker survive regeneration.
out <- ".agents/oracle-agreement.md"
marker <- "<!-- notes: kept when this file is regenerated -->"
notes <- marker
if (file.exists(out)) {
  old <- readLines(out)
  at <- match(marker, old)
  if (!is.na(at)) notes <- old[at:length(old)]
}
writeLines(c(lines, "", notes), out)
message("Wrote ", out)
