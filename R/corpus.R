#' The shipped model corpus
#'
#' Lists the models that ship with zupkpmx. Each entry is one model written
#' in every format that can hold it, with the typical-subject predictions
#' the formats must agree on. The corpus is the package's test suite and a
#' set of known-equivalent translations to compare against.
#'
#' @return A data frame with one row per entry, ordered by `id`:
#'   \describe{
#'     \item{id}{The entry's name, also its directory name.}
#'     \item{title}{A one-line description of the model.}
#'     \item{structure}{The NONMEM `ADVAN`/`TRANS` pair it corresponds to.}
#'     \item{regimen}{The dose route of its predictions: `"bolus"`,
#'       `"oral"` or `"infusion"`.}
#'     \item{formats}{The formats the entry is written in, comma-separated:
#'       `"nonmem"` (`model.ctl`), `"rxode2"` (`model-rxode2.R`) and
#'       `"mrgsolve"` (`model.cpp`).}
#'     \item{source}{Where the model comes from, and its licence.}
#'     \item{path}{The entry's directory.}
#'   }
#' @export
#' @examples
#' corpus <- zupkpmx_corpus()
#' corpus[, c("id", "structure", "formats")]
zupkpmx_corpus <- function() {
  corpus_read(system.file("corpus", package = "zupkpmx"))
}

corpus_fields <- c("Id", "Title", "Structure", "Regimen", "Formats", "Source")

corpus_files <- c(
  nonmem = "model.ctl",
  rxode2 = "model-rxode2.R",
  mrgsolve = "model.cpp"
)

corpus_read <- function(root) {
  dirs <- sort(list.dirs(root, recursive = FALSE))
  if (length(dirs) == 0) {
    zupkpmx_abort(sprintf("No corpus entries under `%s`.", root), "corpus")
  }
  do.call(rbind, c(lapply(dirs, corpus_read_entry), make.row.names = FALSE))
}

corpus_read_entry <- function(dir) {
  dcf <- read.dcf(file.path(dir, "entry.dcf"))
  missing <- setdiff(corpus_fields, colnames(dcf))
  if (length(missing) > 0) {
    zupkpmx_abort(
      sprintf(
        "Corpus entry `%s` has no %s field.",
        basename(dir),
        paste(missing, collapse = ", ")
      ),
      "corpus"
    )
  }

  formats <- strsplit(dcf[, "Formats"], ",", fixed = TRUE)[[1]]
  formats <- trimws(formats)
  unknown <- setdiff(formats, names(corpus_files))
  if (length(unknown) > 0) {
    zupkpmx_abort(
      sprintf(
        "Corpus entry `%s` lists unknown formats: %s.",
        basename(dir),
        paste(unknown, collapse = ", ")
      ),
      "corpus"
    )
  }
  absent <- formats[!file.exists(file.path(dir, corpus_files[formats]))]
  if (length(absent) > 0) {
    zupkpmx_abort(
      sprintf(
        "Corpus entry `%s` lists %s but has no %s.",
        basename(dir),
        paste(absent, collapse = ", "),
        paste(corpus_files[absent], collapse = ", ")
      ),
      "corpus"
    )
  }

  data.frame(
    id = dcf[, "Id"],
    title = gsub("\\s+", " ", dcf[, "Title"]),
    structure = dcf[, "Structure"],
    regimen = dcf[, "Regimen"],
    formats = paste(formats, collapse = ","),
    source = gsub("\\s+", " ", dcf[, "Source"]),
    path = normalizePath(dir)
  )
}

# The Stage 1 standard design: one 100-unit dose at time 0 by the entry's
# regimen, and typical-subject predictions at fixed times. pmx_design()
# grows from this.
corpus_design <- function(regimen) {
  regimens <- c("bolus", "oral", "infusion")
  if (!regimen %in% regimens) {
    zupkpmx_abort(sprintf("Unknown regimen `%s`.", regimen), "corpus")
  }
  times <- c(0.5, 1, 2, 4, 6, 8, 12, 24)
  data.frame(
    time = c(0, times),
    evid = c(1L, rep(0L, length(times))),
    amt = c(100, rep(0, length(times))),
    dur = c(if (regimen == "infusion") 1 else 0, rep(0, length(times))),
    cmt = c(
      if (regimen == "oral") "depot" else "central",
      rep(NA, length(times))
    )
  )
}
