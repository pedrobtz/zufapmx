#' Write a comparison as a Markdown report
#'
#' Writes what a reviewer needs to judge a translation from the report
#' alone: the verdict, the engines and their versions, the design with its
#' seed, the deviations per regimen, the worst point, every point that
#' disagrees, and the text of both models as the engines ran them.
#'
#' @param comparison A comparison from [pmx_compare()].
#' @param file The path to write, or `NULL` to only return the text.
#' @param max_points The most disagreeing points to list, from 0. The
#'   report says how many more there were.
#' @return The report's lines, invisibly.
#' @family comparison
#' @export
#' @examplesIf requireNamespace("rxode2", quietly = TRUE)
#' path <- system.file(
#'   "corpus", "onecmt-oral", "model.ctl",
#'   package = "zupmxio"
#' )
#' model <- pmx_read_nonmem(path)
#' cmp <- pmx_compare(
#'   model,
#'   pmx_write_nonmem(model),
#'   pmx_design(n = 2, regimens = "single")
#' )
#' file <- tempfile(fileext = ".md")
#' pmx_report(cmp, file)
#' cat(readLines(file, n = 12), sep = "\n")
pmx_report <- function(comparison, file = NULL, max_points = 20) {
  if (!inherits(comparison, "zupmxio_comparison")) {
    zupmxio_abort(
      sprintf(
        "`comparison` must be a comparison from pmx_compare(), not %s.",
        class(comparison)[[1]]
      ),
      "model"
    )
  }
  check_path_or_null(file)
  check_count(max_points, 0, .Machine$integer.max)

  x <- comparison
  d <- x$design
  lines <- c(
    "# Model comparison",
    "",
    sprintf(
      "**Result: %s.** %s",
      if (x$pass) "pass" else "FAIL",
      if (!is.null(x$reason)) {
        x$reason
      } else if (x$pass) {
        "Every prediction agrees within the tolerances."
      } else {
        sprintf(
          "%d of %d predictions disagree.",
          sum(!x$predictions$pass),
          nrow(x$predictions)
        )
      }
    ),
    "",
    "## Models",
    "",
    md_table(data.frame(
      Side = c("a", "b"),
      Simulated = vapply(x$sides, `[[`, character(1), "origin"),
      Engine = ifelse(
        x$engines$engine == "table",
        "none: NONMEM's own output",
        paste(x$engines$engine, x$engines$version)
      ),
      Source = vapply(x$sides, report_source, character(1))
    )),
    "",
    "## Design",
    "",
    report_design(x),
    sprintf(
      paste(
        "A point agrees when its relative deviation is at most %s%s its",
        "absolute deviation at most %s%s."
      ),
      md_num(x$tol, 15),
      if (is.null(d)) "," else " or",
      md_num(x$atol, 15),
      if (is.null(d)) {
        ", or the model's prediction rounds to the table's value"
      } else {
        ""
      }
    ),
    "",
    report_results(x, max_points),
    "## Model a",
    "",
    report_text(x$sides$a),
    "",
    "## Model b",
    "",
    report_text(x$sides$b)
  )
  if (!is.null(file)) {
    writeLines(lines, file)
  }
  invisible(lines)
}

report_results <- function(x, max_points) {
  if (!is.null(x$reason)) {
    return(character())
  }
  r <- x$regimens
  out <- c(
    "## Results",
    "",
    md_table(data.frame(
      Regimen = r$regimen,
      Points = r$points,
      Disagree = r$failed,
      `Largest absolute` = md_num(r$max_abs),
      `Largest relative` = md_num(r$max_rel),
      Skipped = ifelse(is.na(r$skipped), "", r$skipped),
      check.names = FALSE
    )),
    ""
  )
  if (!is.null(x$worst)) {
    w <- x$worst
    out <- c(
      out,
      sprintf(
        paste(
          "Worst point: %s, subject %s, time %s: %s (a) against %s (b),",
          "absolute deviation %s, relative %s."
        ),
        w$regimen,
        md_num(w$id, 15),
        md_num(w$time, 15),
        md_num(w$a, 10),
        md_num(w$b, 10),
        md_num(w$abs),
        md_num(w$rel)
      ),
      ""
    )
  }
  bad <- x$predictions[!x$predictions$pass, ]
  if (nrow(bad) > 0) {
    shown <- utils::head(bad, max_points)
    out <- c(
      out,
      "### Points that disagree",
      "",
      if (nrow(shown) > 0) {
        c(
          md_table(data.frame(
            Regimen = shown$regimen,
            Subject = shown$id,
            Time = md_num(shown$time, 15),
            a = md_num(shown$a, 10),
            b = md_num(shown$b, 10),
            Absolute = md_num(shown$abs),
            Relative = md_num(shown$rel)
          )),
          ""
        )
      },
      if (nrow(bad) > nrow(shown)) {
        c(sprintf("And %d more.", nrow(bad) - nrow(shown)), "")
      }
    )
  }
  out
}

report_design <- function(x) {
  d <- x$design
  if (is.null(d)) {
    p <- x$sides[[which(x$engines$engine == "table")]]$provenance
    return(sprintf(
      paste(
        "The tables' own records: %d subject%s and %d records. The model's",
        "typical subject, every random effect at zero, as NONMEM's `PRED`.",
        "Residual error is off."
      ),
      p$subjects,
      if (p$subjects == 1) "" else "s",
      p$records
    ))
  }
  c(
    sprintf(
      "Regimens %s, each a dose of %s into compartment %d.",
      paste(d$regimens, collapse = ", "),
      md_num(design_dose, 15),
      d$cmt
    ),
    sprintf(
      paste(
        "The typical subject (1) and %d subjects with random effects drawn",
        "with seed %s. Residual error is off."
      ),
      d$n,
      md_num(d$seed, 15)
    ),
    if (length(d$covariates) > 0) {
      sprintf(
        "Covariates: %s.",
        paste(
          names(d$covariates),
          md_num(unlist(d$covariates), 15),
          sep = " = ",
          collapse = ", "
        )
      )
    }
  )
}

report_source <- function(side) {
  p <- side$provenance
  file <- p$file
  if (!is.null(file) && !is.na(file)) {
    files <- strsplit(file, ", ", fixed = TRUE)[[1]]
    return(sprintf(
      "%s, %s",
      p$format,
      paste0("`", basename(files), "`", collapse = ", ")
    ))
  }
  if (!is.null(p$fidelity)) {
    return(sprintf("written %s, fidelity %s", p$format, p$fidelity))
  }
  p$format
}

report_text <- function(side) {
  if (identical(side$engine, "table")) {
    return(sprintf(
      "Predictions read from %s.",
      paste0(
        "`",
        basename(strsplit(side$provenance$file, ", ", fixed = TRUE)[[1]]),
        "`",
        collapse = ", "
      )
    ))
  }
  fence <- strrep("`", max(3, md_longest_backticks(side$text) + 1))
  language <- c(rxode2 = "r", mrgsolve = "cpp")[[side$engine]]
  c(paste0(fence, language), side$text, fence)
}

md_longest_backticks <- function(text) {
  runs <- unlist(regmatches(text, gregexpr("`+", text)))
  if (length(runs) == 0) 0 else max(nchar(runs))
}

# Numbers in `digits` significant digits, whatever the caller's digits,
# scipen and OutDec options: a report is shared, so it reads the same
# everywhere.
md_num <- function(x, digits = 3) {
  out <- formatC(x, digits = digits, format = "g", decimal.mark = ".")
  out[is.na(x)] <- ""
  trimws(out)
}

md_table <- function(df) {
  cells <- lapply(df, function(col) {
    gsub("|", "\\|", as.character(col), fixed = TRUE)
  })
  row <- function(v) paste0("| ", paste(v, collapse = " | "), " |")
  c(
    row(names(df)),
    row(rep("---", ncol(df))),
    vapply(
      seq_len(nrow(df)),
      function(i) row(vapply(cells, `[[`, character(1), i)),
      character(1)
    )
  )
}
