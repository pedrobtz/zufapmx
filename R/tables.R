#' Read NONMEM table output
#'
#' Reads the files a NONMEM `$TABLE` record writes, to compare a
#' translation against the run's own predictions with
#' `pmx_compare(tables, model)`. The comparison simulates the model on the
#' table's records, so the table needs the dosing records as well as the
#' observations.
#'
#' A table is NONMEM's layout: an optional `TABLE NO. n` title line, a line
#' of column names, then one record per line, separated by spaces or
#' commas. Several files from one run (say `sdtab1` and `patab1`) are read
#' side by side: they must have the same records, and a column in more than
#' one file must hold the same values in each.
#'
#' The records need `ID`, `TIME` and the prediction column. `AMT`, `EVID`,
#' `MDV`, `CMT`, `RATE`, `II`, `ADDL` and `SS` are read when present, with
#' NONMEM's defaults when not. Without `EVID`, a record with a positive
#' `AMT` is a dose and any other record with `MDV` 0 (or no `MDV`) is an
#' observation. Records that are neither are dropped, and `EVID` 3 and 4
#' (resets) are refused. Every other column is kept, so the model's
#' covariates can come from the table. Column names match without regard
#' to case.
#'
#' The prediction column should be `PRED`, NONMEM's population prediction:
#' the comparison simulates the model's typical subject, every random
#' effect at zero (design Q1). NONMEM prints a fixed number of significant
#' digits, 5 by default; the reader records it, and the comparison accepts
#' a prediction that rounds to the table's value.
#'
#' Tables are read with the zucsv package when it is installed, and with
#' base R otherwise; both give the same result.
#'
#' @param file The path of a table file, or several from one run.
#' @param prediction The name of the prediction column.
#' @return A `zufapmx_tables` object: a list with `records`, a data frame
#'   with columns `id`, `time`, `evid`, `amt`, `cmt`, `rate`, `ii`, `addl`,
#'   `ss` and `pred` followed by the table's other columns under their own
#'   names; `prediction`, the column read as `pred`; `digits`, the
#'   significant digits of the prediction column; and `files`.
#' @family readers
#' @seealso [pmx_compare()] compares a model with the tables.
#' @export
#' @examples
#' file <- tempfile(fileext = ".tab")
#' writeLines(c(
#'   "TABLE NO.  1",
#'   " ID          TIME        AMT         EVID        PRED",
#'   "  1.0000E+00  0.0000E+00  1.0000E+02  1.0000E+00  0.0000E+00",
#'   "  1.0000E+00  1.0000E+00  0.0000E+00  0.0000E+00  1.8187E+00"
#' ), file)
#' pmx_read_tables(file)
pmx_read_tables <- function(file, prediction = "PRED") {
  if (
    !is.character(file) ||
      length(file) == 0 ||
      anyNA(file) ||
      !all(file.exists(file)) ||
      any(dir.exists(file))
  ) {
    zufapmx_abort(
      "`file` must be the paths of existing table files.",
      "model"
    )
  }
  if (
    !is.character(prediction) ||
      length(prediction) != 1 ||
      is.na(prediction) ||
      !nzchar(prediction)
  ) {
    zufapmx_abort("`prediction` must be a column name.", "model")
  }

  tables <- lapply(file, table_read_one, prediction = prediction)
  columns <- tables[[1]]$data
  for (k in seq_along(tables)[-1]) {
    columns <- table_join(columns, tables[[k]]$data, file[[1]], file[[k]])
  }
  digits <- unlist(lapply(tables, `[[`, "digits"))
  if (length(digits) == 0) {
    parse_error(
      file[[1]],
      NA_integer_,
      NA_integer_,
      seen = "no prediction column",
      message = sprintf("no column `%s` in the tables", prediction),
      call = NULL
    )
  }
  structure(
    list(
      records = table_records(columns, prediction, file[[1]]),
      prediction = prediction,
      digits = min(digits),
      files = normalizePath(file)
    ),
    class = "zufapmx_tables"
  )
}

# One file: its columns as numbers, and the significant digits of the
# prediction column when the file has it.
table_read_one <- function(file, prediction) {
  lines <- readLines(file, warn = FALSE)
  at <- seq_along(lines)
  keep <- nzchar(trimws(lines))
  titles <- which(grepl("^\\s*TABLE NO\\.", lines))
  if (length(titles) > 1) {
    parse_error(
      file,
      titles[[2]],
      1L,
      seen = trimws(lines[[titles[[2]]]]),
      message = sprintf(
        paste(
          "a second table at line %d: one table per file",
          "(NONMEM repeats it for each subproblem)"
        ),
        titles[[2]]
      ),
      call = NULL
    )
  }
  keep[titles] <- FALSE
  lines <- lines[keep]
  at <- at[keep]
  if (length(lines) == 0) {
    parse_error(
      file,
      1L,
      1L,
      seen = "an empty file",
      expected = "a line of column names",
      call = NULL
    )
  }
  fields <- function(x) {
    gsub("[[:space:],]+", ",", gsub("^[[:space:],]+|[[:space:],]+$", "", x))
  }
  header <- strsplit(fields(lines[[1]]), ",", fixed = TRUE)[[1]]
  if (
    length(header) == 0 ||
      any(!grepl("^[A-Za-z_][A-Za-z0-9_.]*$", header))
  ) {
    parse_error(
      file,
      at[[1]],
      1L,
      seen = trimws(lines[[1]]),
      message = sprintf(
        paste(
          "line %d is not a line of column names (a table written with",
          "NOHEADER cannot be read)"
        ),
        at[[1]]
      ),
      call = NULL
    )
  }
  if (anyDuplicated(toupper(header))) {
    parse_error(
      file,
      at[[1]],
      1L,
      seen = header[duplicated(toupper(header))][[1]],
      message = "a column named twice",
      call = NULL
    )
  }
  body <- fields(lines[-1])
  width <- lengths(regmatches(body, gregexpr(",", body, fixed = TRUE))) + 1L
  bad <- which(width != length(header))
  if (length(bad) > 0) {
    parse_error(
      file,
      at[-1][[bad[[1]]]],
      1L,
      seen = sprintf("%d values", width[[bad[[1]]]]),
      expected = sprintf("%d values", length(header)),
      call = NULL
    )
  }
  pred <- match(toupper(prediction), toupper(header))
  types <- rep("double", length(header))
  types[pred] <- "character"
  data <- table_parse(c(paste(header, collapse = ","), body), types, file)
  names(data) <- header
  digits <- NULL
  if (!is.na(pred)) {
    text <- data[[pred]]
    data[[pred]] <- suppressWarnings(as.numeric(text))
    # The parsers read "NA" and "NaN" as missing already.
    bad <- which(
      is.na(data[[pred]]) & !is.na(text) & !text %in% c("NA", "NaN")
    )
    if (length(bad) > 0) {
      parse_error(
        file,
        at[-1][[bad[[1]]]],
        NA_integer_,
        seen = text[[bad[[1]]]],
        expected = "a number",
        call = NULL
      )
    }
    digits <- table_digits(text)
  }
  # Only the prediction may be missing; NONMEM writes no NA otherwise.
  for (k in setdiff(seq_along(data), pred)) {
    if (anyNA(data[[k]])) {
      row <- which(is.na(data[[k]]))[[1]]
      parse_error(
        file,
        at[-1][[row]],
        NA_integer_,
        seen = "a missing value",
        expected = sprintf("a number in column `%s`", header[[k]]),
        call = NULL
      )
    }
  }
  list(data = data, digits = digits)
}

# The cleaned text as a data frame: zucsv when installed, base R otherwise.
table_parse <- function(text, types, file) {
  fail <- function(e) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = "the table",
      message = sprintf("not a table of numbers (%s)", conditionMessage(e)),
      call = NULL
    )
  }
  if (engine_available("zucsv")) {
    out <- tryCatch(
      zucsv::read_csv(text = text, col_types = types, na = c("NA", "NaN")),
      error = fail
    )
  } else {
    out <- tryCatch(
      utils::read.csv(
        text = text,
        colClasses = unname(
          c(double = "numeric", character = "character")[types]
        ),
        na.strings = c("NA", "NaN"),
        check.names = FALSE,
        strip.white = FALSE
      ),
      error = fail,
      warning = fail
    )
  }
  as.list(out)
}

# The significant digits a column is printed with: the fewest among its
# non-zero values, from their text (`1.2345E+00` has 5).
table_digits <- function(text) {
  mantissa <- toupper(sub("[eEdD].*$", "", trimws(text)))
  mantissa <- gsub("[^0-9]", "", mantissa)
  mantissa <- sub("^0+", "", mantissa)
  mantissa <- mantissa[nzchar(mantissa)]
  if (length(mantissa) == 0) {
    return(NA_integer_)
  }
  # Trailing zeros after the point count: 1.2000E+00 is 5 digits.
  min(nchar(mantissa))
}

table_join <- function(a, b, file_a, file_b) {
  if (length(a[[1]]) != length(b[[1]])) {
    parse_error(
      file_b,
      NA_integer_,
      NA_integer_,
      seen = sprintf("%d records", length(b[[1]])),
      expected = sprintf("%d records, as in `%s`", length(a[[1]]), file_a),
      call = NULL
    )
  }
  for (name in names(b)) {
    same <- match(toupper(name), toupper(names(a)))
    if (is.na(same)) {
      a[[name]] <- b[[name]]
    } else if (!identical(a[[same]], b[[name]])) {
      parse_error(
        file_b,
        NA_integer_,
        NA_integer_,
        seen = name,
        message = sprintf(
          "column `%s` differs from the same column in `%s`",
          name,
          file_a
        ),
        call = NULL
      )
    }
  }
  a
}

table_standard <- c(
  "ID",
  "TIME",
  "AMT",
  "EVID",
  "MDV",
  "CMT",
  "RATE",
  "II",
  "ADDL",
  "SS"
)

# The records as the engines take them: the standard columns in lower case,
# NONMEM's defaults where a column is absent, and the rest under their own
# names.
table_records <- function(columns, prediction, file) {
  upper <- toupper(names(columns))
  col <- function(name, default) {
    i <- match(name, upper)
    if (is.na(i)) rep(default, length(columns[[1]])) else columns[[i]]
  }
  for (needed in c("ID", "TIME")) {
    if (!needed %in% upper) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = "no column",
        message = sprintf("no column `%s` in the tables", needed),
        call = NULL
      )
    }
  }
  amt <- col("AMT", 0)
  evid <- if ("EVID" %in% upper) {
    col("EVID", 0)
  } else {
    ifelse(amt > 0, 1, ifelse(col("MDV", 0) == 0, 0, 2))
  }
  if (any(evid %in% c(3, 4))) {
    row <- which(evid %in% c(3, 4))[[1]]
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = sprintf("EVID %d in record %d", evid[[row]], row),
      message = "not compared: a reset record (EVID 3 or 4)",
      call = NULL
    )
  }
  bad <- !evid %in% c(0, 1, 2)
  if (any(bad)) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = sprintf("EVID %s", format(evid[bad][[1]])),
      expected = "EVID 0, 1 or 2",
      call = NULL
    )
  }
  id <- col("ID", NA)
  time <- col("TIME", NA)
  # Each subject's records together and in time, as NONMEM reads them.
  starts <- c(TRUE, id[-1] != id[-length(id)])
  if (anyDuplicated(id[starts])) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = sprintf("ID %s", format(id[starts][duplicated(id[starts])][[1]])),
      message = "a subject whose records are not together",
      call = NULL
    )
  }
  back <- which(!starts & c(FALSE, diff(time) < 0))
  if (length(back) > 0) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = sprintf(
        "TIME %s in record %d",
        format(time[[back[[1]]]]),
        back[[1]]
      ),
      message = "time going back within a subject",
      call = NULL
    )
  }
  standard <- data.frame(
    id = id,
    time = time,
    evid = as.integer(evid),
    amt = amt,
    cmt = col("CMT", 1),
    rate = col("RATE", 0),
    ii = col("II", 0),
    addl = col("ADDL", 0),
    ss = col("SS", 0),
    pred = columns[[match(toupper(prediction), upper)]]
  )
  # A column whose name clashes with a standard one (PRED beside an IPRED
  # prediction) is left out rather than overwrite it.
  other <- columns[
    !upper %in% c(table_standard, "PRED", toupper(prediction))
  ]
  out <- standard
  for (name in names(other)) {
    out[[name]] <- other[[name]]
  }
  out <- out[out$evid != 2, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @export
print.zufapmx_tables <- function(x, ...) {
  r <- x$records
  cat(sprintf(
    "<zufapmx tables> %d subject%s, %d dose and %d observation records\n",
    length(unique(r$id)),
    if (length(unique(r$id)) == 1) "" else "s",
    sum(r$evid == 1),
    sum(r$evid == 0)
  ))
  cat(sprintf(
    "Prediction: `%s`, %d significant digits.\n",
    x$prediction,
    x$digits
  ))
  cat(sprintf("From %s.\n", paste(basename(x$files), collapse = ", ")))
  invisible(x)
}
