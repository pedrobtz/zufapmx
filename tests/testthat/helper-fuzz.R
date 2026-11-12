# The fuzz targets of design §9, one per parser. tools/fuzz/*.R runs them
# under zufuzz (a coverage-guided campaign), and test-fuzz.R replays the
# corpus and seeded mutations of it, so the oracle is the same in both.
#
# The oracle: a reader either reads the input or refuses it with a
# zupmxio_parse_error. A model it reads writes to every format or is
# refused with a zupmxio_write_error, and round-trips through JSON. Any
# other error is a finding. Only exported functions are used, so the
# harnesses run against the installed package.

fuzz_target <- function(data, format) {
  text <- fuzz_text(data)
  model <- switch(
    format,
    nonmem = fuzz_read_file(text, ".ctl", zupmxio::pmx_read_nonmem),
    mrgsolve = fuzz_read_file(text, ".cpp", zupmxio::pmx_read_mrgsolve),
    rxode2 = fuzz_read_rxode2(text)
  )
  if (is.null(model)) {
    return(invisible(NULL))
  }
  writers <- list(
    nonmem = zupmxio::pmx_write_nonmem,
    rxode2 = zupmxio::pmx_write_rxode2,
    mrgsolve = zupmxio::pmx_write_mrgsolve
  )
  for (writer in writers) {
    written <- tryCatch(writer(model), zupmxio_write_error = function(e) NULL)
    if (!is.null(written)) {
      stopifnot(
        zupmxio::pmx_fidelity(written) %in% c("exact", "equivalent", "lossy")
      )
    }
  }
  if (requireNamespace("zujson", quietly = TRUE)) {
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path))
    zupmxio::pmx_write_json(model, path)
    stopifnot(identical(zupmxio::pmx_read_json(path), model))
  }
  invisible(model)
}

# Bytes as text, as a file read with readLines() would hold them: NUL
# dropped, CR LF as LF, and invalid UTF-8 replaced. R's parse(text = )
# refuses a CR that readLines() and source() would have dropped.
fuzz_text <- function(data) {
  data <- data[data != as.raw(0)]
  text <- gsub("\r\n", "\n", rawToChar(data), fixed = TRUE)
  Encoding(text) <- "UTF-8"
  if (!validUTF8(text)) {
    text <- iconv(text, "UTF-8", "UTF-8", sub = "?")
  }
  text
}

fuzz_read_file <- function(text, ext, reader) {
  path <- tempfile(fileext = ext)
  on.exit(unlink(path))
  writeLines(text, path, useBytes = TRUE)
  tryCatch(reader(path), zupmxio_parse_error = function(e) NULL)
}

# An rxode2 model is R code. Only a top-level `function` expression is
# evaluated, which builds a function without running its body; nothing
# else in the input is ever run.
fuzz_read_rxode2 <- function(text) {
  exprs <- tryCatch(
    parse(text = text, keep.source = FALSE),
    error = function(e) NULL
  )
  if (
    length(exprs) != 1 ||
      !is.call(exprs[[1]]) ||
      !identical(exprs[[1]][[1]], as.name("function"))
  ) {
    return(NULL)
  }
  fun <- eval(exprs[[1]], baseenv())
  tryCatch(zupmxio::pmx_read_rxode2(fun), zupmxio_parse_error = function(e) {
    NULL
  })
}

# Seeded mutations of a seed input, for the replay test: byte flips,
# deleted, duplicated and swapped lines, truncation, and a token from the
# format's vocabulary spliced in.
fuzz_mutations <- function(seed, n, vocabulary) {
  lines <- strsplit(rawToChar(seed), "\n", fixed = TRUE)[[1]]
  lapply(seq_len(n), function(i) {
    out <- lines
    kind <- sample(6, 1)
    k <- sample(length(out), 1)
    if (kind == 1) {
      bytes <- charToRaw(paste(out, collapse = "\n"))
      at <- sample(length(bytes), min(3, length(bytes)))
      bytes[at] <- as.raw(sample(
        c(32:126, 0, 10, 200:255),
        length(at),
        replace = TRUE
      ))
      return(bytes)
    }
    if (kind == 2) {
      out <- out[-k]
    } else if (kind == 3) {
      out <- append(out, out[[k]], k)
    } else if (kind == 4) {
      j <- sample(length(out), 1)
      out[c(k, j)] <- out[c(j, k)]
    } else if (kind == 5) {
      text <- paste(out, collapse = "\n")
      return(charToRaw(substr(text, 1, sample(nchar(text), 1))))
    } else {
      words <- strsplit(out[[k]], "(?<=\\W)|(?=\\W)", perl = TRUE)[[1]]
      if (length(words) == 0) {
        words <- ""
      }
      words[[sample(length(words), 1)]] <- sample(vocabulary, 1)
      out[[k]] <- paste(words, collapse = "")
    }
    charToRaw(paste(out, collapse = "\n"))
  })
}

fuzz_vocabulary <- list(
  nonmem = c(
    "$PK",
    "$DES",
    "$ERROR",
    "$THETA",
    "$OMEGA",
    "$SIGMA",
    "$MODEL",
    "IF",
    "THEN",
    "ELSE",
    "ENDIF",
    ".GT.",
    ".AND.",
    "EXP",
    "LOG",
    "**",
    "THETA(9)",
    "ETA(3)",
    "EPS(2)",
    "A(1)",
    "DADT(2)",
    "ALAG1",
    "F1",
    "R1",
    "D1",
    "S2",
    "TIME",
    "T",
    "BLOCK(2)",
    "FIX",
    "(0, 1, 2)",
    "-1E-3",
    ";",
    "("
  ),
  mrgsolve = c(
    "$PARAM",
    "$MAIN",
    "$ODE",
    "$TABLE",
    "$CMT",
    "$PKMODEL",
    "$INPUT",
    "$OMEGA @block",
    "double",
    "if",
    "else",
    "{",
    "}",
    "pow",
    "exp",
    "ETA(3)",
    "EPS(2)",
    "dxdt_CENT",
    "F_DEPOT",
    "ALAG_DEPOT",
    "D_CENT",
    "SOLVERTIME",
    "TIME",
    "1/2",
    "1.0e-3",
    ";",
    "//",
    "/*",
    "@annotated"
  ),
  rxode2 = c(
    "ini",
    "model",
    "d/dt(",
    "linCmt()",
    "~",
    "<-",
    "=",
    "exp",
    "log",
    "add(",
    "prop(",
    "combined2()",
    "alag(",
    "f(",
    "if",
    "else",
    "{",
    "}",
    "c(0, 1)",
    "fix(",
    "eta.x",
    "t",
    "^",
    "-1e-3",
    ")"
  )
)

# The seeds of a format: the corpus, and for NONMEM the estimate fixtures.
fuzz_seeds <- function(format) {
  files <- c(
    nonmem = "model.ctl",
    mrgsolve = "model.cpp",
    rxode2 = "model-rxode2.R"
  )
  paths <- file.path(zupmxio_corpus()$path, files[[format]])
  if (format == "nonmem") {
    paths <- c(
      paths,
      test_path("fixtures", "nonmem", c("run1.ctl", "run2.ctl"))
    )
  }
  lapply(paths, function(p) readBin(p, "raw", file.size(p)))
}

expect_fuzz_ok <- function(data, format, label) {
  err <- tryCatch(
    {
      fuzz_target(data, format)
      NULL
    },
    error = function(e) e
  )
  expect(
    is.null(err),
    sprintf(
      "%s: %s escaped the %s target: %s\nInput:\n%s",
      label,
      class(err)[[1]],
      format,
      if (is.null(err)) "" else conditionMessage(err),
      fuzz_text(data)
    )
  )
}
