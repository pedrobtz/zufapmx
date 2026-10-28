# Final estimates from NONMEM's output files (design §6): run.xml through
# zuxml, the .ext file, or the .lst report. Each reader returns
# list(theta, omega, sigma): a numeric vector and two full matrices.

# A seam for tests: zuxml is optional, and run.xml falls back to .ext.
has_zuxml <- function() requireNamespace("zuxml", quietly = TRUE)

nm_attach_estimates <- function(model, file, source) {
  stem <- tools::file_path_sans_ext(file)
  paths <- c(
    xml = paste0(stem, ".xml"),
    ext = paste0(stem, ".ext"),
    lst = paste0(stem, ".lst")
  )

  if (source == "auto") {
    source <- if (file.exists(paths[["xml"]]) && has_zuxml()) {
      "xml"
    } else if (file.exists(paths[["ext"]])) {
      "ext"
    } else {
      "none"
    }
  }
  if (source == "none") {
    return(model)
  }
  path <- paths[[source]]
  if (!file.exists(path)) {
    zupkpmx_abort(sprintf("No %s file at `%s`.", source, path), "model")
  }
  if (source == "xml" && !has_zuxml()) {
    engine_error("zuxml", call = NULL)
  }
  est <- switch(
    source,
    xml = nm_estimates_xml(path),
    ext = nm_estimates_ext(path),
    lst = nm_estimates_lst(path)
  )

  m <- unclass(model)
  n <- nrow(m$parameters)
  if (length(est$theta) != n) {
    parse_error(
      path,
      NA_integer_,
      NA_integer_,
      seen = sprintf("%d thetas", length(est$theta)),
      expected = sprintf("%d thetas", n),
      call = NULL
    )
  }
  m$parameters$estimate <- unname(est$theta)
  m$random_effects$omega <- nm_block_estimates(
    m$random_effects$omega,
    est$omega,
    path
  )
  m$random_effects$sigma <- nm_block_estimates(
    m$random_effects$sigma,
    est$sigma,
    path
  )
  m$provenance$estimates <- normalizePath(path)
  do.call(new_model, m)
}

nm_block_estimates <- function(blocks, full, path) {
  total <- sum(vapply(blocks, function(b) length(b$names), integer(1)))
  if (!identical(dim(full), c(total, total))) {
    parse_error(
      path,
      NA_integer_,
      NA_integer_,
      seen = sprintf("a %d by %d matrix", NROW(full), NCOL(full)),
      expected = sprintf("%d by %d", total, total),
      call = NULL
    )
  }
  at <- 0
  lapply(blocks, function(b) {
    i <- at + seq_along(b$names)
    at <<- at + length(b$names)
    b$estimate <- matrix(
      full[i, i],
      length(i),
      length(i),
      dimnames = list(b$names, b$names)
    )
    b
  })
}

nm_estimates_ext <- function(path) {
  lines <- readLines(path, warn = FALSE)
  tables <- grep("^TABLE NO\\.", lines)
  if (length(tables) == 0) {
    parse_error(
      path,
      1L,
      1L,
      seen = if (length(lines) > 0) lines[[1]] else "an empty file",
      expected = "TABLE NO.",
      call = NULL
    )
  }
  last <- tables[[length(tables)]]
  if (last + 1 > length(lines)) {
    parse_error(
      path,
      last,
      1L,
      seen = "end of file",
      expected = "a header and rows after TABLE NO.",
      call = NULL
    )
  }
  body <- lines[(last + 1):length(lines)]
  header <- strsplit(trimws(body[[1]]), "\\s+")[[1]]
  rows <- lapply(strsplit(trimws(body[-1]), "\\s+"), as.numeric)
  final <- Filter(
    function(r) length(r) == length(header) && r[[1]] == -1000000000,
    rows
  )
  if (length(final) == 0) {
    parse_error(
      path,
      last,
      1L,
      seen = "no final row",
      expected = "ITERATION -1000000000",
      call = NULL
    )
  }
  values <- stats::setNames(final[[1]], header)
  list(
    theta = values[grep("^THETA[0-9]+$", header)],
    omega = lower_to_full(values[grep("^OMEGA\\(", header)]),
    sigma = lower_to_full(values[grep("^SIGMA\\(", header)])
  )
}

nm_estimates_xml <- function(path) {
  # NONMEM declares encoding="ASCII", which expat does not know by that
  # name. ISO-8859-1 reads every byte, and ASCII is a subset of it.
  doc <- zuxml::xml_read(path, encoding = "ISO-8859-1")
  ns <- "http://namespaces.oup.com/nonmem"
  est <- zuxml::xml_find(zuxml::xml_root(doc), "estimation", ns = ns)
  if (length(est) == 0) {
    parse_error(
      path,
      NA_integer_,
      NA_integer_,
      seen = "no <nm:estimation>",
      expected = "<nm:estimation>",
      call = NULL
    )
  }
  last <- est[[length(est)]]
  theta <- zuxml::xml_find(
    zuxml::xml_find(last, "theta", ns = ns),
    "val",
    ns = ns
  )
  matrix_of <- function(name) {
    node <- zuxml::xml_find(last, name, ns = ns)
    cols <- zuxml::xml_find(node, "col", ns = ns)
    lower_to_full(as.numeric(zuxml::xml_text(cols)))
  }
  list(
    theta = as.numeric(zuxml::xml_text(theta)),
    omega = matrix_of("omega"),
    sigma = matrix_of("sigma")
  )
}

nm_estimates_lst <- function(path) {
  lines <- readLines(path, warn = FALSE)
  start <- grep("FINAL PARAMETER ESTIMATE", lines)
  if (length(start) == 0) {
    parse_error(
      path,
      NA_integer_,
      NA_integer_,
      seen = "no final estimates",
      expected = "FINAL PARAMETER ESTIMATE",
      call = NULL
    )
  }
  lines <- lines[start[[length(start)]]:length(lines)]
  section <- function(title, next_titles) {
    from <- grep(title, lines)[1]
    if (is.na(from)) {
      return(character())
    }
    to <- grep(paste(next_titles, collapse = "|"), lines)
    to <- to[to > from]
    lines[(from + 1):(if (length(to) > 0) to[[1]] - 1 else length(lines))]
  }
  numbers <- function(x) {
    x <- gsub("\\.{3,}", " 0 ", x)
    tokens <- unlist(strsplit(trimws(x), "\\s+"))
    as.numeric(tokens[grepl("^[-+]?[0-9.]+([EeDd][-+]?[0-9]+)?$", tokens)])
  }
  theta_lines <- section("THETA - VECTOR", c("OMEGA - ", "SIGMA - "))
  theta_lines <- theta_lines[!grepl("^\\s*TH", theta_lines)]
  omega_lines <- section("OMEGA - ", c("SIGMA - ", "\\*{20}"))
  sigma_lines <- section("SIGMA - ", c("\\*{20}", "OMEGA - "))
  list(
    theta = numbers(theta_lines),
    omega = lower_to_full(numbers(grep("^\\+", omega_lines, value = TRUE))),
    sigma = lower_to_full(numbers(grep("^\\+", sigma_lines, value = TRUE)))
  )
}
