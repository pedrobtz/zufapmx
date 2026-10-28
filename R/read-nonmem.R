#' Read a NONMEM control stream
#'
#' Reads a control stream into the package's representation, with final
#' estimates when NONMEM's output files are next to it.
#'
#' The reader covers `$PROBLEM`, `$INPUT`, `$DATA`, `$SUBROUTINES`,
#' `$MODEL`, `$PK`, `$DES`, `$ERROR`, `$THETA`, `$OMEGA`, `$SIGMA`,
#' `$ESTIMATION`, `$COVARIANCE` and `$TABLE`. Closed forms are `ADVAN1`
#' to `ADVAN4`, `ADVAN11` and `ADVAN12` with `TRANS2` or `TRANS4`; general
#' ODE models are `ADVAN6`, `ADVAN8`, `ADVAN9` and `ADVAN13`. Abbreviated
#' code may use assignments, `IF` in one line or in blocks, arithmetic,
#' comparisons, `EXP`, `LOG`, `LOG10`, `SQRT` and `ABS`.
#'
#' Verbatim Fortran, `$INFN`, `$PRED`, `$MIX`, `$PRIOR`, user subroutines
#' and anything else outside that subset are refused with an error of
#' class `zupkpmx_parse_error` naming the file, line and column. The title,
#' data records, estimation settings, tables, comments and the ODE solver
#' are recorded as losses of the read.
#'
#' NONMEM's names are kept: `THETA(1)` reads as `THETA1`, `ETA(1)` as
#' `ETA1` and `EPS(1)` as `EPS1`. The observation is the endpoint `Y`.
#'
#' @param file Path to the control stream.
#' @param estimates Where final estimates come from. `"auto"` reads
#'   `run.xml` (`<file>.xml`) through zuxml when both exist, else the
#'   `.ext` file, else none. `"xml"`, `"ext"` and `"lst"` read that file and
#'   fail if it is missing; `"none"` reads no estimates.
#' @return A `zupkpmx_model`.
#' @export
#' @examples
#' path <- system.file("corpus", "twocmt-bolus", "model.ctl", package = "zupkpmx")
#' pmx_read_nonmem(path)
pmx_read_nonmem <- function(
  file,
  estimates = c("auto", "xml", "ext", "lst", "none")
) {
  if (!is.character(estimates) || anyNA(estimates)) {
    zupkpmx_abort("`estimates` must be one of the documented strings.", "model")
  }
  estimates <- match.arg(estimates)
  if (
    !is.character(file) ||
      length(file) != 1 ||
      !file.exists(file) ||
      dir.exists(file)
  ) {
    zupkpmx_abort("`file` must be the path of an existing file.", "model")
  }
  model <- nm_read(file)
  nm_attach_estimates(model, file, estimates)
}

nm_closed <- list(
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
nm_ode_advans <- c(6L, 8L, 9L, 13L)

nm_lost_records <- c(
  SIZES = "array sizes ($SIZES)",
  PROBLEM = "title ($PROBLEM)",
  INPUT = "data records ($INPUT, $DATA)",
  DATA = "data records ($INPUT, $DATA)",
  ESTIMATION = "estimation settings ($ESTIMATION, $COVARIANCE)",
  COVARIANCE = "estimation settings ($ESTIMATION, $COVARIANCE)",
  TABLE = "tables ($TABLE, $SCATTERPLOT)",
  SCATTERPLOT = "tables ($TABLE, $SCATTERPLOT)",
  SIMULATION = "simulation settings ($SIMULATION)",
  ABBREVIATED = "abbreviated-code options ($ABBREVIATED)"
)

nm_read <- function(file) {
  stream <- nm_split(readLines(file, warn = FALSE), file)
  records <- stream$records
  names <- vapply(records, `[[`, character(1), "name")
  losses <- if (stream$comments) "comments" else character()

  for (r in records) {
    if (r$name %in% c("PRED", "INFN", "MIX", "PRIOR")) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = paste0("$", r$name),
        message = sprintf(
          "not translated: $%s at line %d",
          r$name,
          r$line[[1]]
        ),
        call = NULL
      )
    }
  }
  losses <- union(
    losses,
    unique(unname(nm_lost_records[intersect(names, names(nm_lost_records))]))
  )
  for (r in records) {
    if (length(r$options) == 1 && r$name %in% c("PK", "DES", "ERROR")) {
      losses <- union(
        losses,
        sprintf("record options ($%s %s)", r$name, r$options)
      )
    }
  }
  record <- function(name) records[names == name]
  one <- function(name) {
    found <- record(name)
    if (length(found) == 0) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = "end of file",
        expected = paste0("$", name),
        call = NULL
      )
    }
    if (length(found) > 1) {
      parse_error(
        file,
        found[[2]]$line[[1]],
        1L,
        seen = paste0("$", name),
        expected = sprintf("one $%s", name),
        call = NULL
      )
    }
    found[[1]]
  }

  subroutines <- nm_subroutines(one("SUBROUTINES"), file)
  losses <- union(losses, subroutines$losses)
  closed <- nm_closed[[as.character(subroutines$advan)]]
  if (!is.null(closed)) {
    if (length(record("DES")) > 0 || length(record("MODEL")) > 0) {
      r <- c(record("DES"), record("MODEL"))[[1]]
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = paste0("$", r$name),
        expected = sprintf("no $%s with ADVAN%d", r$name, subroutines$advan),
        call = NULL
      )
    }
    compartments <- closed$cmt
    defobs <- closed$central
  } else {
    comp <- nm_compartments(one("MODEL"), file)
    compartments <- comp$names
    defobs <- comp$defobs
  }
  cmt <- function(n, line, column) {
    if (n < 1 || n > length(compartments)) {
      parse_error(
        file,
        line,
        column,
        seen = sprintf("A(%d)", n),
        expected = sprintf("a compartment from 1 to %d", length(compartments)),
        call = NULL
      )
    }
    compartments[[n]]
  }

  thetas <- nm_inits(record("THETA"), "THETA", file)
  omega <- nm_blocks(record("OMEGA"), "ETA", file)
  sigma <- nm_blocks(record("SIGMA"), "EPS", file)

  pk <- nm_pk(nm_code(one("PK"), file, cmt), compartments, file)
  if (!is.null(closed)) {
    structure <- nm_closed_structure(
      subroutines,
      closed,
      stmt_targets(pk$individual),
      file
    )
  } else {
    structure <- split_ode(
      nm_code(one("DES"), file, cmt),
      compartments,
      nm_derivative(compartments, file),
      file,
      "$DES"
    )
  }

  scaled <- names(pk$scale)
  if (
    length(scaled) > 1 ||
      (length(scaled) == 1 && as.integer(scaled) != defobs && !is.null(closed))
  ) {
    parse_error(
      file,
      NA_integer_,
      NA_integer_,
      seen = paste0("S", scaled[[length(scaled)]]),
      expected = sprintf("one scale, S%d", defobs),
      call = NULL
    )
  }
  if (length(scaled) == 1) {
    defobs <- as.integer(scaled)
    if (defobs < 1 || defobs > length(compartments)) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = paste0("S", scaled),
        expected = "a scale of a compartment",
        call = NULL
      )
    }
  }
  observed <- as.symbol(compartments[[defobs]])
  prediction <- if (length(scaled) == 1) {
    call("/", observed, pk$scale[[1]])
  } else {
    observed
  }

  error <- read_observation(
    nm_code(one("ERROR"), file, cmt),
    file,
    "$ERROR",
    prediction = quote(F)
  )
  losses <- union(losses, error$losses)

  parameters <- thetas
  parameters$role <- infer_roles(parameters$name, pk$individual, structure)
  read_model(
    file,
    parameters = parameters,
    random_effects = list(
      omega = omega,
      sigma = sigma,
      attach = attach_etas(unlist(lapply(omega, `[[`, "names")), pk$individual),
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = pk$individual,
    structure = structure,
    dosing = pk$dosing,
    observations = list(
      Y = list(
        prediction = prediction,
        residual = error$residual,
        transform = "none"
      )
    ),
    provenance = list(
      format = "nonmem",
      file = normalizePath(file),
      estimates = NA_character_,
      hash = unname(tools::md5sum(file)),
      fidelity = if (length(losses) == 0) "exact" else "lossy",
      losses = losses
    )
  )
}

nm_subroutines <- function(r, file) {
  text <- toupper(paste(r$lines, collapse = " "))
  if (grepl("\\bOTHER\\s*=", text)) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = "OTHER=",
      message = sprintf(
        "not translated: user subroutines at line %d",
        r$line[[1]]
      ),
      call = NULL
    )
  }
  advan <- regmatches(text, regexpr("ADVAN[0-9]+", text))
  if (length(advan) == 0) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = trimws(text),
      expected = "ADVANn",
      call = NULL
    )
  }
  advan <- as.integer(sub("ADVAN", "", advan))
  trans <- regmatches(text, regexpr("TRANS[0-9]+", text))
  trans <- if (length(trans) == 0) 1L else as.integer(sub("TRANS", "", trans))

  if (as.character(advan) %in% names(nm_closed)) {
    if (trans != nm_closed[[as.character(advan)]]$trans) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = sprintf("ADVAN%d TRANS%d", advan, trans),
        expected = sprintf(
          "ADVAN%d TRANS%d",
          advan,
          nm_closed[[as.character(advan)]]$trans
        ),
        call = NULL
      )
    }
    return(list(advan = advan, trans = trans, losses = character()))
  }
  if (advan %in% nm_ode_advans) {
    return(list(
      advan = advan,
      trans = trans,
      losses = sprintf("ODE solver (ADVAN%d)", advan)
    ))
  }
  parse_error(
    file,
    r$line[[1]],
    1L,
    seen = sprintf("ADVAN%d", advan),
    expected = "a closed-form or general ODE ADVAN",
    call = NULL
  )
}

nm_compartments <- function(r, file) {
  text <- paste(r$lines, collapse = " ")
  groups <- regmatches(
    text,
    gregexpr(
      "COMP\\s*=\\s*(\\([^)]*\\)|[A-Za-z_][A-Za-z0-9_]*)",
      text,
      ignore.case = TRUE
    )
  )[[1]]
  if (length(groups) == 0) {
    parse_error(
      file,
      r$line[[1]],
      1L,
      seen = trimws(text),
      expected = "COMP=(NAME)",
      call = NULL
    )
  }
  words <- lapply(groups, function(g) {
    inner <- sub("^COMP\\s*=\\s*\\(?", "", g, ignore.case = TRUE)
    strsplit(trimws(gsub("[(),]", " ", inner)), "\\s+")[[1]]
  })
  names <- vapply(words, `[[`, character(1), 1)
  defobs <- which(vapply(
    words,
    function(w) "DEFOBS" %in% toupper(w[-1]),
    logical(1)
  ))
  list(names = names, defobs = if (length(defobs) == 1) defobs else 1L)
}

# The variables a closed form's ADVAN parameters are. `names` are the
# candidates: $PK targets, and for mrgsolve also $PARAM names.
nm_closed_structure <- function(subroutines, closed, names, file) {
  targets <- names
  found <- vapply(
    closed$names,
    function(name) {
      # An exact match first: C++ (mrgsolve) is case-sensitive, and `cl`
      # next to `CL` must not stand in for it.
      hit <- c(targets[targets == name], targets[toupper(targets) == name])
      if (length(hit) == 0) {
        parse_error(
          file,
          NA_integer_,
          NA_integer_,
          seen = "$PK",
          expected = sprintf(
            "%s for ADVAN%d TRANS%d",
            name,
            subroutines$advan,
            subroutines$trans
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
    advan = subroutines$advan,
    trans = subroutines$trans
  )
}

# $THETA: one row per theta, in order across records.
nm_inits <- function(records, kind, file) {
  items <- nm_init_items(records, file)
  rows <- lapply(seq_along(items), function(i) {
    bounds <- read_bounds(items[[i]]$values)
    if (is.null(bounds)) {
      parse_error(
        file,
        items[[i]]$line,
        NA_integer_,
        seen = paste(items[[i]]$values, collapse = ", "),
        expected = "one to three values, lower <= initial <= upper",
        call = NULL
      )
    }
    data.frame(
      name = paste0(kind, i),
      role = "other",
      init = bounds$init,
      lower = bounds$lower,
      upper = bounds$upper,
      fixed = items[[i]]$fixed
    )
  })
  out <- if (length(rows) == 0) {
    model_parameters_empty()[, 1:6]
  } else {
    do.call(rbind, rows)
  }
  out$estimate <- rep(NA_real_, nrow(out))
  out
}

# $OMEGA and $SIGMA: blocks named ETA1, ETA2, ... or EPS1, EPS2, ...
nm_blocks <- function(records, prefix, file) {
  blocks <- list()
  for (r in records) {
    text <- toupper(paste(r$lines, collapse = " "))
    bad <- regmatches(
      text,
      regexpr("\\b(SAME|SD|STANDARD|CORR|CORRELATION|CHOL|CHOLESKY)\\b", text)
    )
    if (length(bad) == 1) {
      parse_error(
        file,
        r$line[[1]],
        1L,
        seen = bad,
        message = sprintf(
          "not translated: %s in $%s at line %d",
          bad,
          r$name,
          r$line[[1]]
        ),
        call = NULL
      )
    }
    size <- regmatches(text, regexec("BLOCK\\s*\\(\\s*([0-9]+)\\s*\\)", text))[[
      1
    ]]
    r$lines <- gsub(
      "(BLOCK|DIAGONAL)\\s*\\(\\s*[0-9]+\\s*\\)|\\b(VARIANCE|COVARIANCE)\\b",
      " ",
      r$lines,
      ignore.case = TRUE
    )
    items <- nm_init_items(list(r), file)
    values <- vapply(
      items,
      function(i) i$values[[length(i$values)]],
      numeric(1)
    )
    if (length(size) == 2) {
      n <- as.integer(size[[2]])
      if (length(values) != n * (n + 1) / 2) {
        parse_error(
          file,
          r$line[[1]],
          1L,
          seen = sprintf("%d values", length(values)),
          expected = sprintf("%d values for BLOCK(%d)", n * (n + 1) / 2, n),
          call = NULL
        )
      }
      names <- paste0(
        prefix,
        length(unlist(lapply(blocks, `[[`, "names"))) + seq_len(n)
      )
      m <- lower_to_full(values)
      dimnames(m) <- list(names, names)
      blocks[[length(blocks) + 1]] <- list(
        names = names,
        matrix = m,
        fixed = any(vapply(items, `[[`, logical(1), "fixed")) ||
          grepl("\\bFIX(ED)?\\b", text)
      )
    } else {
      for (i in seq_along(values)) {
        name <- paste0(
          prefix,
          length(unlist(lapply(blocks, `[[`, "names"))) + 1
        )
        blocks[[length(blocks) + 1]] <- list(
          names = name,
          matrix = matrix(values[[i]], dimnames = list(name, name)),
          fixed = items[[i]]$fixed
        )
      }
    }
  }
  blocks
}

# Initial-value items: `x`, `x FIX`, `(lo, x)`, `(lo, x, hi)`, each with an
# optional FIX inside or after the parentheses.
nm_init_items <- function(records, file) {
  tokens <- list()
  for (r in records) {
    for (k in seq_along(r$lines)) {
      for (tok in nm_tokenize(r$lines[[k]], r$line[[k]], file)) {
        tok$line <- r$line[[k]]
        tokens[[length(tokens) + 1]] <- tok
      }
    }
  }
  i <- 1L
  peek <- function() if (i <= length(tokens)) tokens[[i]]
  fail <- function(expected) {
    tok <- peek()
    parse_error(
      file,
      if (is.null(tok)) NA_integer_ else tok$line,
      if (is.null(tok)) NA_integer_ else tok$column,
      seen = if (is.null(tok)) "end of record" else tok$value,
      expected = expected,
      call = NULL
    )
  }
  is_fix <- function() {
    tok <- peek()
    !is.null(tok) && toupper(tok$value) %in% c("FIX", "FIXED")
  }
  number <- function() {
    sign <- 1
    if (!is.null(peek()) && peek()$value %in% c("-", "+")) {
      sign <- if (peek()$value == "-") -1 else 1
      i <<- i + 1L
    }
    tok <- peek()
    if (is.null(tok)) {
      fail("a number")
    }
    i <<- i + 1L
    if (tok$type == "number") {
      return(sign * as.numeric(sub("[Dd]", "E", tok$value)))
    }
    if (toupper(tok$value) == "INF") {
      return(sign * Inf)
    }
    i <<- i - 1L
    fail("a number")
  }
  items <- list()
  while (i <= length(tokens)) {
    line <- peek()$line
    fixed <- FALSE
    if (peek()$value == "(") {
      i <- i + 1L
      values <- number()
      while (!is.null(peek()) && peek()$value == ",") {
        i <- i + 1L
        values <- c(values, number())
      }
      if (is_fix()) {
        fixed <- TRUE
        i <- i + 1L
      }
      if (is.null(peek()) || peek()$value != ")") {
        fail(")")
      }
      i <- i + 1L
    } else {
      values <- number()
    }
    if (length(values) > 3) {
      fail("one to three values")
    }
    if (is_fix()) {
      fixed <- TRUE
      i <- i + 1L
    }
    items[[length(items) + 1]] <- list(
      values = values,
      fixed = fixed,
      line = line
    )
  }
  items
}

# $PK: individual statements, scale factors and dosing attributes.
nm_pk <- function(stmts, compartments, file) {
  dosing <- list()
  scale <- list()
  individual <- list()
  attrs <- c(ALAG = "lag", F = "bioavailability", R = "rate", D = "duration")
  check_cmt <- function(n, name) {
    if (n < 1 || n > length(compartments)) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = name,
        expected = sprintf("a compartment from 1 to %d", length(compartments)),
        call = NULL
      )
    }
    compartments[[n]]
  }
  for (x in stmts) {
    target <- if (is_call(x, "<-")) x[[2]]
    name <- if (is.symbol(target)) as.character(target) else ""
    m <- regmatches(name, regexec("^(S|ALAG|F|R|D)([0-9]+)$", name))[[1]]
    if (length(m) == 3) {
      n <- as.integer(m[[3]])
      if (m[[2]] == "S") {
        check_cmt(n, name)
        scale[[as.character(n)]] <- x[[3]]
      } else {
        cmt <- check_cmt(n, name)
        dosing[[cmt]] <- c(
          dosing[[cmt]],
          stats::setNames(list(x[[3]]), attrs[[m[[2]]]])
        )
      }
    } else if (is_call(target, "A_0")) {
      cmt <- check_cmt(target[[2]], expr_text(target))
      dosing[[cmt]] <- c(dosing[[cmt]], list(initial = x[[3]]))
    } else if (is_call(target, "DADT")) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = expr_text(target),
        expected = "DADT(n) in $DES",
        call = NULL
      )
    } else {
      individual[[length(individual) + 1]] <- x
    }
  }
  check_dosing_names(
    individual,
    function(n) grepl("^(S|ALAG|F|R|D)[0-9]+$|^A_0\\(", n),
    file,
    "$PK"
  )
  list(
    individual = individual,
    scale = scale,
    dosing = dosing[intersect(compartments, names(dosing))]
  )
}

# $DES's DADT(n) names compartment n.
nm_derivative <- function(compartments, file) {
  function(x) {
    if (!is_call(x[[2]], "DADT")) {
      return(NULL)
    }
    n <- x[[2]][[2]]
    if (n < 1 || n > length(compartments)) {
      parse_error(
        file,
        NA_integer_,
        NA_integer_,
        seen = expr_text(x[[2]]),
        expected = sprintf(
          "DADT of a compartment from 1 to %d",
          length(compartments)
        ),
        call = NULL
      )
    }
    compartments[[n]]
  }
}
