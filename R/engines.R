# The engines pmx_compare() simulates through (design §4.4, D4). Each side
# of a comparison resolves to an engine and the text it runs: a written
# model runs as written on its own format's engine; a model runs through
# the writer of an engine other than the one under test.

engine_names <- c("rxode2", "mrgsolve")

# Solver tolerances, tight enough that an ODE solution and a closed form
# agree well inside the comparison's 1e-6 (the corpus uses the same).
engine_atol <- 1e-12
engine_rtol <- 1e-10

engine_available <- function(engine) {
  requireNamespace(engine, quietly = TRUE)
}

engine_version <- function(engine) {
  if (identical(engine, "table")) {
    return(NA_character_)
  }
  as.character(utils::packageVersion(engine))
}

engine_writers <- list(
  rxode2 = function(model) pmx_write_rxode2(model),
  mrgsolve = function(model) pmx_write_mrgsolve(model)
)

# The engine of a written model's own format, or NA (NONMEM has none here).
written_engine <- function(x) {
  if (inherits(x, "zupmxio_written") && x$format %in% engine_names) {
    x$format
  } else {
    NA_character_
  }
}

# One side of a comparison: the engine, the text it runs, the model that
# text reads back as, and where it came from. `requested` is the user's
# engine or NA; `exclude` is the engine of the writer under test.
compare_side <- function(x, requested, exclude) {
  own <- written_engine(x)
  if (!is.na(own) && (is.na(requested) || requested == own)) {
    engine_require(own)
    return(list(
      engine = own,
      text = x$text,
      model = text_model(x$text, own),
      origin = sprintf("%s text, simulated as written", own),
      provenance = written_provenance(x)
    ))
  }

  model <- if (inherits(x, "zupmxio_written")) {
    text_model(x$text, x$format)
  } else {
    x
  }
  source <- unclass(model)$provenance$format
  candidates <- if (!is.na(requested)) {
    requested
  } else {
    setdiff(unique(c(intersect(source, engine_names), engine_names)), exclude)
  }
  refusal <- NULL
  for (engine in candidates) {
    if (!engine_available(engine)) {
      next
    }
    written <- tryCatch(
      engine_writers[[engine]](model),
      zupmxio_write_error = function(e) e
    )
    if (inherits(written, "zupmxio_write_error")) {
      refusal <- refusal %||% written
      next
    }
    return(list(
      engine = engine,
      text = written$text,
      model = text_model(written$text, engine),
      origin = sprintf(
        "%s, written to %s by zupmxio",
        if (inherits(x, "zupmxio_written")) {
          sprintf("%s text", x$format)
        } else {
          sprintf("%s model", source)
        },
        engine
      ),
      provenance = if (inherits(x, "zupmxio_written")) {
        written_provenance(x)
      } else {
        unclass(x)$provenance
      }
    ))
  }
  if (!is.null(refusal)) {
    stop(refusal)
  }
  engine_error(candidates[[1]], call = NULL)
}

engine_require <- function(engine) {
  if (!engine_available(engine)) {
    engine_error(engine, call = NULL)
  }
}

written_provenance <- function(x) {
  list(format = x$format, fidelity = x$fidelity, losses = x$losses)
}

# A model text read back as a model. NONMEM and mrgsolve read files.
text_model <- function(text, format) {
  switch(
    format,
    rxode2 = pmx_read_rxode2(eval(
      parse(text = text, keep.source = FALSE)[[1]],
      baseenv()
    )),
    nonmem = ,
    mrgsolve = {
      path <- tempfile(fileext = if (format == "nonmem") ".ctl" else ".cpp")
      on.exit(unlink(path))
      writeLines(text, path)
      if (format == "nonmem") pmx_read_nonmem(path) else pmx_read_mrgsolve(path)
    }
  )
}

# Simulate one side on one regimen's records. `data` has the design's
# columns plus `cmt` and the covariates in the side's spelling; `etas` is
# a matrix with one row per subject and the side's eta names as columns.
# Returns the endpoint's predictions in the order of data's observation
# records, NA where the engine returned none.
engine_simulate <- function(side, data, etas) {
  endpoint <- names(unclass(side$model)$observations)[[1]]
  out <- tryCatch(
    switch(
      side$engine,
      rxode2 = engine_rxode2(side$text, data, etas),
      mrgsolve = engine_mrgsolve(side$text, data, etas)
    ),
    zupmxio_error = function(e) stop(e),
    error = function(e) engine_error(side$engine, e, call = NULL)
  )
  # Engines spell id and time in either case; the endpoint is matched as
  # spelled, so a variable `y` beside endpoint `Y` is not read (#152).
  folded <- tolower(names(out)) %in% c("id", "time")
  names(out)[folded] <- tolower(names(out)[folded])
  if (!"id" %in% names(out) && length(unique(data$id)) == 1) {
    # rxode2 leaves out the id of a single subject.
    out$id <- rep(data$id[[1]], nrow(out))
  }
  obs <- data[data$evid == 0, ]
  # Element by element: format() pads a vector to a common width, so keys
  # built from tables of different lengths would not match.
  # A subject can have several observations at one time; the k-th of them
  # matches the k-th the engine returns.
  key <- function(id, time) {
    k <- sprintf("%d|%.15g", as.integer(id), time)
    paste(k, stats::ave(seq_along(k), k, FUN = seq_along))
  }
  column <- endpoint
  if (!column %in% names(out)) {
    return(rep(NA_real_, nrow(obs)))
  }
  at <- match(key(obs$id, obs$time), key(out$id, out$time))
  as.numeric(out[[column]][at])
}

engine_rxode2 <- function(text, data, etas) {
  fun <- eval(parse(text = text, keep.source = FALSE)[[1]], baseenv())
  quiet <- function(expr) {
    withCallingHandlers(
      suppressMessages(expr),
      warning = function(w) {
        # Etas come from `params` and residual error is off, so rxode2's
        # notes on simulating without omega or sigma are expected.
        if (grepl("omega|sigma", conditionMessage(w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  ui <- quiet(rxode2::zeroRe(quiet(rxode2::rxode2(fun))))
  params <- if (ncol(etas) > 0) {
    data.frame(id = seq_len(nrow(etas)), etas, check.names = FALSE)
  }
  quiet(rxode2::rxSolve(
    ui,
    data,
    params = params,
    returnType = "data.frame",
    atol = engine_atol,
    rtol = engine_rtol,
    ssAtol = engine_atol,
    ssRtol = engine_rtol
  ))
}

engine_mrgsolve <- function(text, data, etas) {
  mod <- engine_mrgsolve_compiled(text)
  mod <- mrgsolve::zero_re(mod)
  mod <- mrgsolve::update(
    mod,
    atol = engine_atol,
    rtol = engine_rtol,
    ss_atol = engine_atol,
    ss_rtol = engine_rtol
  )
  names(data)[names(data) == "id"] <- "ID"
  args <- list(mod, data = data, obsonly = TRUE, output = "df")
  if (ncol(etas) > 0) {
    idata <- data.frame(ID = seq_len(nrow(etas)), etas)
    names(idata)[-1] <- paste0("ETA", seq_len(ncol(etas)))
    args <- c(args, list(idata = idata, etasrc = "idata.all"))
  }
  out <- suppressMessages(do.call(mrgsolve::mrgsim, args))
  names(out)[names(out) == "ID"] <- "id"
  out
}

# Compiled mrgsolve models, by the hash of their text. mrgsolve names the
# shared library after the model, so the name is the hash: two models of
# one name are then the same code.
engine_cache <- new.env(parent = emptyenv())

engine_mrgsolve_compiled <- function(text) {
  dir <- file.path(tempdir(), "zupmxio-mrgsolve")
  dir.create(dir, showWarnings = FALSE)
  path <- tempfile(tmpdir = dir, fileext = ".cpp")
  writeLines(text, path)
  name <- paste0("zupmxio_", substr(unname(tools::md5sum(path)), 1, 16))
  unlink(path)
  if (is.null(engine_cache[[name]])) {
    writeLines(text, file.path(dir, paste0(name, ".cpp")))
    engine_cache[[name]] <- suppressMessages(
      mrgsolve::mread(name, project = dir, quiet = TRUE)
    )
  }
  engine_cache[[name]]
}
