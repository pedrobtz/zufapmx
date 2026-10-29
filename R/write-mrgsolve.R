#' Write a model as an mrgsolve model specification
#'
#' Prints a model as the text of an mrgsolve model file (`.cpp`) with its
#' fidelity (see [pmx_fidelity()]).
#'
#' Fixed effects go to `$PARAM @annotated`, whose description records each
#' one's bounds and whether it is fixed (`[0, Inf] fixed`), so nothing an
#' estimation needs is lost; [pmx_read_mrgsolve()] reads them back. A closed
#' form is written as `$PKMODEL` with its `advan` and `trans`, an ODE model
#' as `$CMT` and `$ODE`. The prediction is captured as `IPRED` and the
#' observation as `Y`, with residual error over `EPS(n)` and `$SIGMA`;
#' residual error held as standard deviations, as rxode2 holds it, is
#' written as variances. `$PARAM` holds one value per parameter, so a model
#' with estimates is written with them and its initial values are recorded
#' as lost.
#'
#' @inheritParams pmx_inspect
#' @inherit pmx_write_rxode2 return
#' @family writers
#' @export
#' @examples
#' path <- system.file(
#'   "corpus", "onecmt-oral", "model-rxode2.R",
#'   package = "zufapmx"
#' )
#' fun <- eval(parse(path))
#' pmx_write_mrgsolve(pmx_read_rxode2(fun))
pmx_write_mrgsolve <- function(model) {
  check_model(model)
  m <- unclass(model)
  losses <- character()

  check_writable(m)
  if (any(!is.na(m$parameters$estimate))) {
    losses <- c(
      losses,
      paste(
        "initial values: mrgsolve's $PARAM holds one value per parameter,",
        "so estimates were written"
      )
    )
  }
  if (
    any(vapply(
      c(m$random_effects$omega, m$random_effects$sigma),
      function(b) isTRUE(b$fixed),
      logical(1)
    ))
  ) {
    losses <- c(losses, "fixed flags of $OMEGA and $SIGMA blocks")
  }

  losses <- c(losses, sd_bound_losses(m))
  m <- sigma_from_sd(m)
  spelling <- mrg_names(m)
  ode_spelling <- spelling
  ode_spelling[["t"]] <- "SOLVERTIME"
  closed <- identical(m$structure$type, "closed")
  compartments <- model_compartments(m$structure)

  text <- c(
    sprintf("$PROB %s", model_title(m$provenance)),
    "",
    "$PARAM @annotated",
    mrg_param_lines(m$parameters, spelling),
    "",
    if (closed) {
      sprintf(
        "$PKMODEL advan = %d, trans = %d, cmt = \"%s\"",
        m$structure$advan,
        m$structure$trans,
        paste(spelling[compartments], collapse = " ")
      )
    } else {
      sprintf("$CMT %s", paste(spelling[compartments], collapse = " "))
    },
    "",
    "$MAIN",
    mrg_stmt_lines(m$individual, spelling),
    closed_alias_lines(m$structure, spelling, "double %s = %s;"),
    mrg_dosing_lines(m, spelling),
    if (!closed) {
      c(
        "",
        "$ODE",
        mrg_stmt_lines(m$structure$statements, ode_spelling),
        vapply(
          compartments,
          function(cmt) {
            sprintf(
              "dxdt_%s = %s;",
              spelling[[cmt]],
              cpp_expr(m$structure$derivatives[[cmt]], ode_spelling)
            )
          },
          character(1),
          USE.NAMES = FALSE
        )
      )
    },
    mrg_block_lines("OMEGA", m$random_effects$omega),
    mrg_block_lines("SIGMA", m$random_effects$sigma),
    "",
    "$TABLE",
    sprintf("double IPRED = %s;", mrg_prediction(m, spelling)),
    sprintf("double Y = %s;", mrg_residual(m, spelling)),
    "",
    "$CAPTURE IPRED Y"
  )

  path <- tempfile(fileext = ".cpp")
  on.exit(unlink(path))
  writeLines(text, path)
  back <- read_back(mrg_read(path), "mrgsolve")
  new_written(
    text = text,
    format = "mrgsolve",
    fidelity = written_fidelity(losses, model, back, fixed_blocks = FALSE),
    losses = losses
  )
}

mrg_reserved_names <- c(
  "ID",
  "TIME",
  "SOLVERTIME",
  "EVID",
  "CMT",
  "AMT",
  "II",
  "SS",
  "ADDL",
  "RATE",
  "NEWIND",
  "IPRED",
  "Y",
  "F",
  "ETA",
  "EPS",
  "THETA",
  "pi",
  "double",
  "int",
  "bool",
  "if",
  "else",
  # C++ keywords a variable might be named after, and the functions model
  # code calls.
  "auto",
  "break",
  "case",
  "char",
  "const",
  "continue",
  "do",
  "false",
  "float",
  "for",
  "long",
  "return",
  "short",
  "static",
  "struct",
  "switch",
  "true",
  "void",
  "while",
  "pow",
  names(cpp_functions)
)

mrg_reserved <- function(x) x %in% mrg_reserved_names

# Every symbol's C++ text.
mrg_names <- function(m) {
  closed <- identical(m$structure$type, "closed")
  compartments <- model_compartments(m$structure)
  cmt_names <- if (closed) {
    toupper(sub(
      "^peripheral",
      "PERIPH",
      sub("^central$", "CENT", compartments)
    ))
  } else {
    gsub(".", "_", compartments, fixed = TRUE)
  }
  map <- c(
    index_names(m$random_effects$omega, "ETA"),
    index_names(m$random_effects$sigma, "EPS"),
    stats::setNames(cmt_names, compartments),
    # Time (D15): the record's TIME in $MAIN and $TABLE; SOLVERTIME in $ODE.
    t = "TIME"
  )
  vars <- setdiff(
    unique(c(
      m$parameters$name,
      stmt_targets(m$individual),
      stmt_targets(m$structure$statements),
      unlist(lapply(
        c(m$individual, m$structure$statements, m$structure$derivatives),
        all.vars
      ))
    )),
    names(map)
  )
  bad_cmt <- compartments[
    mrg_reserved(cmt_names) | !grepl("^[A-Za-z_][A-Za-z0-9_]*$", cmt_names)
  ]
  if (length(bad_cmt) > 0) {
    write_error(
      sprintf(
        "the compartment name `%s`, which mrgsolve reserves",
        bad_cmt[[1]]
      ),
      "structure",
      formats = "rxode2",
      call = NULL
    )
  }
  out <- gsub(".", "_", vars, fixed = TRUE)
  out <- closed_free_names(vars, out, m$structure)
  spelled <- c(unname(map), out)
  bad <- vars[
    mrg_reserved(out) |
      duplicated(spelled)[-seq_along(map)] |
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$", out)
  ]
  if (length(bad) > 0) {
    write_error(
      sprintf("the name `%s`, which mrgsolve cannot hold apart", bad[[1]]),
      "individual",
      formats = "rxode2",
      call = NULL
    )
  }
  c(map, stats::setNames(out, vars))
}

mrg_param_lines <- function(p, spelling) {
  vapply(
    seq_len(nrow(p)),
    function(i) {
      value <- if (is.na(p$estimate[[i]])) p$init[[i]] else p$estimate[[i]]
      sprintf(
        "%s : %s : [%s, %s]%s",
        spelling[[p$name[[i]]]],
        num_text(value),
        num_text(p$lower[[i]]),
        num_text(p$upper[[i]]),
        if (p$fixed[[i]]) " fixed" else ""
      )
    },
    character(1)
  )
}

mrg_dosing_lines <- function(m, spelling) {
  out <- character()
  for (cmt in names(m$dosing)) {
    for (field in names(m$dosing[[cmt]])) {
      target <- if (field == "initial") {
        paste0(spelling[[cmt]], "_0")
      } else {
        paste0(mrg_dosing_macros[[field]], "_", spelling[[cmt]])
      }
      out <- c(
        out,
        sprintf(
          "%s = %s;",
          target,
          cpp_expr(m$dosing[[cmt]][[field]], spelling)
        )
      )
    }
  }
  out
}

# A block's estimates when it has them, like $PARAM's values.
mrg_block_lines <- function(record, blocks) {
  unlist(lapply(blocks, function(b) {
    m <- b$estimate %||% b$matrix
    v <- m[upper.tri(m, diag = TRUE)]
    c(
      "",
      sprintf("$%s%s", record, if (length(b$names) > 1) " @block" else ""),
      paste(vapply(v, num_text, character(1)), collapse = " ")
    )
  }))
}

# The prediction as C++. A closed form's central concentration is written
# over the ADVAN's own name for the central volume (V2 = v is aliased in
# $MAIN), so it reads back as the central concentration.
mrg_prediction <- function(m, spelling) {
  pred <- m$observations[[1]]$prediction
  s <- m$structure
  if (identical(s$type, "closed")) {
    if (
      identical(pred, call("/", quote(central), as.symbol(s$parameters[["v"]])))
    ) {
      v <- closed_forms[[as.character(s$advan)]]$names[["v"]]
      return(sprintf("%s / %s", spelling[["central"]], v))
    }
    if (any(all.vars(pred) %in% closed_compartments(s))) {
      closed_prediction_error(names(m$observations)[[1]])
    }
  }
  cpp_expr(pred, spelling)
}

mrg_residual <- function(m, spelling) {
  r <- m$observations[[1]]$residual
  p <- vapply(r$parameters, function(e) spelling[[e]], character(1))
  switch(
    r$type,
    additive = sprintf("IPRED + %s", p[[1]]),
    proportional = sprintf("IPRED * (1 + %s)", p[[1]]),
    combined = sprintf("IPRED * (1 + %s) + %s", p[[2]], p[[1]]),
    exponential = sprintf("IPRED * exp(%s)", p[[1]]),
    # mrgsolve's reader finds the prediction by Y's form, so a residual
    # written as an expression would not read back.
    expression = write_error(
      "a residual model written as an expression over epsilons",
      sprintf("observations$%s$residual", names(m$observations)[[1]]),
      formats = "nonmem",
      call = NULL
    ),
    write_error(
      sprintf("the %s residual model", r$type),
      sprintf("observations$%s$residual", names(m$observations)[[1]]),
      formats = residual_formats(r$type),
      call = NULL
    )
  )
}

# Statements as C++. A variable is declared `double` where it is first
# assigned at the top level. The validator guarantees that is before any
# `if` assigns it (D14), and that `$ODE` assigns none of `$MAIN`'s names,
# which mrgsolve would declare twice.
mrg_stmt_lines <- function(stmts, spelling, declared = character(), depth = 0) {
  pad <- strrep(" ", depth)
  out <- character()
  for (x in stmts) {
    if (is_call(x, "<-")) {
      name <- as.character(x[[2]])
      text <- spelling[name] %|NA|% name
      decl <- if (depth == 0 && !name %in% declared) "double " else ""
      declared <- c(declared, name)
      out <- c(
        out,
        sprintf("%s%s%s = %s;", pad, decl, text, cpp_expr(x[[3]], spelling))
      )
    } else {
      out <- c(out, mrg_if_lines(x, spelling, declared, depth))
    }
  }
  out
}

mrg_if_lines <- function(x, spelling, declared, depth) {
  pad <- strrep(" ", depth)
  out <- c(
    sprintf("%sif (%s) {", pad, cpp_expr(x[[2]], spelling)),
    mrg_stmt_lines(stmt_body(x[[3]]), spelling, declared, depth + 2)
  )
  if (length(x) == 4) {
    other <- x[[4]]
    if (is_call(other, "if")) {
      inner <- mrg_if_lines(other, spelling, declared, depth)
      inner[[1]] <- sprintf("%s} else %s", pad, trimws(inner[[1]]))
      return(c(out, inner))
    }
    out <- c(
      out,
      sprintf("%s} else {", pad),
      mrg_stmt_lines(stmt_body(other), spelling, declared, depth + 2)
    )
  }
  c(out, sprintf("%s}", pad))
}

cpp_expr <- function(x, spelling) {
  expr_print(x, spelling, cpp_dialect)
}

cpp_dialect <- list(
  name = "mrgsolve",
  formats = c("rxode2", "nonmem"),
  operators = c(
    `|` = "||",
    `&` = "&&",
    `==` = "==",
    `!=` = "!=",
    `<` = "<",
    `<=` = "<=",
    `>` = ">",
    `>=` = ">=",
    `+` = "+",
    `-` = "-",
    `*` = "*",
    `/` = "/"
  ),
  not = "!",
  power = "pow",
  # fabs, fmin and fmax: C++'s abs(), min() and max() are integer or
  # template functions.
  functions = invert_names(cpp_functions),
  # A C++ literal without a decimal point is an integer, and 1/2 is 0.
  number = function(x) with_point(num_text(x))
)
