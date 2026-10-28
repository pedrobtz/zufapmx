#' Write a model as a NONMEM control stream
#'
#' Prints a model as an NM-TRAN control stream with its fidelity (see
#' [pmx_fidelity()]).
#'
#' Fixed effects become `THETA(n)`, random effects `ETA(n)` and residual
#' errors `EPS(n)`, numbered in the model's order; compartment amounts
#' become `A(n)`. A closed form is written as its `ADVAN`/`TRANS` pair, an
#' ODE model as `ADVAN13` with `$MODEL` and `$DES`. Residual error held as
#' standard deviations, as rxode2 holds it, is written as `$SIGMA`
#' variances: the same model, parameterised as NONMEM parameterises it.
#'
#' `$INPUT` and `$DATA` are placeholders to point at a dataset, and no
#' `$ESTIMATION` is written: the representation holds neither.
#'
#' @param model A model read by one of the `pmx_read_*()` functions.
#' @param estimates `"final"` writes final estimates where the model has
#'   them, and its initial values elsewhere; `"initial"` writes initial
#'   values. Whichever of the two is not written is recorded as lost.
#' @return A written model: the text as lines, its fidelity and its losses.
#' @export
#' @examples
#' path <- system.file("corpus", "onecmt-oral", "model-rxode2.R", package = "zupkpmx")
#' fun <- eval(parse(path))
#' pmx_write_nonmem(pmx_read_rxode2(fun))
pmx_write_nonmem <- function(model, estimates = c("final", "initial")) {
  check_model(model)
  estimates <- match.arg(estimates)
  m <- unclass(model)
  losses <- character()

  check_writable(m)
  has_estimates <- any(!is.na(m$parameters$estimate)) ||
    any(vapply(
      c(m$random_effects$omega, m$random_effects$sigma),
      function(b) !is.null(b$estimate),
      logical(1)
    ))
  if (has_estimates) {
    losses <- if (estimates == "final") {
      "initial values: final estimates were written as $THETA, $OMEGA and $SIGMA"
    } else {
      "final estimates: initial values were written"
    }
  }

  m <- nm_sigma_from_sd(m)
  spelling <- nm_names(m)
  text <- c(
    sprintf("$PROBLEM %s", nm_title(m$provenance)),
    "; zupkpmx: $INPUT and $DATA are placeholders; point them at your dataset.",
    "$INPUT ID TIME EVID AMT RATE CMT DV",
    "$DATA data.csv IGNORE=@",
    nm_structure_lines(m, spelling),
    "$PK",
    nm_stmt_lines(
      c(m$individual, closed_aliases(m$structure, spelling)),
      spelling
    ),
    nm_dosing_lines(m, spelling),
    nm_scale_lines(m, spelling),
    if (identical(m$structure$type, "ode")) {
      c(
        "$DES",
        nm_stmt_lines(m$structure$statements, nm_des_spelling(spelling)),
        vapply(
          seq_along(m$structure$compartments),
          function(i) {
            sprintf(
              "DADT(%d) = %s",
              i,
              nm_expr(m$structure$derivatives[[i]], nm_des_spelling(spelling))
            )
          },
          character(1)
        )
      )
    },
    "$ERROR",
    nm_error_lines(m, spelling),
    nm_theta_lines(m$parameters, estimates),
    nm_block_lines("OMEGA", m$random_effects$omega, estimates),
    nm_block_lines("SIGMA", m$random_effects$sigma, estimates)
  )

  path <- tempfile(fileext = ".ctl")
  on.exit(unlink(path))
  writeLines(text, path)
  back <- read_back(nm_read(path), "NONMEM")
  new_written(
    text = text,
    format = "nonmem",
    fidelity = written_fidelity(losses, model, back),
    losses = losses
  )
}

nm_title <- function(provenance) {
  if (identical(provenance$format, "nonmem")) {
    return(basename(provenance$file))
  }
  sprintf("zupkpmx model read from %s", provenance$format)
}

# Residual error held as standard deviations (as rxode2 holds it) moves to
# $SIGMA as variances: each sd parameter becomes one epsilon whose
# variance is its square, and leaves $THETA.
nm_sigma_from_sd <- function(m) {
  if (length(m$random_effects$sigma) > 0) {
    return(m)
  }
  for (e in names(m$observations)) {
    r <- m$observations[[e]]$residual
    if (!r$type %in% c("additive", "proportional", "combined", "exponential")) {
      write_error(
        sprintf("a %s residual model", r$type),
        sprintf("observations$%s$residual", e),
        formats = "rxode2",
        call = NULL
      )
    }
    eps <- character()
    for (sd in r$parameters) {
      used <- unlist(lapply(
        c(m$individual, m$structure$statements, m$structure$derivatives),
        all.vars
      ))
      if (sd %in% used) {
        write_error(
          sprintf("the residual parameter `%s`, which the model also uses", sd),
          sprintf("observations$%s$residual", e),
          formats = "rxode2",
          call = NULL
        )
      }
      row <- m$parameters[m$parameters$name == sd, ]
      if (nrow(row) != 1) {
        write_error(
          sprintf(
            "the residual parameter `%s`, which is not a fixed effect",
            sd
          ),
          sprintf("observations$%s$residual", e),
          formats = character(),
          call = NULL
        )
      }
      name <- paste0("EPS", length(m$random_effects$sigma) + 1)
      block <- list(
        names = name,
        matrix = matrix(row$init^2, dimnames = list(name, name)),
        fixed = row$fixed
      )
      if (!is.na(row$estimate)) {
        block$estimate <- matrix(row$estimate^2, dimnames = list(name, name))
      }
      m$random_effects$sigma[[length(m$random_effects$sigma) + 1]] <- block
      m$parameters <- m$parameters[m$parameters$name != sd, ]
      eps <- c(eps, name)
    }
    m$observations[[e]]$residual$parameters <- eps
  }
  rownames(m$parameters) <- NULL
  m
}

# Every symbol's NM-TRAN text.
nm_names <- function(m) {
  map <- c(
    stats::setNames(
      sprintf("THETA(%d)", seq_len(nrow(m$parameters))),
      m$parameters$name
    ),
    nm_index_names(m$random_effects$omega, "ETA"),
    nm_index_names(m$random_effects$sigma, "EPS")
  )
  compartments <- model_compartments(m$structure)
  map <- c(
    map,
    stats::setNames(sprintf("A(%d)", seq_along(compartments)), compartments),
    # Time (D15): the record's TIME in $PK and $ERROR; nm_des_spelling()
    # makes it T in $DES.
    t = "TIME"
  )

  vars <- setdiff(
    unique(c(
      stmt_targets(m$individual),
      stmt_targets(m$structure$statements),
      unlist(lapply(
        c(m$individual, m$structure$statements, m$structure$derivatives),
        all.vars
      ))
    )),
    names(map)
  )
  nm_vars <- toupper(gsub(".", "_", vars, fixed = TRUE))
  nm_vars <- closed_free_names(vars, nm_vars, m$structure)
  # A variable PREDPP gives meaning to (F1 is compartment 1's
  # bioavailability) gets a `_` suffix.
  nm_vars <- ifelse(nm_reserved(nm_vars), paste0(nm_vars, "_"), nm_vars)
  bad <- vars[
    duplicated(nm_vars) |
      duplicated(nm_vars, fromLast = TRUE) |
      nm_reserved(nm_vars) |
      !grepl("^[A-Z][A-Z0-9_]*$", nm_vars)
  ]
  if (length(bad) > 0) {
    write_error(
      sprintf("the name `%s`, which NM-TRAN cannot hold apart", bad[[1]]),
      "individual",
      formats = "rxode2",
      call = NULL
    )
  }
  c(map, stats::setNames(nm_vars, vars))
}

# Names NM-TRAN and PREDPP give meaning to: data items the writer declares,
# PREDPP's reserved variables, and the numbered families of scales, dosing
# attributes and model event times.
nm_reserved_names <- c(
  "F",
  "Y",
  "A",
  "T",
  "ERR",
  "EPS",
  "ETA",
  "THETA",
  "DADT",
  "ID",
  "TIME",
  "EVID",
  "AMT",
  "RATE",
  "CMT",
  "DV",
  "MDV",
  "II",
  "SS",
  "ADDL",
  "ICALL",
  "NEWIND",
  "IREP",
  "MIXNUM",
  "MIXEST",
  "PRED",
  "RES",
  "WRES",
  "COMACT",
  "COMSAV",
  "MODE",
  "PCMT",
  "MNEXT",
  "MPAST",
  "MNOW",
  "GETETA",
  "NTHETA",
  "NETA",
  "NEPS",
  "IERPRD",
  "MSEC",
  "MFIRST",
  "NEWL2",
  "LIREC"
)

nm_reserved <- function(names) {
  names %in%
    nm_reserved_names |
    grepl("^(S|F|R|D|ALAG|MTIME|THETA|ETA|EPS|ERR)[0-9]+$|^A_0$", names)
}

# In $DES, continuous time is T.
nm_des_spelling <- function(spelling) {
  spelling[["t"]] <- "T"
  spelling
}

nm_index_names <- function(blocks, prefix) {
  symbols <- unlist(lapply(blocks, `[[`, "names"))
  stats::setNames(sprintf("%s(%d)", prefix, seq_along(symbols)), symbols)
}

nm_structure_lines <- function(m, spelling) {
  s <- m$structure
  if (identical(s$type, "closed")) {
    return(sprintf("$SUBROUTINES ADVAN%d TRANS%d", s$advan, s$trans))
  }
  c(
    "$SUBROUTINES ADVAN13 TOL=9",
    paste0(
      "$MODEL ",
      paste(sprintf("COMP=(%s)", toupper(s$compartments)), collapse = " ")
    )
  )
}

nm_dosing_lines <- function(m, spelling) {
  compartments <- model_compartments(m$structure)
  prefix <- c(lag = "ALAG", bioavailability = "F", rate = "R", duration = "D")
  out <- character()
  for (cmt in names(m$dosing)) {
    n <- match(cmt, compartments)
    for (attr in names(m$dosing[[cmt]])) {
      target <- if (attr == "initial") {
        sprintf("A_0(%d)", n)
      } else {
        paste0(prefix[[attr]], n)
      }
      out <- c(
        out,
        sprintf("%s = %s", target, nm_expr(m$dosing[[cmt]][[attr]], spelling))
      )
    }
  }
  out
}

nm_scale_lines <- function(m, spelling) {
  compartments <- model_compartments(m$structure)
  pred <- m$observations[[1]]$prediction
  if (is.symbol(pred) && as.character(pred) %in% compartments) {
    return(character())
  }
  if (
    is_call(pred, "/") &&
      is.symbol(pred[[2]]) &&
      as.character(pred[[2]]) %in% compartments
  ) {
    n <- match(as.character(pred[[2]]), compartments)
    if (
      identical(m$structure$type, "closed") && compartments[[n]] != "central"
    ) {
      write_error(
        "a closed-form prediction other than the central concentration",
        "observations",
        formats = "rxode2",
        call = NULL
      )
    }
    scale <- nm_expr(pred[[3]], spelling)
    # Scale a closed form by its ADVAN's own name for the central volume,
    # so the control stream reads back as the central concentration.
    if (
      identical(m$structure$type, "closed") &&
        identical(pred[[3]], as.symbol(m$structure$parameters[["v"]]))
    ) {
      scale <- nm_closed[[as.character(m$structure$advan)]]$names[["v"]]
    }
    return(sprintf("S%d = %s", n, scale))
  }
  write_error(
    "a prediction that is not a compartment amount over a scale",
    "observations",
    formats = "rxode2",
    call = NULL
  )
}

nm_error_lines <- function(m, spelling) {
  r <- m$observations[[1]]$residual
  p <- vapply(r$parameters, function(e) spelling[[e]], character(1))
  y <- switch(
    r$type,
    additive = sprintf("F + %s", p[[1]]),
    proportional = sprintf("F * (1 + %s)", p[[1]]),
    combined = sprintf("F * (1 + %s) + %s", p[[2]], p[[1]]),
    exponential = sprintf("F * EXP(%s)", p[[1]]),
    expression = nm_expr(r$expression, c(spelling, F = "F")),
    write_error(
      sprintf("a %s residual model", r$type),
      "observations",
      formats = "rxode2",
      call = NULL
    )
  )
  sprintf("Y = %s", y)
}

nm_theta_lines <- function(p, estimates) {
  vapply(
    seq_len(nrow(p)),
    function(i) {
      value <- if (estimates == "final" && !is.na(p$estimate[[i]])) {
        p$estimate[[i]]
      } else {
        p$init[[i]]
      }
      values <- vapply(
        bounded_values(p$lower[[i]], value, p$upper[[i]]),
        nm_number,
        character(1)
      )
      bounds <- if (length(values) == 1) {
        values
      } else {
        sprintf("(%s)", paste(values, collapse = ", "))
      }
      sprintf(
        "$THETA %s%s ; %s",
        bounds,
        if (p$fixed[[i]]) " FIX" else "",
        p$name[[i]]
      )
    },
    character(1)
  )
}

nm_block_lines <- function(record, blocks, estimates) {
  vapply(
    blocks,
    function(b) {
      m <- if (estimates == "final" && !is.null(b$estimate)) {
        b$estimate
      } else {
        b$matrix
      }
      values <- paste(
        vapply(m[upper.tri(m, diag = TRUE)], nm_number, character(1)),
        collapse = " "
      )
      fix <- if (b$fixed) " FIX" else ""
      if (length(b$names) == 1) {
        sprintf("$%s %s%s", record, values, fix)
      } else {
        sprintf("$%s BLOCK(%d)%s %s", record, length(b$names), fix, values)
      }
    },
    character(1)
  )
}

nm_number <- function(x) {
  if (is.infinite(x)) {
    return(if (x > 0) "INF" else "-INF")
  }
  num_text(x)
}

nm_stmt_lines <- function(stmts, spelling, indent = 0) {
  pad <- strrep(" ", indent)
  unlist(lapply(stmts, function(x) {
    if (is_call(x, "<-")) {
      return(sprintf(
        "%s%s = %s",
        pad,
        # An ADVAN alias assigns a name that is already NONMEM's.
        spelling[as.character(x[[2]])] %|NA|% as.character(x[[2]]),
        nm_expr(x[[3]], spelling)
      ))
    }
    nm_if_lines(x, spelling, indent)
  }))
}

nm_if_lines <- function(x, spelling, indent, keyword = "IF") {
  pad <- strrep(" ", indent)
  out <- c(
    sprintf("%s%s (%s) THEN", pad, keyword, nm_expr(x[[2]], spelling)),
    nm_stmt_lines(stmt_body(x[[3]]), spelling, indent + 2)
  )
  if (length(x) == 4) {
    other <- x[[4]]
    out <- if (is_call(other, "if")) {
      c(out, nm_if_lines(other, spelling, indent, "ELSE IF"))
    } else {
      c(
        out,
        sprintf("%sELSE", pad),
        nm_stmt_lines(stmt_body(other), spelling, indent + 2)
      )
    }
  }
  if (keyword == "IF") c(out, sprintf("%sENDIF", pad)) else out
}

# Print an expression as NM-TRAN, adding parentheses where Fortran's
# precedence needs them.
nm_expr <- function(x, spelling) {
  expr_print(x, spelling, nm_dialect)
}

nm_dialect <- list(
  name = "NM-TRAN",
  formats = "rxode2",
  operators = c(
    `|` = ".OR.",
    `&` = ".AND.",
    `==` = ".EQ.",
    `!=` = ".NE.",
    `<` = ".LT.",
    `<=` = ".LE.",
    `>` = ".GT.",
    `>=` = ".GE.",
    `+` = "+",
    `-` = "-",
    `*` = "*",
    `/` = "/",
    `^` = "**"
  ),
  not = ".NOT.",
  power = NULL,
  functions = stats::setNames(names(nm_functions), nm_functions),
  number = function(x) nm_number(x)
)
