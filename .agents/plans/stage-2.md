# Stage 2 — Representation and expressions: implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:executing-plans.

**Goal:** A `zupkpmx_model` that can be built, validated, inspected and
printed without any reader.

**Architecture:** Expressions are R language objects (numbers, symbols,
calls) restricted by a validator to the operators and function table of
§4.5. Statements are `name <- expr` and `if (cond) {...} else {...}`. The
model is a classed list of seven components built by an internal
`new_model()` and checked by `validate_model()`. Readers (Stages 3, 4, 6)
are the only public way in, so the constructor stays internal.

**Spec:** `.agents/design.md` §4.1, §4.5, §5, §8.

## Global Constraints

- Base R only (D8). No exported constructor: §5 exports readers and
  accessors only.
- Errors through `zupkpmx_abort(message, kind, ...)`; classes
  `zupkpmx_<kind>_error` inheriting `zupkpmx_error`.

## Review Focus

- An expression calling a function outside the table (`foo(x)`, `sin`) is
  rejected with `zupkpmx_model_error` naming the function.
- A non-symmetric or non-square omega block is rejected.
- A random effect attached to a parameter that does not exist is rejected.
- A closed-form structure with `ncmt` outside 1–3 is rejected.
- Printing a model without estimates, or without random effects, works.

---

### Task 1: Expressions (`R/expr.R`, `tests/testthat/test-expr.R`)

- `expr_functions`: `exp`, `log`, `log10`, `sqrt`, `abs`, `min`, `max`.
- `expr_operators`: `+ - * / ^ ( == != < <= > >= & | !`.
- `expr_check(x)`: `x` is numeric length-1 constant, symbol, or call to an
  operator or table function with valid arguments; else
  `zupkpmx_model_error`. Returns `x` invisibly.
- `expr_names(x)`: symbols an expression reads.
- `stmt_check(x)`: `<-` with a symbol on the left and a valid expression on
  the right, or `if` with a valid condition and `{` bodies of statements.
- `stmt_targets(x)`: names a statement list assigns.

### Task 2: Model (`R/model.R`, `tests/testthat/test-model.R`)

Components, each validated:

1. `parameters`: data frame `name`, `role` (`clearance`, `volume`, `ka`,
   `other`), `init`, `lower`, `upper`, `fixed`, `estimate` (`NA` if none).
2. `random_effects`: `list(omega, sigma, attach, iov)`; `omega` and `sigma`
   are lists of blocks `list(names, matrix, fixed)`; `attach` is a data
   frame `eta`, `parameter`, `transform` (`exp`, `additive`, `logit`,
   `expression`); `iov` is `list(present = FALSE, occasion = NA)`.
3. `individual`: list of statements.
4. `structure`: `list(type = "closed", ncmt, depot, parameters, advan,
   trans)` with `parameters` a named character vector from canonical name
   (`cl`, `v`, `q`, `vp`, `q2`, `vp2`, `ka`) to the model's variable, or
   `list(type = "ode", compartments, statements, derivatives)` with
   `derivatives` a named list of expressions, one per compartment.
5. `dosing`: named list by compartment of `list(lag, bioavailability,
   rate, duration, initial)`, each an expression or `NULL`.
6. `observations`: named list by endpoint of `list(prediction, residual =
   list(type, parameters), transform)`; residual `type` in `additive`,
   `proportional`, `combined`, `exponential`, `power`, `expression`;
   `transform` in `none`, `log`.
7. `provenance`: `list(format, file, estimates, hash, fidelity)`.

- `new_model(...)` takes the seven components and validates.
- `pmx_parameters(m)`, `pmx_omega(m)` (block-diagonal matrix with
  dimnames), `pmx_ode(m)` (derivatives, or `NULL` for a closed form).
- `print.zupkpmx_model()`: source, structure, parameter count with
  estimate status, etas and what they attach to, endpoints and residual
  models.
- Snapshot tests: `print()` and `str()` of a two-compartment closed-form
  model and a Michaelis-Menten ODE model, both built in
  `helper-model.R`.

### Task 3: Condition classes (`R/errors.R`, `tests/testthat/test-errors.R`)

- `parse_error(message, file, line, column, seen, expected)`,
  `write_error(message, construct, location, formats)`,
  `engine_error(message, engine, parent)`; each a `zupkpmx_abort()` with
  those fields. Tests assert class and fields.

### Task 4: Records

- NEWS (nothing user-visible beyond accessors: add them), roadmap Done.
