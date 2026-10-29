# Models built directly with new_model(), mirroring corpus entries, for
# tests that need a representation without a reader.

model_parameters <- function(name, role, init, estimate = NA_real_) {
  data.frame(
    name = name,
    role = role,
    init = init,
    lower = 0,
    upper = Inf,
    fixed = FALSE,
    estimate = estimate
  )
}

model_block <- function(names, values) {
  m <- diag(values, nrow = length(names))
  dimnames(m) <- list(names, names)
  list(names = names, matrix = m, fixed = FALSE)
}

model_twocmt_oral <- function(estimates = FALSE) {
  init <- c(1, 4, 20, 2, 40)
  new_model(
    parameters = model_parameters(
      name = c("tka", "tcl", "tv", "tq", "tvp"),
      role = c("ka", "clearance", "volume", "other", "volume"),
      init = init,
      estimate = if (estimates) init * 1.1 else NA_real_
    ),
    random_effects = list(
      omega = list(model_block(c("eta.cl", "eta.v"), c(0.09, 0.09))),
      sigma = list(model_block("eps.prop", 0.04)),
      attach = data.frame(
        eta = c("eta.cl", "eta.v"),
        parameter = c("cl", "v"),
        transform = "exp"
      ),
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = list(
      quote(ka <- tka),
      quote(cl <- tcl * exp(eta.cl)),
      quote(v <- tv * exp(eta.v)),
      quote(q <- tq),
      quote(vp <- tvp)
    ),
    structure = list(
      type = "closed",
      ncmt = 2L,
      depot = TRUE,
      parameters = c(ka = "ka", cl = "cl", v = "v", q = "q", vp = "vp"),
      advan = 4L,
      trans = 4L
    ),
    dosing = list(),
    observations = list(
      cp = list(
        prediction = quote(central / v),
        residual = list(type = "proportional", parameters = "eps.prop"),
        transform = "none"
      )
    ),
    provenance = list(
      format = "test",
      file = NA_character_,
      estimates = NA_character_,
      hash = NA_character_,
      fidelity = NA_character_
    )
  )
}

model_onecmt_mm <- function() {
  new_model(
    parameters = model_parameters(
      name = c("tka", "tvm", "tkm", "tv"),
      role = c("ka", "other", "other", "volume"),
      init = c(1, 20, 2, 50)
    ),
    random_effects = list(
      omega = list(model_block(c("eta.vm", "eta.v"), c(0.09, 0.09))),
      sigma = list(model_block("eps.prop", 0.04)),
      attach = data.frame(
        eta = c("eta.vm", "eta.v"),
        parameter = c("vm", "v"),
        transform = "exp"
      ),
      iov = list(present = FALSE, occasion = NA_character_)
    ),
    individual = list(
      quote(ka <- tka),
      quote(vm <- tvm * exp(eta.vm)),
      quote(km <- tkm),
      quote(v <- tv * exp(eta.v))
    ),
    structure = list(
      type = "ode",
      compartments = c("depot", "central"),
      statements = list(quote(c2 <- central / v)),
      derivatives = list(
        depot = quote(-ka * depot),
        central = quote(ka * depot - vm * c2 / (km + c2))
      )
    ),
    dosing = list(depot = list(lag = quote(0.5))),
    observations = list(
      cp = list(
        prediction = quote(central / v),
        residual = list(type = "proportional", parameters = "eps.prop"),
        transform = "none"
      )
    ),
    provenance = list(
      format = "test",
      file = NA_character_,
      estimates = NA_character_,
      hash = NA_character_,
      fidelity = NA_character_
    )
  )
}

# Rebuild a model with one component replaced, through the validator.
model_with <- function(model, ...) {
  parts <- unclass(model)
  parts[names(list(...))] <- list(...)
  do.call(new_model, parts)
}

# str() without truncation: every element, every character, and 15
# significant digits (17 would show the last-place differences R's decimal
# parser makes without long double).
str_full <- function(x) {
  str(
    x,
    vec.len = Inf,
    list.len = Inf,
    nchar.max = 10000,
    digits.d = 15
  )
}
