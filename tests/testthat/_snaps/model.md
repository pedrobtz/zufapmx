# a closed-form model prints its summary

    Code
      print(model_twocmt_oral())
    Output
      <zupkpmx_model> read from test
      Structure: two-compartment closed form, first-order input (ADVAN4 TRANS4)
      Parameters: 5, initial values only
      Random effects: eta.cl on cl (exp), eta.v on v (exp)
      Endpoints: cp (proportional)

---

    Code
      print(model_twocmt_oral(estimates = TRUE))
    Output
      <zupkpmx_model> read from test
      Structure: two-compartment closed form, first-order input (ADVAN4 TRANS4)
      Parameters: 5, 5 with estimates
      Random effects: eta.cl on cl (exp), eta.v on v (exp)
      Endpoints: cp (proportional)

# an ODE model prints its summary

    Code
      print(model_onecmt_mm())
    Output
      <zupkpmx_model> read from test
      Structure: ODE system over 2 compartments: depot, central
      Parameters: 4, initial values only
      Random effects: eta.vm on vm (exp), eta.v on v (exp)
      Dosing: depot lag
      Endpoints: cp (proportional)

# the representation's structure is stable

    Code
      str(unclass(model_twocmt_oral()))
    Output
      List of 7
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "tka" "tcl" "tv" "tq" ...
        ..$ role    : chr [1:5] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:5] 1 4 20 2 40
        ..$ lower   : num [1:5] 0 0 0 0 0
        ..$ upper   : num [1:5] Inf Inf Inf Inf Inf
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr [1:2] "eta.cl" "eta.v"
        .. .. ..$ matrix: num [1:2, 1:2] 0.09 0 0 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr [1:2] "eta.cl" "eta.v"
        .. .. .. .. ..$ : chr [1:2] "eta.cl" "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "eps.prop"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eps.prop"
        .. .. .. .. ..$ : chr "eps.prop"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "eta.cl" "eta.v"
        .. ..$ parameter: chr [1:2] "cl" "v"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 5
        ..$ : language ka <- tka
        ..$ : language cl <- tcl * exp(eta.cl)
        ..$ : language v <- tv * exp(eta.v)
        ..$ : language q <- tq
        ..$ : language vp <- tvp
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 2
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:5] "ka" "cl" "v" "q" ...
        .. ..- attr(*, "names")= chr [1:5] "ka" "cl" "v" "q" ...
        ..$ advan     : int 4
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "eps.prop"
        .. ..$ transform : chr "none"
       $ provenance    :List of 5
        ..$ format   : chr "test"
        ..$ file     : chr NA
        ..$ estimates: chr NA
        ..$ hash     : chr NA
        ..$ fidelity : chr NA

---

    Code
      str(unclass(model_onecmt_mm()))
    Output
      List of 7
       $ parameters    :'data.frame':	4 obs. of  7 variables:
        ..$ name    : chr [1:4] "tka" "tvm" "tkm" "tv"
        ..$ role    : chr [1:4] "ka" "other" "other" "volume"
        ..$ init    : num [1:4] 1 20 2 50
        ..$ lower   : num [1:4] 0 0 0 0
        ..$ upper   : num [1:4] Inf Inf Inf Inf
        ..$ fixed   : logi [1:4] FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:4] NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr [1:2] "eta.vm" "eta.v"
        .. .. ..$ matrix: num [1:2, 1:2] 0.09 0 0 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr [1:2] "eta.vm" "eta.v"
        .. .. .. .. ..$ : chr [1:2] "eta.vm" "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "eps.prop"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eps.prop"
        .. .. .. .. ..$ : chr "eps.prop"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "eta.vm" "eta.v"
        .. ..$ parameter: chr [1:2] "vm" "v"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 4
        ..$ : language ka <- tka
        ..$ : language vm <- tvm * exp(eta.vm)
        ..$ : language km <- tkm
        ..$ : language v <- tv * exp(eta.v)
       $ structure     :List of 4
        ..$ type        : chr "ode"
        ..$ compartments: chr [1:2] "depot" "central"
        ..$ statements  :List of 1
        .. ..$ : language c2 <- central/v
        ..$ derivatives :List of 2
        .. ..$ depot  : language -ka * depot
        .. ..$ central: language ka * depot - vm * c2/(km + c2)
       $ dosing        :List of 1
        ..$ depot:List of 1
        .. ..$ lag: num 0.5
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "eps.prop"
        .. ..$ transform : chr "none"
       $ provenance    :List of 5
        ..$ format   : chr "test"
        ..$ file     : chr NA
        ..$ estimates: chr NA
        ..$ hash     : chr NA
        ..$ fidelity : chr NA

