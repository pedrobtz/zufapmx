# every corpus entry's representation is stable

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral read from mrgsolve 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	3 obs. of  7 variables:
        ..$ name    : chr [1:3] "TVKA" "TVCL" "TVV"
        ..$ role    : chr [1:3] "ka" "clearance" "volume"
        ..$ init    : num [1:3] 1 4 50
        ..$ lower   : num [1:3] -Inf -Inf -Inf
        ..$ upper   : num [1:3] Inf Inf Inf
        ..$ fixed   : logi [1:3] FALSE FALSE FALSE
        ..$ estimate: num [1:3] NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 3
        ..$ : language KA <- TVKA
        ..$ : language CL <- TVCL * exp(ETA1)
        ..$ : language V <- TVV * exp(ETA2)
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 1
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:3] "CL" "V" "KA"
        .. ..- attr(*, "names")= chr [1:3] "cl" "v" "ka"
        ..$ advan     : int 2
        ..$ trans     : int 2
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral read from nonmem 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	3 obs. of  7 variables:
        ..$ name    : chr [1:3] "THETA1" "THETA2" "THETA3"
        ..$ role    : chr [1:3] "ka" "clearance" "volume"
        ..$ init    : num [1:3] 1 4 50
        ..$ lower   : num [1:3] 0 0 0
        ..$ upper   : num [1:3] Inf Inf Inf
        ..$ fixed   : logi [1:3] FALSE FALSE FALSE
        ..$ estimate: num [1:3] NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 3
        ..$ : language KA <- THETA1
        ..$ : language CL <- THETA2 * exp(ETA1)
        ..$ : language V <- THETA3 * exp(ETA2)
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 1
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:3] "CL" "V" "KA"
        .. ..- attr(*, "names")= chr [1:3] "cl" "v" "ka"
        ..$ advan     : int 2
        ..$ trans     : int 2
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral read from rxode2 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	4 obs. of  7 variables:
        ..$ name    : chr [1:4] "tka" "tcl" "tv" "prop.sd"
        ..$ role    : chr [1:4] "ka" "clearance" "volume" "other"
        ..$ init    : num [1:4] 1 4 50 0.2
        ..$ lower   : num [1:4] 0 0 0 -Inf
        ..$ upper   : num [1:4] Inf Inf Inf Inf
        ..$ fixed   : logi [1:4] FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:4] NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.cl"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.v"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.v"
        .. .. .. .. ..$ : chr "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma : list()
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "eta.cl" "eta.v"
        .. ..$ parameter: chr [1:2] "cl" "v"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 3
        ..$ : language ka <- tka
        ..$ : language cl <- tcl * exp(eta.cl)
        ..$ : language v <- tv * exp(eta.v)
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 1
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:3] "cl" "v" "ka"
        .. ..- attr(*, "names")= chr [1:3] "cl" "v" "ka"
        ..$ advan     : int 2
        ..$ trans     : int 2
       $ dosing        : list()
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "prop.sd"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral-lag read from mrgsolve 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "TVKA" "TVCL" "TVV" "TVLAG" ...
        ..$ role    : chr [1:5] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:5] 1 4 50 0.25 0.6
        ..$ lower   : num [1:5] -Inf -Inf -Inf -Inf -Inf
        ..$ upper   : num [1:5] Inf Inf Inf Inf Inf
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 3
        ..$ : language KA <- TVKA
        ..$ : language CL <- TVCL * exp(ETA1)
        ..$ : language V <- TVV * exp(ETA2)
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 1
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:3] "CL" "V" "KA"
        .. ..- attr(*, "names")= chr [1:3] "cl" "v" "ka"
        ..$ advan     : int 2
        ..$ trans     : int 2
       $ dosing        :List of 1
        ..$ depot:List of 2
        .. ..$ lag            : symbol TVLAG
        .. ..$ bioavailability: symbol TVF
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral-lag read from nonmem 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "THETA1" "THETA2" "THETA3" "THETA4" ...
        ..$ role    : chr [1:5] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:5] 1 4 50 0.25 0.6
        ..$ lower   : num [1:5] 0 0 0 0 0
        ..$ upper   : num [1:5] Inf Inf Inf Inf 1
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 3
        ..$ : language KA <- THETA1
        ..$ : language CL <- THETA2 * exp(ETA1)
        ..$ : language V <- THETA3 * exp(ETA2)
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 1
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:3] "CL" "V" "KA"
        .. ..- attr(*, "names")= chr [1:3] "cl" "v" "ka"
        ..$ advan     : int 2
        ..$ trans     : int 2
       $ dosing        :List of 1
        ..$ depot:List of 2
        .. ..$ lag            : symbol THETA4
        .. ..$ bioavailability: symbol THETA5
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral-lag read from rxode2 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	6 obs. of  7 variables:
        ..$ name    : chr [1:6] "tka" "tcl" "tv" "tlag" ...
        ..$ role    : chr [1:6] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:6] 1 4 50 0.25 0.6 0.2
        ..$ lower   : num [1:6] 0 0 0 0 0 ...
        ..$ upper   : num [1:6] Inf Inf Inf Inf 1 ...
        ..$ fixed   : logi [1:6] FALSE FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:6] NA NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.cl"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.v"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.v"
        .. .. .. .. ..$ : chr "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma : list()
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "eta.cl" "eta.v"
        .. ..$ parameter: chr [1:2] "cl" "v"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 3
        ..$ : language ka <- tka
        ..$ : language cl <- tcl * exp(eta.cl)
        ..$ : language v <- tv * exp(eta.v)
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 1
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:3] "cl" "v" "ka"
        .. ..- attr(*, "names")= chr [1:3] "cl" "v" "ka"
        ..$ advan     : int 2
        ..$ trans     : int 2
       $ dosing        :List of 1
        ..$ depot:List of 2
        .. ..$ lag            : symbol tlag
        .. ..$ bioavailability: symbol tf
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "prop.sd"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral-mm read from mrgsolve 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	4 obs. of  7 variables:
        ..$ name    : chr [1:4] "TVKA" "TVVM" "TVKM" "TVV"
        ..$ role    : chr [1:4] "other" "other" "other" "other"
        ..$ init    : num [1:4] 1 20 2 50
        ..$ lower   : num [1:4] -Inf -Inf -Inf -Inf
        ..$ upper   : num [1:4] Inf Inf Inf Inf
        ..$ fixed   : logi [1:4] FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:4] NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "VM" "V"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 4
        ..$ : language KA <- TVKA
        ..$ : language VM <- TVVM * exp(ETA1)
        ..$ : language KM <- TVKM
        ..$ : language V <- TVV * exp(ETA2)
       $ structure     :List of 4
        ..$ type        : chr "ode"
        ..$ compartments: chr [1:2] "DEPOT" "CENT"
        ..$ statements  :List of 1
        .. ..$ : language C2 <- CENT/V
        ..$ derivatives :List of 2
        .. ..$ DEPOT: language -KA * DEPOT
        .. ..$ CENT : language KA * DEPOT - VM * C2/(KM + C2)
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language CENT/V
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral-mm read from nonmem 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	4 obs. of  7 variables:
        ..$ name    : chr [1:4] "THETA1" "THETA2" "THETA3" "THETA4"
        ..$ role    : chr [1:4] "other" "other" "other" "other"
        ..$ init    : num [1:4] 1 20 2 50
        ..$ lower   : num [1:4] 0 0 0 0
        ..$ upper   : num [1:4] Inf Inf Inf Inf
        ..$ fixed   : logi [1:4] FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:4] NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "VM" "V"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 4
        ..$ : language KA <- THETA1
        ..$ : language VM <- THETA2 * exp(ETA1)
        ..$ : language KM <- THETA3
        ..$ : language V <- THETA4 * exp(ETA2)
       $ structure     :List of 4
        ..$ type        : chr "ode"
        ..$ compartments: chr [1:2] "DEPOT" "CENTRAL"
        ..$ statements  :List of 1
        .. ..$ : language C2 <- CENTRAL/V
        ..$ derivatives :List of 2
        .. ..$ DEPOT  : language -KA * DEPOT
        .. ..$ CENTRAL: language KA * DEPOT - VM * C2/(KM + C2)
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language CENTRAL/V
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      onecmt-oral-mm read from rxode2 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "tka" "tvm" "tkm" "tv" ...
        ..$ role    : chr [1:5] "other" "other" "other" "other" ...
        ..$ init    : num [1:5] 1 20 2 50 0.2
        ..$ lower   : num [1:5] 0 0 0 0 -Inf
        ..$ upper   : num [1:5] Inf Inf Inf Inf Inf
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.vm"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.vm"
        .. .. .. .. ..$ : chr "eta.vm"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.v"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.v"
        .. .. .. .. ..$ : chr "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma : list()
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
       $ dosing        : list()
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "prop.sd"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      threecmt-infusion read from mrgsolve 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	6 obs. of  7 variables:
        ..$ name    : chr [1:6] "TVCL" "TVV1" "TVQ2" "TVV2" ...
        ..$ role    : chr [1:6] "clearance" "volume" "other" "volume" ...
        ..$ init    : num [1:6] 4 10 2 20 0.5 40
        ..$ lower   : num [1:6] -Inf -Inf -Inf -Inf -Inf ...
        ..$ upper   : num [1:6] Inf Inf Inf Inf Inf ...
        ..$ fixed   : logi [1:6] FALSE FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:6] NA NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V1"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 6
        ..$ : language CL <- TVCL * exp(ETA1)
        ..$ : language V1 <- TVV1 * exp(ETA2)
        ..$ : language Q2 <- TVQ2
        ..$ : language V2 <- TVV2
        ..$ : language Q3 <- TVQ3
        ..$ : language V3 <- TVV3
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 3
        ..$ depot     : logi FALSE
        ..$ parameters: Named chr [1:6] "CL" "V1" "Q2" "V2" ...
        .. ..- attr(*, "names")= chr [1:6] "cl" "v" "q" "vp" ...
        ..$ advan     : int 11
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V1
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      threecmt-infusion read from nonmem 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	6 obs. of  7 variables:
        ..$ name    : chr [1:6] "THETA1" "THETA2" "THETA3" "THETA4" ...
        ..$ role    : chr [1:6] "clearance" "volume" "other" "volume" ...
        ..$ init    : num [1:6] 4 10 2 20 0.5 40
        ..$ lower   : num [1:6] 0 0 0 0 0 0
        ..$ upper   : num [1:6] Inf Inf Inf Inf Inf ...
        ..$ fixed   : logi [1:6] FALSE FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:6] NA NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V1"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 6
        ..$ : language CL <- THETA1 * exp(ETA1)
        ..$ : language V1 <- THETA2 * exp(ETA2)
        ..$ : language Q2 <- THETA3
        ..$ : language V2 <- THETA4
        ..$ : language Q3 <- THETA5
        ..$ : language V3 <- THETA6
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 3
        ..$ depot     : logi FALSE
        ..$ parameters: Named chr [1:6] "CL" "V1" "Q2" "V2" ...
        .. ..- attr(*, "names")= chr [1:6] "cl" "v" "q" "vp" ...
        ..$ advan     : int 11
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V1
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      threecmt-infusion read from rxode2 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	7 obs. of  7 variables:
        ..$ name    : chr [1:7] "tcl" "tv" "tq" "tvp" ...
        ..$ role    : chr [1:7] "clearance" "volume" "other" "volume" ...
        ..$ init    : num [1:7] 4 10 2 20 0.5 40 0.2
        ..$ lower   : num [1:7] 0 0 0 0 0 ...
        ..$ upper   : num [1:7] Inf Inf Inf Inf Inf ...
        ..$ fixed   : logi [1:7] FALSE FALSE FALSE FALSE FALSE FALSE ...
        ..$ estimate: num [1:7] NA NA NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.cl"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.v"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.v"
        .. .. .. .. ..$ : chr "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma : list()
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "eta.cl" "eta.v"
        .. ..$ parameter: chr [1:2] "cl" "v"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 6
        ..$ : language cl <- tcl * exp(eta.cl)
        ..$ : language v <- tv * exp(eta.v)
        ..$ : language q <- tq
        ..$ : language vp <- tvp
        ..$ : language q2 <- tq2
        ..$ : language vp2 <- tvp2
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 3
        ..$ depot     : logi FALSE
        ..$ parameters: Named chr [1:6] "cl" "v" "q" "vp" ...
        .. ..- attr(*, "names")= chr [1:6] "cl" "v" "q" "vp" ...
        ..$ advan     : int 11
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "prop.sd"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      twocmt-bolus read from mrgsolve 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	4 obs. of  7 variables:
        ..$ name    : chr [1:4] "TVCL" "TVV1" "TVQ" "TVV2"
        ..$ role    : chr [1:4] "clearance" "volume" "other" "volume"
        ..$ init    : num [1:4] 4 20 2 40
        ..$ lower   : num [1:4] -Inf -Inf -Inf -Inf
        ..$ upper   : num [1:4] Inf Inf Inf Inf
        ..$ fixed   : logi [1:4] FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:4] NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V1"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 4
        ..$ : language CL <- TVCL * exp(ETA1)
        ..$ : language V1 <- TVV1 * exp(ETA2)
        ..$ : language Q <- TVQ
        ..$ : language V2 <- TVV2
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 2
        ..$ depot     : logi FALSE
        ..$ parameters: Named chr [1:4] "CL" "V1" "Q" "V2"
        .. ..- attr(*, "names")= chr [1:4] "cl" "v" "q" "vp"
        ..$ advan     : int 3
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V1
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      twocmt-bolus read from nonmem 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	4 obs. of  7 variables:
        ..$ name    : chr [1:4] "THETA1" "THETA2" "THETA3" "THETA4"
        ..$ role    : chr [1:4] "clearance" "volume" "other" "volume"
        ..$ init    : num [1:4] 4 20 2 40
        ..$ lower   : num [1:4] 0 0 0 0
        ..$ upper   : num [1:4] Inf Inf Inf Inf
        ..$ fixed   : logi [1:4] FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:4] NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V1"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 4
        ..$ : language CL <- THETA1 * exp(ETA1)
        ..$ : language V1 <- THETA2 * exp(ETA2)
        ..$ : language Q <- THETA3
        ..$ : language V2 <- THETA4
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 2
        ..$ depot     : logi FALSE
        ..$ parameters: Named chr [1:4] "CL" "V1" "Q" "V2"
        .. ..- attr(*, "names")= chr [1:4] "cl" "v" "q" "vp"
        ..$ advan     : int 3
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V1
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      twocmt-bolus read from rxode2 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "tcl" "tv" "tq" "tvp" ...
        ..$ role    : chr [1:5] "clearance" "volume" "other" "volume" ...
        ..$ init    : num [1:5] 4 20 2 40 0.2
        ..$ lower   : num [1:5] 0 0 0 0 -Inf
        ..$ upper   : num [1:5] Inf Inf Inf Inf Inf
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.cl"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.v"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.v"
        .. .. .. .. ..$ : chr "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma : list()
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "eta.cl" "eta.v"
        .. ..$ parameter: chr [1:2] "cl" "v"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 4
        ..$ : language cl <- tcl * exp(eta.cl)
        ..$ : language v <- tv * exp(eta.v)
        ..$ : language q <- tq
        ..$ : language vp <- tvp
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 2
        ..$ depot     : logi FALSE
        ..$ parameters: Named chr [1:4] "cl" "v" "q" "vp"
        .. ..- attr(*, "names")= chr [1:4] "cl" "v" "q" "vp"
        ..$ advan     : int 3
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "prop.sd"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      twocmt-oral read from mrgsolve 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "TVKA" "TVCL" "TVV2" "TVQ" ...
        ..$ role    : chr [1:5] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:5] 1 4 20 2 40
        ..$ lower   : num [1:5] -Inf -Inf -Inf -Inf -Inf
        ..$ upper   : num [1:5] Inf Inf Inf Inf Inf
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V2"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 5
        ..$ : language KA <- TVKA
        ..$ : language CL <- TVCL * exp(ETA1)
        ..$ : language V2 <- TVV2 * exp(ETA2)
        ..$ : language Q <- TVQ
        ..$ : language V3 <- TVV3
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 2
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:5] "CL" "V2" "Q" "V3" ...
        .. ..- attr(*, "names")= chr [1:5] "cl" "v" "q" "vp" ...
        ..$ advan     : int 4
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V2
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      twocmt-oral read from nonmem 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	5 obs. of  7 variables:
        ..$ name    : chr [1:5] "THETA1" "THETA2" "THETA3" "THETA4" ...
        ..$ role    : chr [1:5] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:5] 1 4 20 2 40
        ..$ lower   : num [1:5] 0 0 0 0 0
        ..$ upper   : num [1:5] Inf Inf Inf Inf Inf
        ..$ fixed   : logi [1:5] FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:5] NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA1"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA1"
        .. .. .. .. ..$ : chr "ETA1"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "ETA2"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "ETA2"
        .. .. .. .. ..$ : chr "ETA2"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma :List of 1
        .. ..$ :List of 3
        .. .. ..$ names : chr "EPS1"
        .. .. ..$ matrix: num [1, 1] 0.04
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "EPS1"
        .. .. .. .. ..$ : chr "EPS1"
        .. .. ..$ fixed : logi FALSE
        ..$ attach:'data.frame':	2 obs. of  3 variables:
        .. ..$ eta      : chr [1:2] "ETA1" "ETA2"
        .. ..$ parameter: chr [1:2] "CL" "V2"
        .. ..$ transform: chr [1:2] "exp" "exp"
        ..$ iov   :List of 2
        .. ..$ present : logi FALSE
        .. ..$ occasion: chr NA
       $ individual    :List of 5
        ..$ : language KA <- THETA1
        ..$ : language CL <- THETA2 * exp(ETA1)
        ..$ : language V2 <- THETA3 * exp(ETA2)
        ..$ : language Q <- THETA4
        ..$ : language V3 <- THETA5
       $ structure     :List of 6
        ..$ type      : chr "closed"
        ..$ ncmt      : int 2
        ..$ depot     : logi TRUE
        ..$ parameters: Named chr [1:5] "CL" "V2" "Q" "V3" ...
        .. ..- attr(*, "names")= chr [1:5] "cl" "v" "q" "vp" ...
        ..$ advan     : int 4
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ Y:List of 3
        .. ..$ prediction: language central/V2
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "EPS1"
        .. ..$ transform : chr "none"

---

    Code
      cat(id, "read from", from, "\n")
    Output
      twocmt-oral read from rxode2 
    Code
      str(model_content(models[[from]]))
    Output
      List of 6
       $ parameters    :'data.frame':	6 obs. of  7 variables:
        ..$ name    : chr [1:6] "tka" "tcl" "tv" "tq" ...
        ..$ role    : chr [1:6] "ka" "clearance" "volume" "other" ...
        ..$ init    : num [1:6] 1 4 20 2 40 0.2
        ..$ lower   : num [1:6] 0 0 0 0 0 ...
        ..$ upper   : num [1:6] Inf Inf Inf Inf Inf ...
        ..$ fixed   : logi [1:6] FALSE FALSE FALSE FALSE FALSE FALSE
        ..$ estimate: num [1:6] NA NA NA NA NA NA
       $ random_effects:List of 4
        ..$ omega :List of 2
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.cl"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. .. .. ..$ : chr "eta.cl"
        .. .. ..$ fixed : logi FALSE
        .. ..$ :List of 3
        .. .. ..$ names : chr "eta.v"
        .. .. ..$ matrix: num [1, 1] 0.09
        .. .. .. ..- attr(*, "dimnames")=List of 2
        .. .. .. .. ..$ : chr "eta.v"
        .. .. .. .. ..$ : chr "eta.v"
        .. .. ..$ fixed : logi FALSE
        ..$ sigma : list()
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
        ..$ parameters: Named chr [1:5] "cl" "v" "q" "vp" ...
        .. ..- attr(*, "names")= chr [1:5] "cl" "v" "q" "vp" ...
        ..$ advan     : int 4
        ..$ trans     : int 4
       $ dosing        : list()
       $ observations  :List of 1
        ..$ cp:List of 3
        .. ..$ prediction: language central/v
        .. ..$ residual  :List of 2
        .. .. ..$ type      : chr "proportional"
        .. .. ..$ parameters: chr "prop.sd"
        .. ..$ transform : chr "none"

# a lossy write prints what it lost

    Code
      print(w)
    Output
      $PROBLEM run1.ctl
      ; zufapmx: $INPUT and $DATA are placeholders; point them at your dataset.
      $INPUT ID TIME EVID AMT RATE CMT DV
      $DATA data.csv IGNORE=@
      $SUBROUTINES ADVAN4 TRANS4
      $PK
      KA = THETA(1)
      CL = THETA(2) * EXP(ETA(1))
      V2 = THETA(3) * EXP(ETA(2))
      Q = THETA(4)
      V3 = THETA(5)
      S2 = V2
      $ERROR
      Y = F * (1 + EPS(1))
      $THETA (0, 1.12) ; THETA1
      $THETA (0, 4.25) ; THETA2
      $THETA (0, 21.5) ; THETA3
      $THETA (0, 2.15) ; THETA4
      $THETA (0, 41.5) ; THETA5
      $OMEGA 0.084
      $OMEGA 0.096
      $SIGMA 0.038
      # nonmem, fidelity: lossy (lost: initial values: final estimates were written as $THETA, $OMEGA and $SIGMA)

