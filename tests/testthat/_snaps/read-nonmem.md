# closed-form and ODE control streams read into their structures

    Code
      print(closed)
    Output
      <zufapmx_model> read from nonmem
      Structure: two-compartment closed form, first-order input (ADVAN4 TRANS4)
      Parameters: 5, initial values only
      Random effects: ETA1 on CL (exp), ETA2 on V2 (exp)
      Endpoints: Y (proportional)

---

    Code
      print(ode)
    Output
      <zufapmx_model> read from nonmem
      Structure: ODE system over 2 compartments: DEPOT, CENTRAL
      Parameters: 4, initial values only
      Random effects: ETA1 on VM (exp), ETA2 on V (exp)
      Endpoints: Y (proportional)

---

    Code
      unclass(ode)$provenance$losses
    Output
      [1] "comments"                                      
      [2] "title ($PROBLEM)"                              
      [3] "data records ($INPUT, $DATA)"                  
      [4] "estimation settings ($ESTIMATION, $COVARIANCE)"
      [5] "ODE solver (ADVAN13)"                          

