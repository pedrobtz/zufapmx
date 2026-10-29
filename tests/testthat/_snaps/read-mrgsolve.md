# closed-form and ODE models read into their structures

    Code
      print(closed)
    Output
      <zufapmx_model> read from mrgsolve
      Structure: two-compartment closed form, first-order input (ADVAN4 TRANS4)
      Parameters: 5, initial values only
      Random effects: ETA1 on CL (exp), ETA2 on V2 (exp)
      Endpoints: Y (proportional)

---

    Code
      print(ode)
    Output
      <zufapmx_model> read from mrgsolve
      Structure: ODE system over 2 compartments: DEPOT, CENT
      Parameters: 4, initial values only
      Random effects: ETA1 on VM (exp), ETA2 on V (exp)
      Endpoints: Y (proportional)

