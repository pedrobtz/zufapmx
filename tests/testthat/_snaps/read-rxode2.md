# a closed-form corpus model reads into its structure

    Code
      print(m)
    Output
      <zupkpmx_model> read from rxode2
      Structure: two-compartment closed form, first-order input (ADVAN4 TRANS4)
      Parameters: 6, initial values only
      Random effects: eta.cl on cl (exp), eta.v on v (exp)
      Endpoints: cp (proportional)

# an ODE corpus model reads into states, statements and derivatives

    Code
      print(m)
    Output
      <zupkpmx_model> read from rxode2
      Structure: ODE system over 2 compartments: depot, central
      Parameters: 5, initial values only
      Random effects: eta.vm on vm (exp), eta.v on v (exp)
      Endpoints: cp (proportional)

