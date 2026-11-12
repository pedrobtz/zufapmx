# a design prints its regimens, subjects and covariates

    Code
      pmx_design()
    Output
      <zufapmx design> single, infusion, multiple, ss, dosing compartment 1
      The typical subject and 20 with random effects (seed 1).
    Code
      pmx_design(n = 3, seed = 7, regimens = "ss", covariates = list(WT = 70))
    Output
      <zufapmx design> ss, dosing compartment 1
      The typical subject and 3 with random effects (seed 7).
      Covariates: WT = 70.

