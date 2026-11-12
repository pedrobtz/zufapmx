# a comparison prints its verdict, engines and worst point

    Code
      pmx_compare(m, pmx_write_rxode2(m), pmx_design(n = 1))
    Output
      <zufapmx comparison> FAIL
      a: nonmem model, written to mrgsolve by zufapmx, on mrgsolve 0.0.0
      b: rxode2 text, simulated as written, on rxode2 0.0.0
      Tolerance: relative 1e-06, absolute 1e-09.
      70 of 70 predictions disagree.
      Worst: multiple, subject 1, time 50: 50.05 against 50 (abs 0.05, rel 0.000999).
        regimen points failed max_abs  max_rel skipped
         single     16     16  0.0240 0.000999        
       infusion     16     16  0.0240 0.000999        
       multiple     24     24  0.0720 0.000999        
             ss     14     14  0.0115 0.000999        
    Code
      pmx_compare(m, m, pmx_design(n = 0, regimens = "single"))
    Output
      <zufapmx comparison> pass
      a: nonmem model, written to rxode2 by zufapmx, on rxode2 0.0.0
      b: nonmem model, written to rxode2 by zufapmx, on rxode2 0.0.0
      Tolerance: relative 1e-06, absolute 1e-09.
      0 of 8 predictions disagree.
      Worst: single, subject 1, time 0.5: 0.5 against 0.5 (abs 0, rel 0).
       regimen points failed max_abs max_rel skipped
        single      8      0       0       0        

---

    Code
      pmx_compare(m, b, pmx_design(n = 0))
    Output
      <zufapmx comparison> FAIL
      a: nonmem model, written to rxode2 by zufapmx, on rxode2 0.0.0
      b: nonmem model, written to rxode2 by zufapmx, on rxode2 0.0.0
      Tolerance: relative 1e-06, absolute 1e-09.
      The models have 2 and 1 random effects.

