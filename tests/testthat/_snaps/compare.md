# a comparison prints its verdict, engines and worst point

    Code
      pmx_compare(m, pmx_write_rxode2(m), pmx_design(n = 1))
    Output
      <zupmxio comparison> FAIL
      a: nonmem model, written to mrgsolve by zupmxio, on mrgsolve 0.0.0
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
      <zupmxio comparison> pass
      a: nonmem model, written to rxode2 by zupmxio, on rxode2 0.0.0
      b: nonmem model, written to rxode2 by zupmxio, on rxode2 0.0.0
      Tolerance: relative 1e-06, absolute 1e-09.
      0 of 8 predictions disagree.
      Worst: single, subject 1, time 0.5: 0.5 against 0.5 (abs 0, rel 0).
       regimen points failed max_abs max_rel skipped
        single      8      0       0       0        

---

    Code
      pmx_compare(m, b, pmx_design(n = 0))
    Output
      <zupmxio comparison> FAIL
      a: nonmem model, written to rxode2 by zupmxio, on rxode2 0.0.0
      b: nonmem model, written to rxode2 by zupmxio, on rxode2 0.0.0
      Tolerance: relative 1e-06, absolute 1e-09.
      The models have 2 and 1 random effects.

# a comparison with tables prints and reports its source

    Code
      cmp
    Output
      <zupmxio comparison> FAIL
      a: NONMEM table output, column `PRED` (5 significant digits)
      b: nonmem model, written to rxode2 by zupmxio, on rxode2 0.0.0
      Tolerance: relative 1e-06, absolute 1e-09.
      2 of 2 predictions disagree.
      Worst: table, subject 1, time 1: 1 against 1.001 (abs 0.001, rel 0.000999).
       regimen points failed max_abs  max_rel skipped
         table      2      2   0.002 0.000999        
    Code
      writeLines(utils::head(pmx_report(cmp), 32))
    Output
      # Model comparison
      
      **Result: FAIL.** 2 of 2 predictions disagree.
      
      ## Models
      
      | Side | Simulated | Engine | Source |
      | --- | --- | --- | --- |
      | a | NONMEM table output, column `PRED` (5 significant digits) | none: NONMEM's own output | nonmem table, `sdtab1` |
      | b | nonmem model, written to rxode2 by zupmxio | rxode2 0.0.0 | nonmem, `advan1.ctl` |
      
      ## Design
      
      The tables' own records: 1 subject and 3 records. The model's typical subject, every random effect at zero, as NONMEM's `PRED`. Residual error is off.
      A point agrees when its relative deviation is at most 1e-06, its absolute deviation at most 1e-09, or the model's prediction rounds to the table's value.
      
      ## Results
      
      | Regimen | Points | Disagree | Largest absolute | Largest relative | Skipped |
      | --- | --- | --- | --- | --- | --- |
      | table | 2 | 2 | 0.002 | 0.000999 |  |
      
      Worst point: table, subject 1, time 1: 1 (a) against 1.001 (b), absolute deviation 0.001, relative 0.000999.
      
      ### Points that disagree
      
      | Regimen | Subject | Time | a | b | Absolute | Relative |
      | --- | --- | --- | --- | --- | --- | --- |
      | table | 1 | 1 | 1 | 1.001 | 0.001 | 0.000999 |
      | table | 1 | 2 | 2 | 2.002 | 0.002 | 0.000999 |
      
      ## Model a

