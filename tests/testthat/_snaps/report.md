# a report reads as it did

    Code
      writeLines(pmx_report(cmp, max_points = 2))
    Output
      # Model comparison
      
      **Result: FAIL.** 15 of 15 predictions disagree.
      
      ## Models
      
      | Side | Simulated | Engine | Source |
      | --- | --- | --- | --- |
      | a | nonmem model, written to mrgsolve by zufapmx | mrgsolve 0.0.0 | nonmem, `advan1.ctl` |
      | b | rxode2 text, simulated as written | rxode2 0.0.0 | written rxode2, fidelity equivalent |
      
      ## Design
      
      Regimens single, ss, each a dose of 100 into compartment 1.
      The typical subject (1) and 0 subjects with random effects drawn with seed 1. Residual error is off.
      A point agrees when its relative deviation is at most 1e-06 or its absolute deviation at most 1e-09.
      
      ## Results
      
      | Regimen | Points | Disagree | Largest absolute | Largest relative | Skipped |
      | --- | --- | --- | --- | --- | --- |
      | single | 8 | 8 | 0.024 | 0.000999 |  |
      | ss | 7 | 7 | 0.0115 | 0.000999 |  |
      
      Worst point: single, subject 1, time 0.5: 0.5005 (a) against 0.5 (b), absolute deviation 0.0005, relative 0.000999.
      
      ### Points that disagree
      
      | Regimen | Subject | Time | a | b | Absolute | Relative |
      | --- | --- | --- | --- | --- | --- | --- |
      | single | 1 | 0.5 | 0.5005 | 0.5 | 0.0005 | 0.000999 |
      | single | 1 | 1 | 1.001 | 1 | 0.001 | 0.000999 |
      
      And 13 more.
      
      ## Model a
      
      ```cpp
      $PROB advan1.ctl
      
      $PARAM @annotated
      THETA1 : 4 : [0, Inf]
      THETA2 : 50 : [0, Inf]
      
      $PKMODEL advan = 1, trans = 2, cmt = "CENT"
      
      $MAIN
      double CL = THETA1 * exp(ETA(1));
      double V = THETA2 * exp(ETA(2));
      
      $OMEGA
      0.09
      
      $OMEGA
      0.04
      
      $SIGMA
      0.04
      
      $TABLE
      double IPRED = CENT / V;
      double Y = IPRED * (1 + EPS(1));
      
      $CAPTURE IPRED Y
      ```
      
      ## Model b
      
      ```r
      function() {
        ini({
          THETA1 <- c(0, 4)
          THETA2 <- c(0, 50)
          EPS1 <- c(0, 0.2)
          ETA1 ~ 0.09
          ETA2 ~ 0.04
        })
        model({
          CL <- THETA1 * exp(ETA1)
          V <- THETA2 * exp(ETA2)
          Y <- linCmt()
          Y ~ prop(EPS1)
        })
      }
      ```

