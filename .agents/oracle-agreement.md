# Oracle agreement

Generated 2026-10-28 by `tools/oracle-agreement.R`.

Each cell compares the typical-subject predictions of one translation
of the entry's `model.ctl` with `expected.csv` (rxode2 on
`model-rxode2.R`). The last column simulates the entry's own `model.cpp`.
A cell agrees when the largest relative deviation is at most 1e-06;
the number after it is that deviation. `absent`: the converter is
not installed.

Versions:

- rxode2 5.1.8
- mrgsolve 2.0.1
- nonmem2rx 0.1.12
- amp.sim 0.1.1
- nonmem2mrgsolve not installed

| Entry | nonmem2rx | amp.sim | nonmem2mrgsolve | model.cpp |
| --- | --- | --- | --- | --- |
| onecmt-oral | agrees 4e-10 | error [1] | absent | agrees 4e-10 |
| onecmt-oral-mm | agrees 4e-10 | agrees 4e-10 | absent | agrees 4e-10 |
| threecmt-infusion | agrees 3e-10 | error [1] | absent | agrees 3e-10 |
| twocmt-bolus | agrees 2e-10 | error [1] | absent | agrees 2e-10 |
| twocmt-oral | agrees 4e-10 | error [1] | absent | agrees 4e-10 |

Errors:

1. all records must have cmt set to zero.

<!-- notes: kept when this file is regenerated -->

## Findings (2026-10-28)

- **nonmem2rx agrees on every entry**, closed forms and ODE alike, to
  about 4e-10. It is the strong oracle for Stage 7, as the design expects.
- **The corpus's own `model.cpp` files agree on every entry**, so the
  rxode2 and mrgsolve forms of each entry are the same model.
- **amp.sim covers general ODE models only.** It writes `$CMT` from
  `$MODEL` and has nothing to write for a closed-form `ADVAN`, so every
  `ADVAN2`/`3`/`4`/`11` entry fails with "all records must have cmt set to
  zero". It also reads only the first `COMP=` when two share a `$MODEL`
  line, which NM-TRAN allows; `onecmt-oral-mm` keeps one `COMP` per line
  so amp.sim can be checked against it, and then agrees. It does not
  capture `IPRED` unless asked (`mod_return = "IPRED"`). As the
  mrgsolve-side oracle (D12) it therefore checks ODE entries only, and the
  closed-form mrgsolve path rests on the corpus's `model.cpp` files and on
  round trips.
- **mrgsolve `$PKMODEL` needs `trans` spelled out.** With `ncmt = 2` and no
  depot, the default `trans` is not 4, and `CL`/`V1`/`Q`/`V2` fail with
  "pred_VC has a 0 or negative value". The corpus writes every closed form
  as `advan = n, trans = m`, mirroring its control stream. mrgsolve 2.0
  has a three-compartment closed form (`advan = 11`), which the design did
  not assume.
- **Compile one mrgsolve model per name.** Two models built under the same
  name in one R session collide, and the second silently runs the first
  one's code. The script and, later, the Stage 7 engine must give each
  compile its own name.
- **nonmem2mrgsolve** was not installed: installing it from GitHub needs
  the user's go-ahead. Its column fills when it is.
