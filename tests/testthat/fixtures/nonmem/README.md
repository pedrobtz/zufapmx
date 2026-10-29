# NONMEM output fixtures

Hand-written for zupkpmx's tests in October 2026; no NONMEM run produced
them. They imitate the output formats of NONMEM 7.5 closely enough to
exercise the readers in `R/nonmem-estimates.R`, with values chosen so that
each source and each matrix element can be told apart.

- `run1.ctl`: the `twocmt-oral` corpus control stream. Its `.ext`, `.lst` and
  `.xml` give different final estimates (1.12..., 1.14..., 1.13...), so a
  test can tell which file was read. Omega and sigma are diagonal.
- `run2.ctl`: a one-compartment model with a `BLOCK(3)` omega and two
  sigmas. Its three output files agree, and every off-diagonal element is
  distinct, so the orientation of the lower triangle is pinned.

Deliberately left out:

- more than one `$ESTIMATION` step (one `TABLE NO.` block in each `.ext`, one
  `<nm:estimation>` in each `.xml`);
- most of a real `.lst`: only the final-estimate section the reader looks
  for, and the line that ends it;
- standard errors beyond `run1.ext`'s `-1000000001` row, which the reader
  must skip.
