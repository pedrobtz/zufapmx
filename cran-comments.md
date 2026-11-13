## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environments

`R CMD check --as-cran` on GitHub Actions
(pedrobtz/r-actions `r-cmd-check.yml`):

* macOS (latest), R release
* Windows (latest), R release and R-devel
* Ubuntu (latest), R release and R oldrel-1
* R-devel containers from r-hub: clang 23, Ubuntu clang, Ubuntu gcc 16
* R-devel without long double (`nold`)
* R release without Suggests installed (`_R_CHECK_FORCE_SUGGESTS_=false`)

## Notes for the reviewer

* The package has no hard dependencies. The simulation engines (rxode2,
  mrgsolve), the converters it is checked against (nonmem2rx, amp.sim) and
  the readers of optional files (zuxml, zucsv, zujson) are in Suggests, and
  every use is guarded. Tests that need an engine skip on CRAN.
* The examples of `pmx_compare()` and `pmx_report()` compile one rxode2
  model, which takes about 3 seconds; they run only when rxode2 is
  installed.

## Reverse dependencies

None: this is a new package.
