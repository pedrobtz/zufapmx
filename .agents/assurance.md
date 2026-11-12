# Assurance

How this package meets `.claude/shared/assurance.md`. CI checks this
file (r-actions `assurance.yml`), so keep the headings and table
columns as they are.

- **Statuses:** a cell is a fact, `gap #<issue>` or `n/a: <reason>`.
- **Kinds:** delete the ones that do not apply.
- **Tables:** delete a table only when no kind requires it. Delete the
  example rows.

## Kinds

`untrusted-input` `spec` `emits`

## Required checks

`Where` names the workflow file under `.github/workflows/` that runs the
check. Add `(own)` when that workflow is the package's own rather than a
call to r-actions.

| Check      | Status | Where |
| ---------- | ------ | ----- |
| fuzz       | gap #142 |       |
| fuzz-reach | gap #134 |       |

## Boundaries

One row per boundary the package crosses. An oracle knows the answer:

- a conformance suite or test vectors;
- a reference implementation;
- a round trip;
- equivalent inputs compared;
- the bytes the other side received.

"No crash" is not an oracle.

<!-- markdownlint-disable MD013 -->
| Boundary | Oracle | Carried by |
| -------- | ------ | ---------- |
| NM-TRAN control stream, read | nonmem2rx translates the same file and its predictions equal `expected.csv` | tests/testthat/test-corpus-oracles.R |
| NM-TRAN control stream, read | the model read, written to rxode2 and mrgsolve, simulates to `expected.csv` | tests/testthat/test-read-nonmem.R |
| NM-TRAN control stream, write | an independent reader (nonmem2rx) of the written text simulates to `expected.csv` | gap #135 |
| NM-TRAN control stream, write | round trip through the NONMEM reader, `exact` or `equivalent` | tests/testthat/test-write-nonmem.R |
| mrgsolve model file, read | mrgsolve simulates the source file to `expected.csv`, and the model read and written back simulates the same | tests/testthat/test-corpus-expected.R, tests/testthat/test-write-mrgsolve.R |
| mrgsolve model file, read | amp.sim translates the ODE entries' control streams for mrgsolve, and its predictions equal `expected.csv` | tests/testthat/test-corpus-oracles.R |
| mrgsolve model file, read | mrgsolve's own model library (`modlib()`): what reads and what is refused | gap #136 |
| mrgsolve model file, write | mrgsolve simulates the written text to `expected.csv`, from every source format | tests/testthat/test-write-mrgsolve.R |
| rxode2 model function, read | rxode2 simulates the source function to `expected.csv`, and the model read and written back simulates the same | tests/testthat/test-corpus-expected.R, tests/testthat/test-write-rxode2.R |
| rxode2 model function, read | the nlmixr2lib model library: at least 44 models read | tests/testthat/test-nlmixr2lib.R |
| rxode2 model function, write | rxode2 simulates the written text to `expected.csv`, from every source format | tests/testthat/test-write-rxode2.R |
| model across formats | every corpus model round-trips X → Y → X and passes `pmx_compare()` on independent engines | tests/testthat/test-compare.R |
| model across formats | random closed-form models round-trip through every format | tests/testthat/test-property.R |
| NONMEM estimates (`.ext`, `.lst`, `run.xml`), read | the three files of one run give the same estimates | tests/testthat/test-nonmem-estimates.R |
| NONMEM estimates (`.ext`, `.lst`, `run.xml`), read | real NONMEM 7 output files | gap #137 |
| NONMEM `$TABLE` output, read | tables printed from `expected.csv` in NONMEM's layout compare with the model; zucsv and base R read the same table | tests/testthat/test-tables.R, tests/testthat/test-compare.R |
| NONMEM `$TABLE` output, read | real NONMEM table output, Fortran exponents included | gap #138 |
| design records, sent to rxode2 and mrgsolve | both engines' predictions equal the closed forms on every regimen and subject, with independently drawn etas | tests/testthat/test-compare.R |
| JSON document, write and read | round trip `identical()` on the corpus, random models and non-ASCII names in a C locale | tests/testthat/test-json.R, tests/testthat/test-property.R |
| Markdown report, write | a Markdown parser reads the report's tables with the expected cells | gap #139 |
<!-- markdownlint-enable MD013 -->

## Limits

One row per limit the package enforces. "Straddled by" is the test of
`limit - 1`, `limit` and `limit + 1` on every path that reaches it.

<!-- markdownlint-disable MD013 -->
| Limit | Unit | Straddled by |
| ----- | ---- | ------------ |
| `pmx_design(n)` at most 10000 | subjects | tests/testthat/test-design.R |
| `pmx_design(cmt)` from 1 to 1000 | compartment position | tests/testthat/test-design.R |
| `pmx_design(seed)` within the integer range | whole number | tests/testthat/test-design.R |
| `pmx_report(max_points)` from 0 to the integer range | points | tests/testthat/test-report.R |
| `tol` positive, `atol` non-negative | relative, absolute deviation | tests/testthat/test-compare.R |
| a point agrees at relative deviation `tol` or absolute `atol` | relative, absolute deviation | tests/testthat/test-compare.R |
| a table value agrees within half a unit in its last printed digit | that digit | tests/testthat/test-compare.R |
| `same_content()` tolerates 4 units in the last place | ulps | gap #140 |
| JSON counts (`version`, `ncmt`, `advan`, `trans`) within the integer range | whole number | gap #141 |
<!-- markdownlint-enable MD013 -->
