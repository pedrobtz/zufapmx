# zufuzz harness for the rxode2 reader (design §9). Run from the package root
# with zupmxio installed:
#
#   Rscript tools/fuzz/rxode2.R <inputs...>           # replay inputs, no engine
#   afl-fuzz -i tools/fuzz/corpus/rxode2 -o out -- Rscript tools/fuzz/rxode2.R
#
# or from R: zufuzz::fuzz_file("tools/fuzz/rxode2.R",
#   corpus = "tools/fuzz/corpus/rxode2", time_limit = 60).
# The oracle is fuzz_target() in tests/testthat/helper-fuzz.R, which
# tests/testthat/test-fuzz.R also replays on every check.

library(zufuzz)
source("tests/testthat/helper-fuzz.R")
instrument_package("zupmxio")

test_one_input <- function(data) fuzz_target(data, "rxode2")

fuzz(test_one_input, engine = "auto")
