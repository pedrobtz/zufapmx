# Copy the model corpus into tools/fuzz/corpus/<format>/ as fuzz seeds.
# Run from the package root: Rscript tools/fuzz/seed-corpus.R
files <- c(
  nonmem = "model.ctl",
  mrgsolve = "model.cpp",
  rxode2 = "model-rxode2.R"
)
for (format in names(files)) {
  dir <- file.path("tools", "fuzz", "corpus", format)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (entry in list.dirs("inst/corpus", recursive = FALSE)) {
    file.copy(
      file.path(entry, files[[format]]),
      file.path(dir, paste0(basename(entry), "-", files[[format]])),
      overwrite = TRUE
    )
  }
}
