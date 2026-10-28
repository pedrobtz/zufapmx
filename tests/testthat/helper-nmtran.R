# Parse lines of abbreviated code as one $PK record of a two-compartment
# model.
nm_parse_lines <- function(
  lines,
  cmt = function(n, col) c("DEPOT", "CENTRAL")[[n]]
) {
  record <- list(name = "PK", lines = lines, line = seq_along(lines))
  nm_code(record, "test.ctl", cmt)
}
