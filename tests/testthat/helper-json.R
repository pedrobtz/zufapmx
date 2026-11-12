# A model written to a JSON file and read back.
json_round_trip <- function(model, env = parent.frame()) {
  file <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  pmx_write_json(model, file)
  pmx_read_json(file)
}
