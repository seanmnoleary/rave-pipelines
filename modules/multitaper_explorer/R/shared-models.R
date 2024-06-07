MODEL_ROOT_PATH <- normalizePath(".")

load_model <- function(name) {
  stopifnot(length(name) == 1 && !is.na(name) && nzchar(name))
  model_path <- file.path(MODEL_ROOT_PATH, "models", name)
  if(!isTRUE(file.exists(model_path))) {
    stop("Cannot find model: ", model_path)
  }
  return(model_path)
}
