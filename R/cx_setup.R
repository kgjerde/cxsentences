#' Initiate Python environment
#'
#' @param virtenv Path to virtenv with required modules
#'
#' @return
#' @export
cx_setup <-
  function(virtenv = "/Users/Kristian/.local/share/virtualenvs/apekatt-mr9vg8U1/") {
    reticulate::use_virtualenv(virtenv, required = TRUE)

    path_to_py_functions <- system.file("python", "sentence_functions.py", package = "cxsentences")
    reticulate::py_run_file(path_to_py_functions)
  }
