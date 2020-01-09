#' Initiate Python environment
#'
#' @param virtenv Path to virtenv with required modules
#'
#' @return
#' @export
cx_setup <-
  function(virtenv = "/Users/Kristian/.local/share/virtualenvs/apekatt-mr9vg8U1/") {
    reticulate::use_virtualenv(virtenv, required = TRUE)

    path_to_py_functions <- system.file("python", package = "cxsentences")

    reticulate::py_run_file(paste0(
        path_to_py_functions,
        "/sentence_functions.py"))

    reticulate::py_run_file(paste0(
        path_to_py_functions,
        "/tokenization_ru.py"))

    reticulate::py_run_file(paste0(
        path_to_py_functions,
        "/tokenization_no.py"))

    reticulate::py_run_file(paste0(
        path_to_py_functions,
        "/tokenization_en.py"))
  }
