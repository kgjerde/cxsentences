# To comply with R CMD check
# See https://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
utils::globalVariables(
  c(
      "Date",
      "Term_1",
      "cx_ID",
      "explore0",
      "get_nested_indices"
  )
)

.onLoad <- function(libname, pkgname) {
    cx_setup()
}