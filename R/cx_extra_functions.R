#' @export
cx_extra_subset <- function(input_field, df) {
  magic_input <- input_field %>%
    stringr::str_split("--") %>%
    unlist()

  if (length(magic_input) == 3) {
    p1 <- magic_input[1]
    p2 <- magic_input[2]
    w <- magic_input[3]

    get_nested_indices(
      filter_pattern = p1,
      search_pattern = p2,
      window = w
    )

    filtering_indices <- get_filtered_doc_indices_from_py()

    df <-
      df[df$ID %in%
        filtering_indices, ]
  }
  return(df)
}

#' @export
cx_extra_tab_text <- function() {
    get_filtered_sentences_from_py()
}