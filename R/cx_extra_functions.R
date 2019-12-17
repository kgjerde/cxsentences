#' @export
cx_extra_subset <- function(input_field, df) {
  magic_input <- input_field %>%
    stringr::str_split("--") %>%
    unlist()

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w
    )

    if (length(more_patterns) > 0) {
        for (i in seq_along(more_patterns)) {
            filter_sentence_window(more_patterns[i])
        }
    }

    filtering_indices <- get_filtered_doc_indices_from_py()

    df <-
      df[df$ID %in%
        filtering_indices, ]

  return(df)
}

#' @export
cx_extra_tab_text <- function() {
    get_filtered_sentences_from_py()
}