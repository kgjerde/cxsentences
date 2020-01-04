#' @export
cx_extra_reset_data <- function() {
  reticulate::py_run_string("filtering_indices = list(range(len(sentences)))")
}

#' @export
cx_extra_subset <- function(input_field, df, case_sensitive) {
  magic_input <- input_field %>%
    stringr::str_remove_all("\n.*") %>%
    stringr::str_split("--") %>%
    unlist()

  if (case_sensitive == FALSE) {
      magic_input <- stringr::str_to_lower(magic_input)
  }

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive
    )

    if (length(more_patterns) > 0) {
        for (i in seq_along(more_patterns)) {
            filter_sentence_window(more_patterns[i],
                                   case_sensitive)
        }
    }

    filtering_indices <- get_filtered_doc_indices_from_py()

    df <-
      df[df$ID %in%
        filtering_indices, ]
  }

  return(df)
}

#' @export
cx_extra_chart <- function(input_field, df_dok, df_modus, case_sensitive, modus) {
  magic_input <- input_field %>%
    stringr::str_remove_all("\n.*") %>%
    stringr::str_split("--") %>%
    unlist()

  if (case_sensitive == FALSE) {
      magic_input <- stringr::str_to_lower(magic_input)
  }

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive
    )

    if (length(more_patterns) > 0) {
        for (i in seq_along(more_patterns)) {
            filter_sentence_window(more_patterns[i],
                                   case_sensitive)
        }
    }

    # get_filtered_doc_indices_from_py()

  }

  count_overview <- get_number_of_sentences_per_doc()

  if (modus == "data_365") {

    count_overview$Date <- plyr::mapvalues(count_overview$ID, df_dok$ID, as.character(df_dok$Date), warn_missing = FALSE) %>%
      as.Date()
    count_overview <- count_overview %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(Term_1 = sum(Term_1))
    count_overview <- dplyr::left_join(df_modus, count_overview, by = "Date")
    count_overview[["Term_1"]][is.na(count_overview[["Term_1"]])] <- 0
    count_overview <- dplyr::select(count_overview, Term_1)
    count_overview <- as.data.frame(count_overview)

  } else {
      count_overview <- dplyr::filter(count_overview, ID %in% df_modus$ID)
      if (nrow(count_overview) != nrow(df_modus)) {
        day_doc_without_hits <- df_modus$ID[!(df_modus$ID %in% count_overview$ID)]
        day_doc_without_hits <- data.frame(ID = day_doc_without_hits, Term_1 = 0)
        count_overview <- rbind(count_overview, day_doc_without_hits) %>%
          dplyr::arrange(ID)
      }
      count_overview <- dplyr::select(count_overview, Term_1)
      count_overview <- as.data.frame(count_overview)
  }

  return(count_overview)

}

#' @export
cx_extra_tab_text <- function(doc_ID) {
    get_filtered_sentences_from_one_doc_py(doc_ID)
}

#' @export
cx_extra_replace_main_text <- function(input_field, df) {
  if (stringr::str_detect(input_field, "\nZOOM")) {
      df$Text_original_case <- get_filtered_sentences_from_py()
      df$Text <- df$Text_original_case %>%
        stringr::str_to_lower()
  }
  return(df)
}

#' @export
cx_validate_input <- function(input_field) {

  validation_regex <- "^[^-]+--\\d+(--[^-]+)*$"

  if (all(stringr::str_detect(input_field, validation_regex)) == FALSE) {
    return(FALSE)
  }

  dissected_pattern <- input_field %>%
    stringr::str_split("--") %>%
    unlist()

  # from check_regexes() from corporaexplorer explorer app:
  tryCatch(
    is.integer(stringr::str_count("esel", dissected_pattern)),
    error = function(e) {
      FALSE
    }
  )

}