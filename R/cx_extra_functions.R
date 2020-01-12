#' @export
cx_extra_reset_data <- function() {
  reticulate::py_run_string("filtered_indices = []")
  reticulate::py_run_string("charting_indices = []")
  reticulate::py_run_string("doc_indices = []")
}

cx_collect_terms <- function(input_field, case_sensitive) {
  magic_input <- stringr::str_remove_all(input_field, "\n.*")
  magic_input <- stringr::str_split(magic_input, "--")
  magic_input <- unlist(magic_input)

  if (case_sensitive == FALSE) {
    magic_input <- stringr::str_to_lower(magic_input)
  }
  return(magic_input)
}

#' @export
cx_extra_subset <- function(input_field, df, case_sensitive, indices_included = NULL) {

  magic_input <- cx_collect_terms(input_field, case_sensitive)

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive,
      indices_included
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
cx_extra_chart <- function(input_field, df_dok, df_modus, case_sensitive, modus, indices_included = NULL) {

  magic_input <- cx_collect_terms(input_field, case_sensitive)

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive,
      indices_included,
      py_var_name = "charting_indices"
    )

    if (length(more_patterns) > 0) {
        for (i in seq_along(more_patterns)) {
            filter_sentence_window(more_patterns[i],
                                   case_sensitive,
                                   py_var_name = "charting_indices")
        }
    }

  }

  count_overview <- get_number_of_sentences_per_doc()

  # If zero hits:
  if (is.null(count_overview)) {
      return(data.frame(Term_1 = rep(0, nrow(df_modus))))
  }

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
cx_extra_tab_text <- function(df, min_rad, patterns) {
  index <- df$ID[min_rad]
  text <- ""
  if (!identical(patterns, "")) {
    for (pattern in patterns) {
      cx_indices_one_doc(pattern, FALSE, index)
    print(reticulate::py_eval("len(doc_indices[0][1])"))
      text <-
        paste0(
          text,
          "\n\n\n<b>",
          pattern,
          "</b>\n\n\n",
          get_filtered_sentences_from_one_doc_py()
        )
      text <- stringr::str_trim(text)
    }
  }
  return(text)
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

#' @export
cx_shiny_validate <- function(extra_patterns) {
    shiny::validate(shiny::need(

    cx_validate_input(extra_patterns) |
      identical(extra_patterns, "")
    ,
    paste("\nInvalid pattern in extra chart field.")
))
}

#' @export
cx_indices_one_doc <- function(input_field, case_sensitive, doc_index) {

  index <- as.integer(doc_index - 1)

  magic_input <- cx_collect_terms(input_field, case_sensitive)

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive,
      indices_included = index,
      py_var_name = "doc_indices"
    )

    if (length(more_patterns) > 0) {
        for (i in seq_along(more_patterns)) {
            filter_sentence_window(more_patterns[i],
                                   case_sensitive,
                                   py_var_name = "doc_indices")
        }
    }

  }

}

#' @export
cx_extra_create_df_for_info <- function(df, terms, case_sensitive) {
  linjer <- length(terms)
  count_overview <- data.frame(dummy = seq_len(nrow(df)))

  for (i in seq_len(linjer)) {
    count_overview[i] <- cx_extra_chart(terms[i],
      df,
      df,
      case_sensitive,
      "data_dok",
      indices_included = df$ID - 1
    )
  }

  colnames(count_overview) <- terms

  return(list(
    start_df = count_overview,
    full_terms = terms,
    tresh = rep(NA, length(terms)),
    cust_col = rep(NA, length(terms)),
    terms = terms,
    years = df$Year
  ))
}
