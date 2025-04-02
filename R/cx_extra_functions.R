#' Actions to be carried out when search button is clicked
#'
#' @export
cx_extra_reset_data <- function() {
  reticulate::py_run_string("filtered_indices = []")
  reticulate::py_run_string("charting_indices = []")
  reticulate::py_run_string("doc_indices = []")
}

#' Parse one input phrase into something useful for further processing
#'
#' @param input_field One pattern collected from input field
#' @param case_sensitive search_arguments$case_sensitive
#'
#' @return Something useful, e.g. parsed phrase
#'   "ost--0" to c("ost", "0")
#' @export
cx_parse_terms <- function(extra_input_phrase, case_sensitive) {
  magic_input <- stringr::str_remove_all(extra_input_phrase, "\n.*")
  magic_input <- stringr::str_split(magic_input, "--")
  magic_input <- unlist(magic_input)

  if (case_sensitive == FALSE) {
    magic_input <- stringr::str_to_lower(magic_input)
  }

  ## "!" first in pattern converts pattern to match
  # sentences that do _not_ match pattern:
  # https://stackoverflow.com/questions/1240275/how-to-negate-specific-word-in-regex
  # Also: "." match also newlines: item "(?aiLmsux)" at
  # https://docs.python.org/3/library/re.html

  magic_input <- stringr::str_replace(magic_input,
                                      "^!(.*)$",
                                      "(?s)^(?!.*\\1).*$")

  return(magic_input)
}


#' Subset corpus
#'
#' Filter corpus based on one "extra subset field" phrase
#'
#' @param extra_input_phrase phrase from extra subset field
#' @param df data_dok corporaexplorer data frame
#' @param case_sensitive search_arguments$case_sensitive
#' @param indices_included R data_dok IDs included from potential filtering before this function
#'   is called
#'
#' @return filtered data_dok corporaexplorer data frame
#' @export
cx_extra_subset <- function(extra_input_phrase, df, case_sensitive, indices_included = NULL) {

  magic_input <- cx_parse_terms(extra_input_phrase, case_sensitive)

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive,
      indices_included - 1  # translation from R to Python indexing
    )

    if (length(more_patterns) > 0) {
        for (i in seq_along(more_patterns)) {
            filter_sentence_window(more_patterns[i],
                                   case_sensitive)
        }
    }

    filtering_indices <- get_filtered_doc_indices_from_py()

    df <-
      df[df$cx_ID %in%
        filtering_indices, ]
  }

  return(df)
}


#' Count hits/chart
#'
#' Chart hits on corpus based on one "extra chart field" phrase
#'
#' @param extra_input_phrase phrase from extra chart field
#' @param df_dok data_dok df
#' @param df_modus data_dok or data_365 df
#' @param case_sensitive search_arguments$case_sensitive
#' @param modus plot_mode
#' @param indices_included R data_dok IDs included from potential filtering before this function
#'   is called
#'
#' @return base R data frame with nrow() == nrow(df_modus) and 1 column with hits per row
#' @export
cx_extra_chart <- function(extra_input_phrase, df_dok, df_modus, case_sensitive, modus, indices_included = NULL) {

  magic_input <- cx_parse_terms(extra_input_phrase, case_sensitive)

  if (length(magic_input) >= 2) {

    main_filter_pattern <- magic_input[1]
    w <- magic_input[2]

    more_patterns <- magic_input[-(c(1,2))]

    get_sentence_window_indices(
      filter_pattern = main_filter_pattern,
      window = w,
      case_sensitive,
      indices_included - 1,  # translation from R to Python indexing
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

    count_overview$Date <- plyr::mapvalues(count_overview$cx_ID, df_dok$cx_ID, as.character(df_dok$Date), warn_missing = FALSE) %>%
      as.Date()
    count_overview <- count_overview %>%
      dplyr::group_by(Date) %>%
      dplyr::summarise(Term_1 = sum(Term_1))
    count_overview <- dplyr::left_join(df_modus, count_overview, by = "Date")
    count_overview[["Term_1"]][is.na(count_overview[["Term_1"]])] <- 0
    count_overview <- dplyr::select(count_overview, Term_1)
    count_overview <- as.data.frame(count_overview)

  } else {
      count_overview <- dplyr::filter(count_overview, cx_ID %in% df_modus$cx_ID)
      if (nrow(count_overview) != nrow(df_modus)) {
        day_doc_without_hits <- df_modus$cx_ID[!(df_modus$cx_ID %in% count_overview$cx_ID)]
        day_doc_without_hits <- data.frame(cx_ID = day_doc_without_hits, Term_1 = 0)
        count_overview <- rbind(count_overview, day_doc_without_hits) %>%
          dplyr::arrange(cx_ID)
      }
      count_overview <- dplyr::select(count_overview, Term_1)
      count_overview <- as.data.frame(count_overview)
  }

  return(count_overview)

}


#' Text to be displayed in "extra tab" in document box
#'
#' @param df corporaexplorer df (data_dok or data_365)
#' @param min_rad row in df
#' @param patterns patternS from extra chart input field
#'
#' @return Text to be displayed
#' @export
cx_extra_tab_text <- function(df, min_rad, patterns) {
  index <- df$cx_ID[min_rad]
  text <- ""
  if (!identical(patterns, "")) {
    for (pattern in patterns) {
      cx_indices_one_doc(pattern, FALSE, index)
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


#' Validate input phrases
#'
#' @param input_field character vector of "extra input phrases"
#'
#' @return Boolean. TRUE if all phrases are OK.
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
#' "shiny::validate(shiny::need(" wrapper for cx_validate_input()
#'
#' @param extra_patterns character vector of "extra input phrases"
#'
#' @export
cx_shiny_validate <- function(extra_patterns) {
    shiny::validate(shiny::need(

    cx_validate_input(extra_patterns) |
      identical(extra_patterns, "")
    ,
    paste("\nInvalid pattern in extra chart field.")
))
}


#' Generate identified chunk indices for one doc, one phrase
#'
#' Internal. And works in the Python process
cx_indices_one_doc <- function(input_field, case_sensitive, doc_index) {

  index <- as.integer(doc_index - 1)

  magic_input <- cx_parse_terms(input_field, case_sensitive)

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


#' "Extra" equivalent to create_df_for_info() in corporaexplorer
#'
#' @param df session_variables$data_dok,
#' @param terms search_arguments$extra_chart_terms
#' @param case_sensitive search_arguments$case_sensitive
#'
#' @return A peculiar list to be used to create table and
#'   plot in corpus info tab
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
      indices_included = df$cx_ID
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
