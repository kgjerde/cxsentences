#' Tokenize character vector to a list of lists of sentences
#'
#' IMPORTANT: must be run first in order to create
#'   internal Python object `sentences`
#'   and `sentences_lower`
#'   to be accessed by `get_sentence_filtered_doc_indices()`
#'   (and thereby also by `create_sentence_filtered_df()`
#'   and `explore_sentence_filtered_df()`).
#'
#' @param df R data frame or corporaexplorerobject
#' @param to_lower convert the final list of lists to lower case?
#'
#' @return internal Python object `sentences`
#'   and `sentences_lower`.
#' @export
tokenize_sentences_ru <- function(df, to_lower = TRUE) {
    if (identical(class(df), "corporaexplorerobject")) {
      df <- corporaexplorer:::get_df(df)
    }
    py$Text <- df$Text
    reticulate::py_run_string("sentences = tokenize_sentences_ru(Text)")

    if (to_lower == TRUE) {
        reticulate::py_run_string("sentences_lower = sentences_to_lower(sentences)")
    }
}

#' @export
tokenize_sentences_no <- function(df, to_lower = TRUE) {
    if (identical(class(df), "corporaexplorerobject")) {
      df <- corporaexplorer:::get_df(df)
    }
    py$Text <- df$Text
    reticulate::py_run_string("sentences = tokenize_sentences_no(Text)")

    if (to_lower == TRUE) {
        reticulate::py_run_string("sentences_lower = sentences_to_lower(sentences)")
    }
}

#' @export
tokenize_sentences_en <- function(df, to_lower = TRUE) {
    if (identical(class(df), "corporaexplorerobject")) {
      df <- corporaexplorer:::get_df(df)
    }
    py$Text <- df$Text
    reticulate::py_run_string("sentences = tokenize_sentences_en(Text)")

    if (to_lower == TRUE) {
        reticulate::py_run_string("sentences_lower = sentences_to_lower(sentences)")
    }
}

#' Wrapper for convenient argument passing
#'
#' Important: "silent" argument: `sentences` in the Python process,
#'   created by `tokenize_sentences_wrapper()`
#'
#' @param filter_pattern Chr. around which pattern extract sentences?
#' @param search_pattern Which pattern to search within extracted sentences?
#' @param r_indexing Add 1 to python zero indexing to achieve equivalent
#'   indexing in R?
#' @param window How many sentences to each side of `filter_pattern` to extract?
#'
#' @return R integer vector with (Python zero style) indices for the
#'   documents that have `search_pattern` in `window` sentence proximity
#'     to `filter_pattern`.
#' @export
get_sentence_window_indices <-
  function(filter_pattern,
           window,
           case_sensitive = FALSE,
           indices_included = NULL,
           py_var_name = "filtered_indices") {
    py$filter_pattern <- filter_pattern
    py$window <- as.integer(window)
    py$indices_included <- indices_included

    if (case_sensitive == FALSE) {
      reticulate::py_run_string(sprintf("

%s = sentence_windows_i_for_corpus(sentences_lower, filter_pattern, window, indices_included)
%s = remove_docs_without_hits(%s)

",
py_var_name, py_var_name, py_var_name))

    } else if (case_sensitive == TRUE) {
      reticulate::py_run_string(sprintf("

%s = sentence_windows_i_for_corpus(sentences, filter_pattern, window, indices_included)
%s = remove_docs_without_hits(%s)

",
py_var_name, py_var_name, py_var_name))
    }
  }

#' @export
filter_sentence_window <- function(filter_pattern, case_sensitive = FALSE, py_var_name = "filtered_indices") {
  py$filter_pattern <- filter_pattern

  if (case_sensitive == FALSE) {

  reticulate::py_run_string(sprintf("

%s = filter_index_object(%s, sentences_lower, filter_pattern)
%s = remove_docs_without_hits(%s)

",
py_var_name, py_var_name, py_var_name, py_var_name))

  } else if (case_sensitive == TRUE) {
  reticulate::py_run_string(sprintf("

%s = filter_index_object(%s, sentences, filter_pattern)
%s = remove_docs_without_hits(%s)

",
py_var_name, py_var_name, py_var_name, py_var_name))
  }
}

#' Title
#'
#' @param r_indexing Whether the input is from R index (starting on 1)
get_filtered_doc_indices_from_py <- function(r_indexing = TRUE) {
    indices <- reticulate::py_eval("get_indices(filtered_indices)")

    if (length(indices) > 0) {
        indices <- indices + r_indexing
    }
    return(indices)
}

get_number_of_sentences_per_doc <- function(r_indexing = TRUE) {
    py$chunks_per_doc <- reticulate::py_eval("number_of_extracted_sentences_per_doc(charting_indices)")
    docs <- reticulate::py_eval("get_indices(chunks_per_doc)")
    if (length(docs) > 0) {
        docs <- docs + r_indexing
        n <- reticulate::py_eval("[x[1] for x in chunks_per_doc]")
        df <- data.frame(Term_1 = n, ID = docs)
        return(df)
    }
}

# TODO. Ser stygt ut i
#' Title
get_filtered_sentences_from_one_doc_py <- function() {
    text <- reticulate::py_eval("retrieve_sentences_from_nested_indices_one_doc(doc_indices, sentences)")
    text <- lapply(text, paste, collapse = " ") %>%
      unlist(use.names = FALSE) %>%
        paste(collapse = "\n\n")
}

#' Returns a filtered
#'   (full text) df based on the returned indices
create_sentence_filtered_df <-
    function(df,
             filter_pattern,
             search_pattern,
             window) {

        get_nested_indices(filter_pattern,
                              search_pattern,
                              window)

        df_indices <- get_filtered_doc_indices_from_py()

        new_df <- df[df_indices, ]

        return(new_df)
    }

#' runs corporaexplorer `explore0()` on the df returned by
#'   `create_sentence_filtered_df()`, with two search fields
#'   pre-populated
explore_sentence_filtered_df <-
    function(df,
             filter_pattern,
             search_pattern,
             window) {

        new_df <- create_sentence_filtered_df(df,
                                          filter_pattern,
                                          search_pattern,
                                          window)

        explore0(new_df, arguments_explore = list(search_input = list(
            search_terms = c(filter_pattern, search_pattern)
        )))
    }
