#' Tokenize character vector to a list of lists of sentences
#'
#' IMPORTANT: must be run first in order to create
#'   internal Python object `sentences`
#'   and `sentences_lower`
#'   to be accessed by `get_sentence_filtered_doc_indices()`
#'   (and thereby also by `create_sentence_filtered_df()`
#'   and `explore_sentence_filtered_df()`).
#'
#' @param df R data frame
#' @param to_lower convert the final list of lists to lower case?
#'
#' @return internal Python object `sentences`
#'   and `sentences_lower`.
#' @export
tokenize_sentences_wrapper <- function(df, to_lower = TRUE) {
    py$Text <- df$Text
    reticulate::py_run_string("sentences = tokenize_sentences(Text)")

    if (to_lower == TRUE) {
        reticulate::py_run_string("sentences_lower = sentences_to_lower(sentences)")
    }
    # return(py$sentences)
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
             window) {
        py$filter_pattern <- filter_pattern
        py$window <- as.integer(window)

        reticulate::py_run_string('

filtered_indices = [(i, sentence_window_i(doc, filter_pattern, window)) for i, doc in enumerate(sentences_lower)]
filtered_indices = remove_docs_without_hits(filtered_indices)

')
}

#' @export
filter_sentence_window <- function(filter_pattern) {
  py$filter_pattern <- filter_pattern
  reticulate::py_run_string("

filtered_indices = filter_index_object(filtered_indices, sentences_lower, filter_pattern)
filtered_indices = remove_docs_without_hits(filtered_indices)

")
}

#' Title
#'
#' @param r_indexing
#'
#' @return
#' @export
get_filtered_doc_indices_from_py <- function(r_indexing = TRUE) {
    indices <- reticulate::py_eval("get_indices(filtered_indices)")

    if (length(indices) > 0) {
        indices <- indices + r_indexing
    }
    return(indices)
}

# TODO. Ser stygt ut i
#' Title
#'
#' @return
#' @export
get_filtered_sentences_from_py <- function() {
    texts <- reticulate::py_eval("retrieve_sentences_from_nested_indices(filtered_indices, sentences, join_string = ' ')")
    texts <- lapply(texts, paste, collapse = "\n\n")
    texts <- unlist(texts)
}


#' Returns a filtered
#'   (full text) df based on the returned indices
#' @export
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
#' @export
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
