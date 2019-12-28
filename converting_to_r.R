sentr <- py$sentences_lower

sentr_window <- function(list_of_sentences, word, window) {
    kwics <- list()
    # kwics = character()
    sentence_found <-
        which(stringr::str_detect(list_of_sentences, word))
    for (i in sentence_found) {
        first_index <- i - window
        if (first_index < 1) {
            first_index <- 0
        }
        second_index <- i + window
        if (second_index > length(list_of_sentences)) {
            second_index <- length(list_of_sentences)
        }

        chunk <- list_of_sentences[first_index:second_index]

        kwics <- c(kwics, list(chunk))

    }
    return(kwics)
}

#' @param list_of_lists list returned by sentr_window()
merge_sentences_to_chunks <- function(list_of_lists) {
    lapply(list_of_lists, lapply, paste, collapse = " ") %>%
            lapply(unlist, use.names = FALSE)
}

#' @param list_of_lists list returned by merge_sentences_to_chunks()
filter_filtered_sentences <- function(list_of_lists, patterns) {
    for (pattern in patterns) {
        list_of_lists <- lapply(list_of_lists, stringr::str_subset, pattern)
    }
    return(list_of_lists)
}

#' @param list_of_lists list returned by filter_filtered_sentences()
number_of_chunks_per_doc <- function(list_of_lists) {
    lapply(list_of_lists, length) %>%
        unlist(use.names = FALSE)
}

#' @param chunks 1 element in list returned by filter_filtered_sentences()
print_chunks <- function(chunks) {
    info <- sprintf("Chunks in document: %s\n\n", length(chunks))
    chunks <- paste(chunks, collapse = "\n\n")
    return(paste0(info, chunks))
}

system.time(
    a <- print_chunks(bbb[[2]])
)



return_counts <- function(sentr, word1, word2, window) {
    lapply(sentr, sentr_window, word1, window) %>%
        merge_sentences_to_chunks() %>%
        filter_filtered_sentences(word2) %>%
        number_of_chunks_per_doc()
}

system.time({

# system.time(
b <- lapply(sentr, sentr_window, "аркти", 2)
# )
#  user  system elapsed
# 1.018   0.014   1.050

# system.time(
bb <- merge_sentences_to_chunks(b)
# )
#  user  system elapsed
# 0.805   0.011   0.836

# system.time(
bbb <- filter_filtered_sentences(bb, "норв")
# )
#  user  system elapsed
# 0.765   0.007   0.789

})

microbenchmark::microbenchmark(
    r = return_counts(sentr, "аркти", "норв", 2),
    py = cx_extra_chart("аркти--2--норв", v, v, FALSE, "data_dok"),
    times = 10
)
# Unit: milliseconds
#  expr      min        lq      mean    median        uq
#     r 985.4460 1091.0442 1274.3196 1220.9430 1327.2674
#    py 485.6264  505.3578  583.1751  557.6209  610.9683
#       max neval
#  2985.072   100
#  1188.451   100



system.time(
    cx_extra_subset("аркти--2--норв", v, FALSE)
)

system.time(
    cc <- cx_extra_chart("аркти--2--норв", v, v, FALSE, "data_dok")
)

ff <- number_of_chunks_per_doc(bbb)

#####

bdf <- tibble::tibble(Text = bb, ID = seq_along(bb))

system.time(
bdf <- tidyr::unnest(bdf, Text)
)
  #  user  system elapsed
  # 7.966   1.304   9.307

system.time(
bdf2 <- dplyr::filter(bdf, stringr::str_detect(Text, "внимание"))
)







system.time(
bb <- lapply(b[[1]], stringr::str_detect, "норв")
)

system.time({
ny <- list()
for (doc in b) {
    mlm <- lapply(doc,  stringr::str_detect, "норв")
    mlm <- unlist(lapply(mlm, any), use.names = FALSE)
    ny <- c(ny, list(mlm))
}
})


system.time(
    pasted_window <- lapply(b, lapply, paste, collapse = " ") %>%
        lapply(unlist, use.names = FALSE)
)

system.time(
    pasted_window <- lapply(b, unlist, use.names = FALSE)
)

system.time(
get_sentence_window_indices("Росс", 0, TRUE)
)
  #  user  system elapsed
  # 0.733   0.008   0.751


