sentr <- py$sentences_lower

is_sent_in_doc <- function(list_of_sentences, word, window, word2) {
    sentence_found <-
        which(stringr::str_detect(list_of_sentences, word))
    for (i in sentence_found) {
        first_index <- i - window
        if (first_index < 1) {
            first_index <- 1
        }
        second_index <- i + window
        if (second_index > length(list_of_sentences)) {
            second_index <- length(list_of_sentences)
        }

        chunk <- list_of_sentences[first_index:second_index]

        if (any(stringr::str_detect(chunk, word2))) {
            return(TRUE)
        }
    }
    return(FALSE)
}

find_docs_with_sent <- function(list_of_sentences, word, window, word2) {
    lapply(list_of_sentences, is_sent_in_doc, word, window, word2) %>%
        unlist(use.names = FALSE)
}

system.time(
d <- find_docs_with_sent(sentr, "аркти", 0, "норв")
)

sentr_window_2 <- function(list_of_sentences, word, window, word2) {
    kwics <- list()
    # kwics = character()
    sentence_found <-
        which(stringr::str_detect(list_of_sentences, word))
    for (i in sentence_found) {
        first_index <- i - window
        if (first_index < 1) {
            first_index <- 1
        }
        second_index <- i + window
        if (second_index > length(list_of_sentences)) {
            second_index <- length(list_of_sentences)
        }

        chunk <- list_of_sentences[first_index:second_index]

        if (any(stringr::str_detect(chunk, word2))) {
            kwics <- c(kwics, list(chunk))
        }
    }
    return(kwics)
}

sentr_window <- function(list_of_sentences, word, window) {
    kwics <- list()
    # kwics = character()
    sentence_found <-
        which(stringr::str_detect(list_of_sentences, word))
    for (i in sentence_found) {
        first_index <- i - window
        if (first_index < 1) {
            first_index <- 1
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

return_counts2 <- function(sentr, word1, word2, window) {
    lapply(sentr, sentr_window_2, word1, window, word2) %>%
        merge_sentences_to_chunks() %>%
        # filter_filtered_sentences(word2) %>%
        number_of_chunks_per_doc()
}

system.time({

system.time(
b <- lapply(sentr, sentr_window, "аркти", 2)
)
#  user  system elapsed
# 1.018   0.014   1.050

system.time(
bb <- merge_sentences_to_chunks(b)
)
#  user  system elapsed
# 0.805   0.011   0.836

system.time(
bbb <- filter_filtered_sentences(bb, "норв")
)
#  user  system elapsed
# 0.765   0.007   0.789

system.time(bbbb <- number_of_chunks_per_doc(bbb))

})

microbenchmark::microbenchmark(
    r = return_counts(sentr, "аркти", "норв", 5),
    r2 = return_counts2(sentr, "аркти", "норв", 5),
    py = cx_extra_chart("аркти--5--норв", v, v, FALSE, "data_dok"),
    times = 20
)
# Unit: milliseconds
#  expr       min        lq      mean   median        uq
#     r 1017.4205 1074.1159 1122.7743 1105.152 1183.4227
#    r2  966.8129  991.5643 1050.5192 1042.982 1104.4626
#    py  486.9778  490.4522  541.1427  527.708  556.4243
#        max neval

microbenchmark::microbenchmark(
    r = return_counts(sentr, "росс", "норв", 5),
    r2 = return_counts2(sentr, "росс", "норв", 5),
    py = cx_extra_chart("росс--5--норв", v, v, FALSE, "data_dok"),
    times = 20
)
# Unit: seconds
#  expr      min       lq     mean   median       uq      max neval
#     r 4.472105 4.581303 4.911670 4.715933 5.305840 5.777559    20
#    r2 5.653064 5.818729 6.119831 5.980553 6.315807 6.896194    20
#    py 1.065378 1.155618 1.240243 1.197457 1.337164 1.481881    20

microbenchmark::microbenchmark(
    r = return_counts(sentr, "росс", "норв", 2),
    r2 = return_counts2(sentr, "росс", "норв", 2),
    py = cx_extra_chart("росс--2--норв", v, v, FALSE, "data_dok"),
    times = 10
)
# Unit: milliseconds
#  expr       min       lq     mean   median       uq      max neval
#     r 3096.4287 3150.921 3291.055 3245.594 3261.815 4091.520    10
#    r2 4677.5691 4786.988 5034.295 4949.699 5050.506 5941.090    10
#    py  973.4694 1037.370 1075.632 1061.108 1078.962 1339.887    10


microbenchmark::microbenchmark(
    r = return_counts(sentr, "норв", "росс", 2),
    r2 = return_counts2(sentr, "норв", "росс", 2),
    py = cx_extra_chart("норв--2--росс", v, v, FALSE, "data_dok"),
    times = 10
)
# Unit: milliseconds
#  expr       min        lq      mean   median        uq      max neval
#     r 1116.3101 1198.9747 1282.3095 1215.354 1415.1576 1636.677    10
#    r2 1107.0773 1150.8080 1225.2359 1179.460 1252.1754 1538.974    10
#    py  543.6935  598.6902  723.9059  618.181  720.2247 1515.922    10

microbenchmark::microbenchmark(
    r = return_counts(sentr, "норв", "росс", 30),
    r2 = return_counts2(sentr, "норв", "росс", 30),
    py = cx_extra_chart("норв--30--росс", v, v, FALSE, "data_dok"),
    times = 5
)
# Unit: milliseconds
#  expr       min        lq      mean    median        uq       max neval
#     r 1201.3765 1211.9685 1264.3716 1228.2818 1283.5112 1396.7200     5
#    r2 1066.1594 1077.6697 1124.5491 1147.8952 1156.5733 1174.4480     5
#    py  495.0763  495.4088  515.6205  496.8427  510.0993  580.6754     5



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


