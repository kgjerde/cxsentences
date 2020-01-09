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

sentr_window_list <- function(list_of_sentences, word, window) {
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
merge_sentences_to_chunks <- function(list_of_lists, window) {
    if (window != 0) {
        lapply(list_of_lists, lapply, stringi::stri_c, collapse = " ") %>%
                lapply(unlist, use.names = FALSE)
    } else {
        lapply(list_of_lists, unlist, use.names = FALSE)
    }
}

#' @param list_of_lists list returned by sentr_window()
merge_sentences_to_chunks_old <- function(list_of_lists) {
    lapply(list_of_lists, lapply, paste0, collapse = " ") %>%
            lapply(unlist, use.names = FALSE)
}



#' @param list_of_lists list returned by merge_sentences_to_chunks()
filter_filtered_sentences <- function(list_of_lists, patterns) {
    for (pattern in patterns) {
        list_of_lists <- lapply(list_of_lists, stringr::str_subset, pattern)
    }
    return(list_of_lists)
}

#' @param list_of_lists list returned by merge_sentences_to_chunks()
filter_filtered_sentences_loop <- function(list_of_lists, patterns) {
    new_list <- vector("list", length = length(list_of_lists))
    for (i in seq_along(list_of_lists)) {
        new_list[[i]] <- stringr::str_subset(list_of_lists[[i]], patterns)
    }
    return(new_list)
}

#' @param list_of_lists list returned by sentr_window()
filter_filtered_sentences_2 <- function(list_of_lists, patterns) {
    # for (pattern in patterns) {
    doc_list <- list()
        for (chunk in list_of_lists) {
            if (any(stringr::str_detect(chunk, patterns))) {
                doc_list <- c(doc_list, list(chunk))
            }
        }
    # }
    return(doc_list)
}

filter_filtered_sentences_22 <- function(list_of_lists, patterns) {
    lapply(list_of_lists, filter_filtered_sentences_2, patterns)
}


system.time(
alt <- lapply(b, filter_filtered_sentences_2, "норв")
)


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
        merge_sentences_to_chunks(window) %>%
        filter_filtered_sentences(word2) %>%
        number_of_chunks_per_doc()
}

return_counts_base <- function(sentr, word1, word2, window) {
    lapply(sentr, sentr_window, word1, window) %>%
        merge_sentences_to_chunks_old() %>%
        filter_filtered_sentences(word2) %>%
        number_of_chunks_per_doc()
}

return_counts_sparse <- function(sentr, word1, word2, window) {
    a <- lapply(sentr, sentr_window, word1, window)
    a <- purrr::compact(a) %>%
        merge_sentences_to_chunks(window) %>%
        filter_filtered_sentences(word2) %>%
        number_of_chunks_per_doc()
}

return_counts2 <- function(sentr, word1, word2, window) {
    lapply(sentr, sentr_window_2, word1, window, word2) %>%
        merge_sentences_to_chunks(window) %>%
        # filter_filtered_sentences(word2) %>%
        number_of_chunks_per_doc()
}

return_counts3 <- function(sentr, word1, word2, window) {
    lapply(sentr, sentr_window, word1, window) %>%
        filter_filtered_sentences_22(word2) %>%
        number_of_chunks_per_doc()
}


microbenchmark::microbenchmark(
    stringi = merge_sentences_to_chunks(b),
    base = merge_sentences_to_chunks_old(b),
    times = 40
)
# b <- lapply(sentr, sentr_window, "росс", 8)
# Unit: seconds
#     expr      min       lq     mean   median       uq      max neval
#  stringi 1.115870 1.170933 1.269453 1.219204 1.289466 2.441569   100
#     base 2.290937 2.362328 2.504674 2.438422 2.548939 3.708332   100

# b <- lapply(sentr, sentr_window, "норв", 3)
# Unit: milliseconds
#     expr      min       lq     mean   median       uq      max neval
#  stringi 36.14927 38.49612 80.87380 62.20351 107.1074 260.9485    40
#  base 41.86557 45.93326 70.55967 55.28758 100.8056 139.7486    40

# b <- lapply(sentr, sentr_window, "норв", 0)
# Unit: milliseconds
#     expr      min       lq     mean   median       uq      max neval
#  stringi 30.55087 33.22793 52.79158 37.78607 72.38779 112.7723    40
#     base 31.11313 33.65611 61.72426 58.29211 87.11013 133.6688    40

microbenchmark::microbenchmark(
    stringi = merge_sentences_to_chunks(b, 0),
    base = merge_sentences_to_chunks_old(b),
    times = 40
)

    system.time({

system.time(
b <- lapply(sentr, sentr_window, "росс", 5)
)


#  user  system elapsed
# 1.018   0.014   1.050

system.time(
bb <- merge_sentences_to_chunks(b, 5)
)
#  user  system elapsed
# 0.805   0.011   0.836

system.time(
bbb <- filter_filtered_sentences(bb, "норв")
)
#  user  system elapsed
# 0.765   0.007   0.789

system.time(
bbb <- filter_filtered_sentences_loop(bb, "норв")
)


system.time(bbbb <- number_of_chunks_per_doc(bbb))

})

system.time(
alt <- filter_filtered_sentences_2(b, "норв")
)


microbenchmark::microbenchmark(
    r = return_counts(sentr, "аркти", "норв", 5),
    r2 = return_counts2(sentr, "аркти", "норв", 5),
    r3 = return_counts3(sentr, "аркти", "норв", 5),
    py = cx_extra_chart("аркти--5--норв", v, v, FALSE, "data_dok"),
    times = 10
)
# Unit: milliseconds
#  expr       min        lq      mean    median       uq
#     r 1019.1927 1077.0435 1218.5715 1094.1826 1168.996
#    r2 1030.1402 1043.4909 1248.5082 1074.5888 1440.986
#    r3  936.9908  964.3483 1509.3789 1040.6802 1735.206
#    py  486.4179  487.7473  524.7346  505.0007  549.792

microbenchmark::microbenchmark(
    r = return_counts(sentr, "росс", "норв", 5),
    r2 = return_counts2(sentr, "росс", "норв", 5),
    r3 = return_counts3(sentr, "росс", "норв", 5),
    r_sparse = return_counts_sparse(sentr, "росс", "норв", 5),
    py = cx_extra_chart("росс--5--норв", v, v, FALSE, "data_dok"),
    times = 5
)
# Unit: seconds
#      expr      min        lq      mean    median        uq       max neval
#         r 4.795036  5.917433  6.456875  6.366423  6.850265  8.355219     5
#        r2 9.626686 10.240727 10.977829 11.253021 11.295491 12.473219     5
#        r3 8.109973  9.193608 11.225763 10.390355 10.986902 17.447978     5
#  r_sparse 4.647805  5.152424  5.551334  5.394288  5.603673  6.958481     5
#        py 1.355176  1.505261  1.709374  1.696474  1.986740  2.003220     5

microbenchmark::microbenchmark(
    r = return_counts(sentr, "росс", "норв", 2),
    r2 = return_counts2(sentr, "росс", "норв", 2),
    r3 = return_counts3(sentr, "росс", "норв", 2),
    py = cx_extra_chart("росс--2--норв", v, v, FALSE, "data_dok"),
    times = 5
)
# Unit: milliseconds
#  expr       min        lq     mean    median       uq      max neval
#     r 2776.4753 2906.4279 3067.321 3072.0986 3158.118 3423.485     5
#    r2 4179.7102 4316.1172 4487.372 4479.8379 4579.965 4881.228     5
#    r3 4282.2881 4289.2812 4832.204 4315.3793 5003.075 6270.999     5
#    py  955.4101  967.0355  996.471  969.3608 1002.168 1088.381     5


microbenchmark::microbenchmark(
    r = return_counts(sentr, "норв", "росс", 2),
    r2 = return_counts2(sentr, "норв", "росс", 2),
    r_sparse = return_counts_sparse(sentr, "норв", "росс", 2),
    py = cx_extra_chart("норв--2--росс", v, v, FALSE, "data_dok"),
    times = 10
)
# Unit: milliseconds
#      expr      min       lq      mean    median        uq       max neval
#         r 1200.836 1269.203 1400.8449 1338.3125 1528.1599 1760.5873    10
#        r2 1243.599 1293.935 1382.7447 1360.0783 1444.0837 1649.3052    10
#  r_sparse 1158.021 1338.388 1379.4529 1389.6424 1434.6172 1676.9741    10
#        py  539.566  551.234  599.0443  579.9473  622.5914  723.2612    10

microbenchmark::microbenchmark(
    r = return_counts(sentr, "норв", "росс", 30),
    r2 = return_counts2(sentr, "норв", "росс", 30),
    r3 = return_counts3(sentr, "норв", "росс", 30),
    py = cx_extra_chart("норв--30--росс", v, v, FALSE, "data_dok"),
    times = 5
)
# Unit: milliseconds
#  expr       min        lq      mean    median       uq      max neval
#     r 1181.6108 1191.0589 1270.2349 1222.9625 1240.916 1514.626     5
#    r2 1096.9151 1151.0544 1230.2714 1158.1573 1233.869 1511.361     5
#    r3 1020.9474 1071.3057 1197.0889 1093.3209 1125.091 1674.780     5
#    py  523.3105  592.0201  808.0021  709.6433 1057.759 1157.277     5


microbenchmark::microbenchmark(
    r = return_counts(sentr, "росс", "норв", 0),
    r2 = return_counts2(sentr, "росс", "норв", 0),
    r3 = return_counts3(sentr, "росс", "норв", 0),
    py = cx_extra_chart("росс--0--норв", v, v, FALSE, "data_dok"),
    times = 10
)
# Unit: milliseconds
#  expr       min        lq      mean    median       uq      max neval
#     r 1672.2031 1698.1521 1840.3343 1757.0411 1893.594 2420.260    10
#    r2 4200.8553 4541.2222 4794.3296 4638.9840 5036.015 5942.347    10
#    r3 3992.8002 4201.8057 4846.3927 4651.3899 5566.201 5916.061    10
#    py  792.6106  800.3474  877.1182  843.2661  957.358 1053.648    10


microbenchmark::microbenchmark(
    r = return_counts(sentr, "норв", "росс", 0),
    r2 = return_counts2(sentr, "норв", "росс", 0),
    r3 = return_counts3(sentr, "норв", "росс", 0),
    py = cx_extra_chart("норв--0--росс", v, v, FALSE, "data_dok"),
    times = 30
)
# Unit: milliseconds
#  expr       min        lq      mean    median        uq       max neval
#     r 1166.4966 1294.3164 1447.7057 1414.0066 1627.6103 1794.6415    30
#    r2 1122.9069 1293.0699 1393.2291 1342.4379 1510.7530 1920.9471    30
#    r3 1151.3516 1330.1098 1413.5744 1409.2442 1468.3387 1970.9396    30
#    py  518.4052  563.1554  607.7522  603.2494  642.3546  713.2174    30


microbenchmark::microbenchmark(
    r = return_counts(sentr, "росс", "норв", 30),
    roldmerge = return_counts_base(sentr, "росс", "норв", 30),
    r3 = return_counts3(sentr, "росс", "норв", 30),
    py = cx_extra_chart("росс--30--норв", v, v, FALSE, "data_dok"),
    times = 5
)
# Unit: seconds
#       expr       min        lq      mean    median        uq       max
#          r  8.948094  8.958196  9.165218  9.069965  9.327429  9.522408
#  roldmerge 12.468532 12.563883 13.012457 12.818955 13.517599 13.693318
#         r3 10.148831 10.205735 10.496296 10.278802 10.622344 11.225770
#         py  1.695224  1.737301  1.832413  1.752999  1.884579  2.091964


word_2 <- "норв"
word_1  <- "аркт"
window <- 2
times <- 25
microbenchmark::microbenchmark(
    r = return_counts(sentr, word_1, word_2, window),
    roldmerge = return_counts_base(sentr, word_1, word_2, window),
    # r2 = return_counts2(sentr, word_1, word_2, window),
    # r3 = return_counts3(sentr, word_1, word_2, window),
    r_sparse = return_counts_sparse(sentr, word_1, word_2, window),
    py = cx_extra_chart(
        sprintf("%s--%s--%s", word_1, window, word_2),
        v,
        v,
        FALSE,
        "data_dok"
    ),
    times = times
)








# df way ------------------------------------------------------------------

library(dplyr)

sentr <- py$sentences_lower

sdf <- tibble::tibble(Text = sentr) %>%
    dplyr::mutate(ID = seq_along(sentr)) %>%
    tidyr::unnest(Text)

sdf <- sdf %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(Sent_ID = seq_len(dplyr::n())) %>%
    dplyr::mutate(Last_in_doc = dplyr::row_number() == dplyr::n())

system.time(
    ny <- sdf %>%
        dplyr::filter(stringr::str_detect(Text, "норв"))
)




# df ----------------------------------------------------------------------

system.time({
    ny <- sdf
    ny$Found <- stringr::str_detect(ny$Text, "норв")
})

ny <- sdf
ny$Found_2 <- FALSE

ind <- which(ny$Found)

ny$Found_2 <- FALSE

window <- 2

system.time({
for (index in ind) {
    ny$Found_2[(index-window):(index+window)] <- TRUE
}
})

system.time({
for (index in ind) {
    ny$Found_2[(index-window):(index+window)] <- TRUE
}
})

make_window <- function(df, window) {
    df$Found <- stringr::str_detect(df$Text, "норв")
    # ind <- which(df$Found)
    # df$Found_2[(ind-window):(ind+window)] <- TRUE
    return(df)
}

system.time(
ny2 <- dplyr::group_modify(ny, make_window)
)

ny$Found_2 <-



# df slutt ----------------------------------------------------------------

library(data.table)

sdf_dt <- as.data.table(sdf)

sdf_df <- as.data.frame(sdf)

system.time(
    ny <- sdf_df %>%
        dplyr::mutate(Ja = stringr::str_detect(Text, "норв"))
)





system.time({
    ny <- sdf_dt
    ny$Ja <- stringr::str_detect(ny$Text, "норв")
})



system.time(
Ja <- stringr::str_detect(a, "норв")
)

system.time(a <- lapply(sentr, stringr::str_detect, "норв"))


word <- "норв"
microbenchmark::microbenchmark(
    dplyr = dplyr::mutate(sdf, Ja = stringr::str_detect(Text, word)),
    dt = dplyr::mutate(sdf_dt, Ja = stringr::str_detect(Text, word)),
    dt2 = sdf_dt$TEST <- stringr::str_detect(sdf_dt$Text, word),
    df = sdf_df$TEST <- stringr::str_detect(sdf_df$Text, word),
    times = 10
)

# word <- "норв"
# Unit: milliseconds
# Unit: milliseconds
#   expr       min        lq      mean    median        uq       max neval
#  dplyr 1003.6870 1032.5743 1089.0236 1056.0998 1091.2308 1382.4055    10
#     dt  785.0390  790.2472  824.0070  818.2013  839.0706  894.0379    10
#    dt2  786.9947  792.1260  831.0378  807.4845  824.5520 1007.1968    10
#     df  787.3194  804.5691  815.2068  813.4455  827.7101  841.5788    10

# word <- "росс"
# Unit: milliseconds
#   expr      min       lq     mean   median        uq       max neval
#  dplyr 925.8694 932.4151 992.7588 955.6869 1055.9946 1129.9085    10
#     dt 727.4570 736.2820 794.9027 777.4732  828.2888  984.3401    10
#    dt2 735.3228 751.0398 781.7947 764.4673  813.4891  850.3447    10
#     df 733.4842 751.7376 797.3132 776.5748  839.3657  896.5035    10

