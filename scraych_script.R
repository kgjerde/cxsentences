library(reticulate)

reticulate::use_virtualenv("/Users/Kristian/.local/share/virtualenvs/apekatt-mr9vg8U1/", required = TRUE)

reticulate::py_run_file("/Users/Kristian/Downloads/apekatt/sentence_functions.py")
# source("/Users/Kristian/Downloads/apekatt/sentence_functions.R")


# # Load df -----------------------------------------------------------------
# df <- readRDS("/Users/Kristian/Dropbox/Projects/autoscraping/MID/data/R_data/mid_live.rds")
#
# # Sentence tokenizing - the time consuming (and language dependent) part --
# tokenize_sentences_wrapper(df)
#
# # Explore0 in one command -------------------------------------------------
#
# explore_sentence_filtered_df(
#     df = df,
#     filter_pattern = "двойн.{1,3}\\sстандарт",
#     search_pattern = "\\bнато",
#     window = 6
# )
#
#
#
# # Just get indices (from `py$setninger` -----------------------------------
#
# create_sentence_filtered_df(
#     df = df,
#     filter_pattern = "двойн.{1,3}\\sстандарт",
#     search_pattern = "\\bнато",
#     window = 6
# )
#
# system.time(
# get_sentence_filtered_doc_indices(
#     filter_pattern = "двойн.{1,3}\\sстандарт",
#     search_pattern = "\\bнато",
#     window = 6
# )
# )
#
# reticulate::py_run_string('
#
# print(9)
# print(11)
#
# ')
