rm(list=ls())

words <- readLines("words.txt")

devtools::use_data(words, overwrite = TRUE)








