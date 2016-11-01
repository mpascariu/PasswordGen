rm(list = ls())
pack_name <- 'PasswordGen'
version <- 'PasswordGen_0.0.1.tar.gz'
PATH <- '~/Dropbox/EDU/SDU-MaxO/PROJECTS/B1_PasswordGen/'

devtools::document()
devtools::build()
devtools::check_built(path = paste0(PATH, pack_name))

remove.packages(pack_name, lib="~/R/win-library/3.3")
install.packages(paste0(PATH, version), repos = NULL, type = "source")
# Shift + Ctrl + F10
