#' Function to generate secure readable passwords/passphrases
#' 
#' @param no_words - Number of words to be used in the password
#' @param symbols - Do we want symbols in the password (TRUE/FALSE)? 
#' @param numbers - Do we want numbers in the password (TRUE/FALSE)? 
#' If TRUE random numbers between 0 and 999 are selected.
#' @param sep - Specify the separator between the words. 
#' If NULL a random symbol is selected.
#' @param data - words to be used in the password. In default mode the words from
#' \href{http://www.gutenberg.org/ebooks/29765}{The Project Gutenberg EBook 
#' of Webster's Unabridged Dictionary, by Various} are used (93077 words).
#' @return A password.
#' @examples 
#' library(PasswordGen)
#' 
#' password(no_words = 7, numbers = FALSE, symbols = FALSE, sep = ' ')
#' password(no_words = 3, numbers = FALSE, symbols = FALSE, sep = '_')
#' password(no_words = 3, numbers = FALSE, symbols = TRUE, sep = '_')
#' password(no_words = 4, numbers = TRUE, symbols = FALSE, sep = '-')
#' password(1)
#' password(2)
#' password()
#' 
#' @export
password <- function(no_words = 3, 
                     symbols = TRUE, numbers = TRUE, 
                     sep = NULL, data = NULL){
  
  if (is.null(data)) data = PasswordGen::Webster
  if (class(data) == 'ExtractWords') {data = data$words}
  # Words and symbols
  Symbols <- c('!', '#', '$', '%', '&', '+', 
               '-', '=', '?', '@', '_', '|', '/',
               '~', '^', '*', '(',')','{','}', '<', '>')
  if (is.null(sep)) sep = sample(c(' ', Symbols))
  # Randomly select words, numbers and symbols
  w1  <- sample(data, no_words)
  if (no_words > 1) {
    pos <- sort.int(sample(1:no_words, size = round(no_words/2)))
    pos2 <- sort.int(sample(1:no_words, size = round(no_words/2)))
    w1[pos]  <- tolower(w1[pos]) 
    w1[-pos] <- toupper(w1[-pos])
    w1[pos2] <- sapply(w1[pos2], simpleCap)
  }
  if (no_words == 1) w1 <- sample(c(toupper(w1), tolower(w1)), 1)
  
  if (numbers == TRUE) { w2 <- sample(0:999, no_words) }else{ w2 <- NULL }
  if (symbols == TRUE) { w3 <- sample(Symbols, no_words) }else{ w3 <- NULL }
  # Put the password together
  w4 <- paste0(w3, w2)
  Password <- paste0(paste(w1, w4, sep = ''), sep = '', collapse = sep)
  return(Password)
}


#' @keywords internal
#' 
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse = " ")
}



#' Function to extract words from a .txt file
#' 
#' Function to extract words from a .txt file. For example a book from 
#' Project Gutenberg \url{http://www.gutenberg.org/wiki/Main_Page}.
#' @param data.txt a .txt file from your working directory (say, your favorite book)
#' @param data.r a character string
#' @param words.min.length The minimum legth of the words to be extracted. 
#' Default value is 3.
#' @examples
#' library(PasswordGen)
#' 
#' # Using the \code{gutenbergr} R package we will download a book and 
#' # extract words from it. Let's say, "The Adventures of Sherlock Holmes" by 
#' # Doyle, Arthur Conan
#' 
#' library(gutenbergr)
#' library(stringr)
#' # look up book and check the id on Gutenberg Project
#' info <- gutenberg_works(str_detect(title, "The Adventures of Sherlock Holmes"))
#' info$gutenberg_id # this should be 1661
#' 
#' # download the book
#' my_book <- gutenberg_download(gutenberg_id = 1661, meta_fields = "title",
#'                               mirror = "http://gutenberg.pglaf.org")
#' 
#' # Extract words
#' words <- ExtractWords(data.r = my_book$text)
#' words
#' 
#' # Generate passwords
#' password(no_words = 5, data = words)
#' 
#' # -----------
#' # Example 2 - Extract russian words and generate passwords
#' 
#' # check the id of the book on Gutenberg Project
#' info <- gutenberg_works(languages = 'ru')
#' info # let's take the first id: 5316
#' 
#' # download the book
#' my_book <- gutenberg_download(gutenberg_id = 5316, meta_fields = "title",
#'                               mirror = "http://gutenberg.pglaf.org")
#' 
#' # Extract words
#' words <- ExtractWords(data.r = my_book$text)
#' 
#' password(no_words = 5, data = words,
#'          numbers = FALSE, symbols = FALSE, sep = "-")
#' 
#' @export
ExtractWords <- function(data.txt = NULL, 
                         data.r = NULL, 
                         words.min.length = 3) {
  pb <- startpb(0, 3)
  on.exit(closepb(pb))
  
  if (!is.null(data.r)) {
    file_name = paste0(data.r[1],'.Rdata')
    cat(data.r, file = file_name, sep = "\n")
    file = file_name } 
  else {file = data.txt}
  
  conn  <- file(file, open = "r")
  linn  <- readLines(conn)
  close(conn)
  linn1 <- unique(tolower(linn))
  linn2 <- gsub('[^[:alpha:]]',' ', linn1)
  setpb(pb, 1)
  w  <- unique(unlist(strsplit(linn2, split = ' ')))
  setpb(pb, 2)
  
  criteria <- apply(data.frame(w), 1, 
                    function(x) nchar(x)) >= words.min.length
  out <- list(file = file, words = sort(w[criteria]))
  out_ <- structure(class = 'ExtractWords', out)
  
  setpb(pb, 3)
  if (!is.null(data.r)) file.remove(paste(file_name))
  return(out_)
}

#' @keywords internal
#' @export
print.ExtractWords <- function(x, ...){
  cat('\nFile: ', x$file)
  cat('\nNumber of unique words: ', length(x$words))
}





