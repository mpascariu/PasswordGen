
#' Function to generate secure readable passwords/passphrases
#' 
#' @param no_words - Number of words to be used in the password
#' @param symbols - Do we want symbols in the password (TRUE/FALSE)? 
#' @param numbers - Do we want numbers in the password (TRUE/FALSE)? 
#' If TRUE random numbers between 100 and 999 are selected.
#' @param sep - Specify the separator between the words. 
#' If NULL a random symbol is selected.
#' @param data - words to be used in the password. In default mode the words from
#' \href{http://www.gutenberg.org/ebooks/29765}{The Project Gutenberg EBook 
#' of Webster's Unabridged Dictionary, by Various} are used (93077 words).
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
password <- function(no_words = 3, symbols = TRUE, 
                     numbers = TRUE, sep = NULL, data = Webster){
  if (class(data) == 'ExtractWords') {data = data$words}
  # Words and symbols
  Symbols <- c('!', '#', '$', '%', '&', '+', 
               '-', '=', '?', '@', '_', '|' )
  if (is.null(sep)) sep = sample(c(' ', Symbols))
  # Randomly select words, numbers and symbols
  w1  <- sample(data, no_words)
  if (no_words > 1) {
  pos <- sort.int(sample(1:no_words, size = round(no_words/2)))
  w1[pos]  <- tolower(w1[pos]) 
  w1[-pos] <- toupper(w1[-pos]) 
  }
  if (no_words == 1) w1 <- sample(c(toupper(w1), tolower(w1)), 1)
  
  if (numbers == TRUE) { w2 <- sample(100:999, no_words) }else{w2 <- NULL}
  if (symbols == TRUE) { w3 <- sample(Symbols, no_words) }else{w3 <- NULL}
  # Put the password together
  w4 <- paste0(w3, w2)
  Password <- paste0(paste(w1, w4, sep = ''), sep = '', collapse = sep)
  return(Password)
}

# ---------
#' Function to extract words from a .txt file
#' 
#' Function to extract words from a .txt file. For example a book from 
#' Project Gutenberg \url{http://www.gutenberg.org/wiki/Main_Page}.
#' @param file .txt file (say, your favorite book)
#' @param words.min.length The minimum legth of the words to be extracted. 
#' Default value is 3.
#' @examples
#' \dontrun{
#' library(PasswordGen)
#' 
#' # Specify the name of a file that you already saved in your workind directory
#' mybook <- "Project Gutenberg's The Adventures of Sherlock Holmes, 
#' by Arthur Conan Doyle.txt"
#' 
#' # Extract the unique words
#' mywords <- ExtractWords(file = mybook, words.min.length = 3)
#' 
#' # Generate password using my words from Sherlock Holmes book.
#' password(data = mywords)
#' 
#' password(no_words = 7, numbers = FALSE, symbols = FALSE, sep = ' ',
#'          data = mywords)
#' } 
#' @export
#' 
ExtractWords <- function(file, words.min.length = 3) {
  pb <- startpb(0, 3)
  on.exit(closepb(pb))
  
  conn  <- file(file, open = "r")
  linn  <- readLines(conn)
  linn2 <- gsub('[^[:alpha:]]',' ', linn)
  close(conn)
  setpb(pb, 1)
  
  w  <- unlist(strsplit(linn2, split = ' '))
  w_ <- unique(tolower(w)) # Convert all words to lowercase and remove duplicates
  setpb(pb, 2)
  
  criteria <- apply(data.frame(w_), 1, 
                    function(x) nchar(x)) > words.min.length
  out <- list(file = file, words = sort(w_[criteria]))
  out_ <- structure(class = 'ExtractWords', out)
  setpb(pb, 3)
  return(out_)
}

#' @keywords internal
#' @export
#' 
print.ExtractWords <- function(x, ...){
  cat('\nFile: ', x$file)
  cat('\nNumber of unique words: ', length(x$words))
}





