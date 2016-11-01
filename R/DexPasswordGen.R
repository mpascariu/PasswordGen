
#' Function to generate secure readable passwords/passphrases
#' 
#' @param no_words - Number of words to be used in the password
#' @param symbols - Do we want symbols in the password (TRUE/FALSE)? 
#' @param numbers - Do we want numbers in the password (TRUE/FALSE)? 
#' If TRUE random numbers between 100 and 999 are selected.
#' @param sep - Specify the separator between the words. 
#' If NULL a random symbol is selected.
#' @param data - words to be used in the password
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
                     numbers = TRUE, sep = NULL, data = words){
  # Words and symbols
  Symbols <- c('!', '#', '$', '%', '&', '+', 
               '-', '=', '?', '@', '_', '|' )
  if(is.null(sep)) sep = sample(c(' ', Symbols))
  # Randomlly select words, numbers and symbols
  w1  <- sample(data, no_words)
  if(no_words > 1){
  pos <- sort.int(sample(1:no_words, size = round(no_words/2)))
  w1[pos] <- tolower(w1[pos]) 
  }
  if(no_words == 1) w1 <- sample(c(w1, tolower(w1)), 1)
  
  if(numbers == TRUE) { w2 <- sample(100:999, no_words) }else{ w2 <- NULL}
  if(symbols == TRUE) { w3 <- sample(Symbols, no_words) }else{ w3 <- NULL}
  # Put the password together
  w4 <- paste0(w3, w2)
  Password <-  paste0(paste(w1, w4, sep = ''), sep = '', collapse = sep)
  return(Password)
}



