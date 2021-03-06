# PasswordGen R Package
[![Build Status](https://travis-ci.org/mpascariu/PasswordGen.svg?branch=master)](https://travis-ci.org/mpascariu/PasswordGen)
[![license](https://img.shields.io/github/license/mpascariu/PasswordGen.svg)]()

Sometimes we need not only secure passwords but also passwords easy to
remember. PasswordGen is an **R** package that helps you generates
passwords and passphrases easy to remember with an algorithm based on
the dictionary method.

The default `password()` function generates random passwords using words
from [The Project Gutenberg EBook of Webster's Unabridged Dictionary, by
Various](http://www.gutenberg.org/ebooks/29765). In total 93077 words
are used. However, you can use the `ExtractWords()` function to extract
all the words from any book and use them in the `password()` generator.
Check the examples provided in the help files of the package.

### Installation

1.  Make sure you have the most recent version of R
2.  Run the following code in your R console

<!-- -->

    devtools::install_github("mpascariu/PasswordGen")

If you do not have **devtools**, you can install it from CRAN, by typing
`install.packages("devtools")`.

Alternatively, clone or download the repository to a local directory and
install it by running
`install.packages(<local_dir>, repos = NULL, type = "source")`.

### Help

All functions are documented in the standard way, which means that once
you load the package using `library(PasswordGen)` you can just type
`?password` to see the help file.

### Example 1

Generate passwords and passphrases using function.

    library(PasswordGen)
    password()

    ## [1] "Graspless)478-WATERLEAF*805-Oversize/585"

The default version of the function is generating a string that
contains three words (lower and upper cases) plus some random symbols
and numbers. As mentioned above the words are borrowed from Webster's
Unabridged Dictionary.

### Example 2

We may want to change increase or decrease the complexity of our
passwords. We can do this by playing with the arguments of the function.

Let's generate a pass-phrase composed out of 5 words plus a random number
after each word, no symbols, and separate them using a dash line.

    password(no_words = 5,     # 5 word password
             numbers = TRUE,   # with numbers
             symbols = FALSE,  # without symbols
             sep = '-') # with a space between the words

    ## [1] "Decani695-ferret903-ANANGULAR910-CRYPTONYM135-ENTRICK647"

### Example 3

Generate a password using words from my favorite book.

Using the *gutenbergr* R package we will download a book and extract words from it.
"The Adventures of Sherlock Holmes" by Doyle, Arthur Conan is a good
choice.

    library(gutenbergr)
    library(stringr)
    # look up book and check the id on Gutenberg Project
    info <- gutenberg_works(str_detect(title, "The Adventures of Sherlock Holmes"))
    info$gutenberg_id # this should be 1661

    ## [1] 1661

    # download the book
    my_book <- gutenberg_download(gutenberg_id = 1661, meta_fields = "title",
                                 mirror = "http://gutenberg.pglaf.org")

Now we can extract the words using the function.

    words <- ExtractWords(data.r = my_book$text)
    words

    ## 
    ## File:  THE ADVENTURES OF SHERLOCK HOLMES.Rdata
    ## Number of unique words:  7739

    # Note, this book has significantly less unique words than Webster's Unabridged Dictionary. More words to choose from is better.

And generate the password:

    password(no_words = 5, data = words)

    ## [1] "exit<574@CURLING}347@Outhouse&118@CONCEIVE~418@STEPPING@979"

### Example 4

Extract words from other language than English and generate passwords.
Mihai Eminescu was a Romantic poet, often regarded as the most famous
and influential Romanian poet. Let's check his words.

    # check the id of the book on Gutenberg Project
    info <- gutenberg_works(languages = 'ro')

    # download the book
    my_book <- gutenberg_download(gutenberg_id = 35323, meta_fields = "title",
                                 mirror = "http://gutenberg.pglaf.org")

    # Extract words
    words <- ExtractWords(data.r = my_book$text)

    password(no_words = 4, data = words,
            numbers = TRUE, symbols = TRUE, sep = "_")

    ## [1] "BUBANI-749_Picioruse>518_Nemaigandita<914_SARAC)351"

Nice Romanian password!
