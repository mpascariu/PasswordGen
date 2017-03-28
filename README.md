# PasswordGen

Sometimes we need not only secure passwords but also passwords easy to remember.
PasswordGen is an R package that helps you generates passwords and passphrases 
easy to remember with an algorithm based on the dictionary method. 
 
The default ```password()``` function generates random passwords using words from 
[The Project Gutenberg EBook of Webster's Unabridged Dictionary, 
by Various](http://www.gutenberg.org/ebooks/29765). In total 93077 words are used.
However, you can use the ```ExtractWords()``` function to extract all the words from 
any book and use them in the ```password()``` generator. 
Check the examples provided in the help files of the package.   


Installation
============

1. Make sure you have the most recent version of R
2. Run the following code in your R console 

```r
# install.packages("devtools")

library(devtools)
install_github("mpascariu/PasswordGen")
```

Help
===============
All functions are documented in the standard way, which means that 
once you load the package using ```library(PasswordGen)```
you can just type ```?password``` to see the help file. 

