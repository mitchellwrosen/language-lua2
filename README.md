[![Build Status](https://travis-ci.org/mitchellwrosen/language-lua2.svg?branch=master)](https://travis-ci.org/mitchellwrosen/language-lua2)

##### Unmaintained!

This parser works but is extremely slow and memory-intensive. You should
probably use [language-lua](https://hackage.haskell.org/package/language-lua)
instead.

The main impetus behind this project was style-checking lua code, which requires
token information to stick around after parsing.

Currently, this parser tags each AST node with the list of tokens used to
produce it. Instead, the parser should simply tag each AST node with the
indicies of the first and last token used to produce it, along with a vector
of tokens.
