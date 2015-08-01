module Main where

import Lexer
import Token

import Control.Monad
import Language.Lexer.Applicative

main :: IO ()
main = forever $ getLine >>= print . streamToList . runLexer luaLexer "stdin"
