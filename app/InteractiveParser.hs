{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lexer
import Parser
import Syntax
import Token

import Control.Monad
import Data.Loc
import Language.Lexer.Applicative
import System.Environment
import System.Exit
import System.IO
import Text.Earley

main :: IO ()
main = getArgs >>= \case
    ["-b"] -> go blockGrammar
    ["-s"] -> go statementGrammar
    ["-e"] -> go expressionGrammar
    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " [-b | -e | -s]"
        hPutStrLn stderr $ "   (use block, statement, or expression grammar)"
        exitFailure

go :: Show a => (forall r. Grammar r String (Prod r String Token a)) -> IO ()
go grammar = forever $
    getLine
      >>= either print (\xs -> print $ fullParses $ parser grammar $ map unLoc xs)
          . streamToEitherList
          . runLexer luaLexer "stdin"
