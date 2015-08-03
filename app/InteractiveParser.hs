{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lexer
import Parser
import Syntax
import Token

import Control.Monad
import Data.Loc
import Language.Lexer.Applicative
import System.Environment
import Text.Earley

main :: IO ()
main = getArgs >>= \case
    ["-b"] -> go blockGrammar
    ["-s"] -> go statementGrammar
    _ -> do
        putStrLn "Using expression grammar. Use -s for statement, or -b for block"
        go expressionGrammar

go :: forall a. Show a => (forall r. Grammar r String (Prod r String (L Token) a)) -> IO ()
go grammar = forever $
    getLine
      >>= either print (\xs -> f $ fullParses $ parser grammar xs)
          . streamToEitherList
          . runLexer luaLexer "stdin"
  where
    f :: ([a], Report String [L Token]) -> IO ()
    f ([x], _) = print x
    f ([], r)  = putStrLn ("Parse error: " ++ show r)
    f (xs, r)  = do
        putStrLn "Ambiguous grammar."
        mapM_ print xs
        print r
