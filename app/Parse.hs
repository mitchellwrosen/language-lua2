{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Lexer
import Parser
import Syntax
import Token

import           Control.Monad.ST
import           Data.Loc
import           Language.Lexer.Applicative
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Earley

main :: IO ()
main = getArgs >>= \case
    [file] -> readFile file >>=
        either print (\xs -> f $ fullParses $ parser blockGrammar xs)
        . streamToEitherList
        . runLexer luaLexer file
      where
        f :: ([Block SrcLoc], Report String [L Token]) -> IO ()
        f ([x], _) = print x
        f ([], r)  = putStrLn ("Parse error: " ++ show r)
        f (xs, r)  = do
            putStrLn "Ambiguous grammar."
            mapM_ print xs
            print r
    _ -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " <filename>"
        exitFailure
