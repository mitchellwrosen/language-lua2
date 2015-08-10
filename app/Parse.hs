{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lexer
import Parser
import Token

import Control.Monad
import Data.Loc
import Language.Lexer.Applicative
import Options.Applicative
import System.IO                  (stdout)
import Text.Earley
import Text.PrettyPrint.Leijen    (Pretty(..), displayIO, renderPretty)

data GrammarMode = BlockMode | StatementMode | ExpressionMode
    deriving Show

data Opts = Opts
    { optsInteractive :: GrammarMode
    , optsPretty      :: Bool
    , optsFile        :: Maybe FilePath
    } deriving Show

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> p)
        ( fullDesc
       <> progDesc "Parse a lua file, or parse interactively"
       <> header "Lua parser")

    p :: Parser Opts
    p = Opts
        <$> (mode <|> pure ExpressionMode)
        <*> switch
            ( long "pretty"
           <> help "Pretty-print output")
        <*> optional (strArgument
            ( metavar "FILE"
           <> help "Lua file to parse"))

    mode :: Parser GrammarMode
    mode = BlockMode      <$ switch (long "block")
       <|> StatementMode  <$ switch (long "statement")
       <|> ExpressionMode <$ switch (long "expression")

main' :: Opts -> IO ()
main' Opts{..} =
    case optsFile of
        Just file -> readFile file >>= go luaBlock file
        Nothing ->
            case optsInteractive of
                BlockMode      -> forever $ getLine >>= go luaBlock      "<stdin>"
                StatementMode  -> forever $ getLine >>= go luaStatement  "<stdin>"
                ExpressionMode -> forever $ getLine >>= go luaExpression "<stdin>"
  where
    go :: forall a. (Show a, Pretty a) => (forall r. Grammar r String (Prod r String (L Token) a)) -> String -> String -> IO ()
    go g filename =
        either print (\xs -> f $ fullParses $ parser g xs)
        . streamToEitherList
        . runLexer luaLexer filename
      where
        f :: ([a], Report String [L Token]) -> IO ()
        f ([x], _) = if optsPretty
                         then displayIO stdout (renderPretty 1.0 80 (pretty x))
                         else print x
        f ([], r)  = putStrLn ("Parse error: " ++ show r)
        f (xs, r)  = do
            putStrLn "Ambiguous grammar."
            mapM_ print xs
            print r
