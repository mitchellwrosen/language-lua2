{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Language.Lua.Parser

import Control.Monad
import Options.Applicative
import Text.PrettyPrint.Leijen    (Pretty(..), displayS, renderPretty)

data GrammarMode = ChunkMode | StatementMode | ExpressionMode
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
    mode = ChunkMode      <$ switch (long "chunk")
       <|> StatementMode  <$ switch (long "statement")
       <|> ExpressionMode <$ switch (long "expression")

main' :: Opts -> IO ()
main' Opts{..} =
    case optsFile of
        Just file -> readFile file >>= go luaChunk file
        Nothing ->
            case optsInteractive of
                ChunkMode      -> forever $ getLine >>= go luaChunk      "<stdin>"
                StatementMode  -> forever $ getLine >>= go luaStatement  "<stdin>"
                ExpressionMode -> forever $ getLine >>= go luaExpression "<stdin>"
  where
    go :: (Show (f NodeInfo), Pretty (f NodeInfo)) => LuaGrammar f -> String -> String -> IO ()
    go g filename contents = do
        let x = parseLuaWith g filename contents
        if optsPretty
             then putStrLn $ displayS (renderPretty 1.0 80 (pretty x)) ""
             else print x
