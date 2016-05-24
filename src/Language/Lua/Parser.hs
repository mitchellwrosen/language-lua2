{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes         #-}

module Language.Lua.Parser
    ( -- * Lua parsers
      parseLua
    , parseLuaWith
    , LuaParseException(..)

      -- * Lua grammars
    , LuaGrammar
    , luaChunk
    , luaStatement
    , luaExpression
    , NodeInfo(..)
    , nodeLoc
    , nodeTokens

    -- * <https://hackage.haskell.org/package/Earley Earley> re-exports
    -- | These are provided if you want more control over parsing than what
    -- 'parseLua' or 'parseLuaWith' provides.
    , Report(..)
    , Result(..)
    , allParses
    , fullParses
    , parser
    , report
    ) where

import Language.Lua.Lexer
import Language.Lua.Parser.Internal
import Language.Lua.Syntax
import Language.Lua.Token

import Control.Exception   (Exception, mapException, throw)
import Data.Data
import Data.Functor        ((<$>))
import Data.List           (intercalate)
import Data.Loc
import Text.Earley

type LuaGrammar f = forall r. Grammar r (Prod r String (L Token) (f NodeInfo))

data LuaParseException
    = LuaLexException !Pos
    | LuaParseException !FilePath !(Report String [L Token])
    | LuaAmbiguousParseException !FilePath !(Report String [L Token])
    deriving (Eq, Typeable)

instance Show LuaParseException where
    show e =
        case e of
            LuaLexException pos -> "Unexpected token at " ++ displayPos pos
            LuaParseException filename r ->
                case r of
                    Report _ xs [] -> unlines
                        [ filename ++ ":"
                        , "    Expected one of: " ++ intercalate ", " xs
                        , "    But found: <EOF>"
                        ]
                    Report _ xs (L (Loc p _) tk : _) -> unlines
                        [ displayPos p ++ ":"
                        , "    Expected one of: " ++ intercalate ", " xs
                        , "    But found: '" ++ showToken tk ++ "'"
                        ]
            LuaAmbiguousParseException filename r ->
                let s = "Ambiguous parse. See <http://www.lua.org/manual/5.3/manual.html#3.3.1> for a likely explanation.\n"
                in case r of
                       Report _ xs [] -> unlines
                           [ filename ++ ":"
                           , "    " ++ s
                           , "    Expected one of: " ++ intercalate ", " xs
                           , "    But found: <EOF>"
                           ]
                       Report _ xs (L (Loc p _) tk : _) -> unlines
                           [ displayPos p ++ ":"
                           , "    " ++ s
                           , "    Expected one of: " ++ intercalate ", " xs
                           , "    But found: '" ++ showToken tk ++ "'"
                           ]

instance Exception LuaParseException

-- | Parse a Lua file. May throw 'LuaParseException'.
--
-- @
-- 'parseLua' = 'parseLuaWith' 'luaChunk'
-- @
parseLua
    :: String -- ^ Source filename (used in locations).
    -> String -- ^ Source contents.
    -> Chunk NodeInfo
parseLua = parseLuaWith luaChunk

-- | Parse Lua code with the given grammar. May throw 'LuaParseException'.
--
-- >>> parseLuaWith luaExprssion "" "5+5"
-- Binop
--     (NodeInfo
--         { nodeLoc    = Loc (Pos "" 1 1 0) (Pos "" 1 3 2)
--         , nodeTokens = fromList [TkIntLit "5",TkPlus,TkIntLit "5"]
--         })
--     (Plus
--         (NodeInfo
--             { nodeLoc    = Loc (Pos "" 1 2 1) (Pos "" 1 2 1)
--             , nodeTokens = fromList [TkPlus]
--             }))
--     (Integer
--         (NodeInfo
--             { nodeLoc    = Loc (Pos "" 1 1 0) (Pos "" 1 1 0)
--             , nodeTokens = fromList [TkIntLit "5"]
--             })
--         "5")
--     (Integer
--         (NodeInfo
--             { nodeLoc    = Loc (Pos "" 1 3 2) (Pos "" 1 3 2)
--             , nodeTokens = fromList [TkIntLit "5"]
--             })
--          "5")
--
-- All AST nodes are 'Functor's over their annotation:
--
-- >>> (() <$) <$> parseLuaWith luaExpression "" "5+5"
-- Binop () (Plus ()) (Integer () "5") (Integer () "5")
parseLuaWith
    :: LuaGrammar f -- ^ Grammar to parse with.
    -> String       -- ^ Source filename (used in locations).
    -> String       -- ^ Source contents.
    -> f NodeInfo
parseLuaWith g filename contents =
    let tokens = streamToList' (runLexer luaLexer filename contents)
    in case fullParses (parser g) tokens of
           ([x], _) -> x
           ([],  r) -> throw (LuaParseException filename r)
           (_,   r) -> throw (LuaAmbiguousParseException filename r)
  where
    -- Wrap a LexicalError in our own LuaParseException.
    streamToList' :: forall tok. TokenStream tok -> [tok]
    streamToList' = go . streamToList
      where
        go :: [tok] -> [tok]
        go ts = case mapException f ts of
                    [] -> []
                    (t:ts') -> t : go ts'

        f :: LexicalError -> LuaParseException
        f (LexicalError pos) = LuaLexException pos

-- | Grammar for a Lua chunk; i.e. a Lua compilation unit, defined as a list of
-- statements. This is the grammar you should use to parse real Lua code.
luaChunk :: LuaGrammar Chunk -- Grammar r String (Prod r String (L Token) (Block NodeInfo))
luaChunk = (\(x,_,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) <$> grammar

-- | Grammar for a single Lua statement. Mostly subsumed by 'luaChunk'.
luaStatement :: LuaGrammar Statement
luaStatement = (\(_,x,_,_,_,_,_,_,_,_,_,_,_,_,_) -> x) <$> grammar

-- | Grammar for a Lua expression. Provided for smaller REPL-like parsing that
-- operates only on expressions.
luaExpression :: LuaGrammar Expression
luaExpression = (\(_,_,_,_,_,x,_,_,_,_,_,_,_,_,_) -> x) <$> grammar
