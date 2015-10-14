{-# LANGUAGE CPP #-}

module Main where

import Instances ()
import Language.Lua.Lexer
import Language.Lua.Parser
import Language.Lua.Pretty
import Language.Lua.Syntax
import Language.Lua.Token

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$))
#endif
import           Control.DeepSeq       (rnf)
import           Control.Exception     (evaluate)
import           Data.Loc
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ lexerTests
    , parserTests
    ]

lexerTests :: TestTree
lexerTests = testGroup "lexer tests"
    [ stringLiteralTests
    , intLiteralTests
    , floatLiteralTests
    , commentTests
    ]
  where
    stringLiteralTests :: TestTree
    stringLiteralTests = testCase "string literals" $ do
        let tk = [TkStringLit "alo\n123\""]
        l "\'alo\\\n123\"\'"        @?= tk
        l "\"alo\\\n123\\\"\""      @?= tk
        l "\'\\97lo\\10\\04923\"\'" @?= tk
        l "[[alo\n123\"]]"          @?= tk
        l "[==[\nalo\n123\"]==]"    @?= tk

    intLiteralTests :: TestTree
    intLiteralTests = testCase "int literals" $ do
        l "3"        @?= [TkIntLit "3"]
        l "345"      @?= [TkIntLit "345"]
        l "0xff"     @?= [TkIntLit "0xff"]
        l "0XBEBada" @?= [TkIntLit "0XBEBada"]

    floatLiteralTests :: TestTree
    floatLiteralTests = testCase "float literals" $ do
        l "3."        @?= [TkFloatLit "3."]
        l "3.1416"    @?= [TkFloatLit "3.1416"]
        l "3.14e-2"   @?= [TkFloatLit "3.14e-2"]
        l "3.14E+2"   @?= [TkFloatLit "3.14E+2"]
        l "3.14E2"    @?= [TkFloatLit "3.14E2"]
        l "0x0.1E"    @?= [TkFloatLit "0x0.1E"]
        l "0xA23p-4"  @?= [TkFloatLit "0xA23p-4"]
        l "0X1.92P+1" @?= [TkFloatLit "0X1.92P+1"]
        l "0X1.92p1"  @?= [TkFloatLit "0X1.92p1"]

    commentTests :: TestTree
    commentTests = testCase "comments" $ do
        l "--"                     @?= []
        l "--just a comment"       @?= []
        l "-- just a comment"      @?= []
        l "3 -- hi"                @?= [TkIntLit "3"]
        l "3 --[ hi"               @?= [TkIntLit "3"]
        l "3 --[\nhi"              @?= [TkIntLit "3", TkIdent "hi"]
        l "3 --[[hello\nworld]] 4" @?= [TkIntLit "3", TkIntLit "4"]

    l :: String -> [Token]
    l = map unLoc . streamToList . runLexer luaLexer ""

parserTests :: TestTree
parserTests = testGroup "parser tests"
    [ testCase "parse sample.lua" $ do
          contents <- readFile "sample.lua"
          evaluate (rnf (parseLua "sample.lua" contents))

    -- TODO: Make smarter shrinks and re-enable
    -- , QC.testProperty "Pretty-printer round-trip" (\luaAst -> luaFromString (luaToString luaAst) == Just luaAst)
    ]

luaToString :: Chunk () -> String
luaToString c = displayS (renderPretty 1.0 80 (pretty c)) ""

luaFromString :: String -> Maybe (Chunk ())
luaFromString s = either (const Nothing) Just (streamToEitherList (runLexer luaLexer "" s)) >>= \tks ->
    case fullParses (parser luaChunk) tks of
        ([c], _) -> Just (() <$ c)
        _        -> Nothing
