module Main where

import Instances ()
import Lexer
import Parser
import Pretty
import Syntax
import Token

import           Data.Loc
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck       as QC

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

    l :: String -> [Token]
    l = map unLoc . streamToList . runLexer luaLexer ""

parserTests :: TestTree
parserTests = QC.testProperty "Pretty-printer round-trip" (\luaAst -> luaFromString (luaToString luaAst) == Just luaAst)

luaToString :: Chunk SrcLoc -> String
luaToString c = displayS (renderPretty 1.0 80 (pretty c)) ""

luaFromString :: String -> Maybe (Chunk SrcLoc)
luaFromString s = either (const Nothing) Just (streamToEitherList (runLexer luaLexer "" s)) >>= \tks ->
    case fullParses (parser luaBlock tks) of
        ([c], _) -> Just c
        _        -> Nothing
