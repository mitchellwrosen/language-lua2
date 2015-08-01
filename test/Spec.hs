module Main where

import Lexer
import Token

import Data.Loc (unLoc)
import Language.Lexer.Applicative
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests" [lexerTests]

lexerTests :: TestTree
lexerTests = testGroup "lexer tests"
    [ stringLiteralTests
    , intLiteralTests
    , floatLiteralTests
    ]
  where
    stringLiteralTests :: TestTree
    stringLiteralTests = testCase "string literals" $ do
        let expected = [TkStringLit "alo\n123\""]
        lex "\'alo\\\n123\"\'"        @?= expected
        lex "\"alo\\\n123\\\"\""      @?= expected
        lex "\'\\97lo\\10\\04923\"\'" @?= expected
        lex "[[alo\n123\"]]"          @?= expected
        lex "[==[\nalo\n123\"]==]"    @?= expected

    intLiteralTests :: TestTree
    intLiteralTests = testCase "int literals" $ do
        lex "3"        @?= [TkIntLit "3"]
        lex "345"      @?= [TkIntLit "345"]
        lex "0xff"     @?= [TkIntLit "0xff"]
        lex "0XBEBada" @?= [TkIntLit "0XBEBada"]

    floatLiteralTests :: TestTree
    floatLiteralTests = testCase "float literals" $ do
        lex "3."        @?= [TkFloatLit "3."]
        lex "3.1416"    @?= [TkFloatLit "3.1416"]
        lex "3.14e-2"   @?= [TkFloatLit "3.14e-2"]
        lex "3.14E+2"   @?= [TkFloatLit "3.14E+2"]
        lex "3.14E2"    @?= [TkFloatLit "3.14E2"]
        lex "0x0.1E"    @?= [TkFloatLit "0x0.1E"]
        lex "0xA23p-4"  @?= [TkFloatLit "0xA23p-4"]
        lex "0X1.92P+1" @?= [TkFloatLit "0X1.92P+1"]
        lex "0X1.92p1"  @?= [TkFloatLit "0X1.92p1"]

    lex :: String -> [Token]
    lex = map unLoc . streamToList . runLexer luaLexer ""
