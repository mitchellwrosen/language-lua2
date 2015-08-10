{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Instances where

import Syntax

import           Control.Applicative
import           Data.Char                  (isAsciiLower, isAsciiUpper, isDigit)
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import           Data.List.NonEmpty         (NonEmpty)
import qualified Data.List.NonEmpty         as NE
import           Data.Loc
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

instance Arbitrary a => Arbitrary (Ident a) where
    arbitrary = Ident <$> arbitrary <*> genIdent
      where
        genIdent :: Gen String
        genIdent = liftA2 (:) first rest `suchThat` \s -> not (HS.member s keywords)
          where
            first :: Gen Char
            first = frequency [(5, pure '_'), (95, arbitrary `suchThat` isAsciiLetter)]

            rest :: Gen String
            rest = listOf $
                frequency [ (10, pure '_')
                          , (45, arbitrary `suchThat` isAsciiLetter)
                          , (45, arbitrary `suchThat` isDigit)
                          ]

            -- Meh, forget unicode for now.
            isAsciiLetter :: Char -> Bool
            isAsciiLetter c = isAsciiLower c || isAsciiUpper c

        keywords :: HashSet String
        keywords = [ "and", "break", "do", "else", "elseif", "end", "false"
                   , "for", "function", "goto", "if", "in", "local", "nil"
                   , "not", "or", "repeat", "return", "then", "true", "until", "while"
                   ]

instance Arbitrary a => Arbitrary (Block a) where
    arbitrary = Block <$> arbitrary <*> listOf1 arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (Statement a) where
    arbitrary = oneof
        [ EmptyStmt      <$> arbitrary
        , Assign         <$> arbitrary <*> arbitrary <*> arbitrary
        , FunCall        <$> arbitrary <*> arbitrary
        , Label          <$> arbitrary <*> arbitrary
        , Break          <$> arbitrary
        , Goto           <$> arbitrary <*> arbitrary
        , Do             <$> arbitrary <*> arbitrary
        , While          <$> arbitrary <*> arbitrary <*> arbitrary
        , Repeat         <$> arbitrary <*> arbitrary <*> arbitrary
        , If             <$> arbitrary <*> arbitrary <*> arbitrary
        , For            <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , ForIn          <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , FunAssign      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , LocalFunAssign <$> arbitrary <*> arbitrary <*> arbitrary
        , LocalAssign    <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (ReturnStatement a) where
    arbitrary = ReturnStatement <$> arbitrary <*> arbitrary
    shrink = genericShrink


instance Arbitrary a => Arbitrary (Variable a) where
    arbitrary = oneof
        [ VarIdent     <$> arbitrary <*> arbitrary
        , VarField     <$> arbitrary <*> arbitrary <*> arbitrary
        , VarFieldName <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (Expression a) where
    arbitrary = oneof
        [ Nil       <$> arbitrary
        , Bool      <$> arbitrary <*> arbitrary
        , Integer   <$> arbitrary <*> (show <$> (arbitrary :: Gen Int)) -- TODO: Make these better
        , Float     <$> arbitrary <*> (show <$> (arbitrary :: Gen Float))
        , String    <$> arbitrary <*> arbitrary
        , Vararg    <$> arbitrary
        , FunDef    <$> arbitrary <*> arbitrary
        , PrefixExp <$> arbitrary <*> arbitrary
        , TableCtor <$> arbitrary <*> arbitrary
        , Binop     <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        , Unop      <$> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (PrefixExpression a) where
    arbitrary = oneof
        [ PrefixVar     <$> arbitrary <*> arbitrary
        , PrefixFunCall <$> arbitrary <*> arbitrary
        , Parens        <$> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (FunctionCall a) where
    arbitrary = oneof
        [ FunctionCall <$> arbitrary <*> arbitrary <*> arbitrary
        , MethodCall   <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (FunctionArgs a) where
    arbitrary = oneof
        [ Args       <$> arbitrary <*> arbitrary
        , ArgsTable  <$> arbitrary <*> arbitrary
        , ArgsString <$> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (FunctionBody a) where
    arbitrary = FunctionBody <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (TableConstructor a) where
    arbitrary = TableConstructor <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary a => Arbitrary (Field a) where
    arbitrary = oneof
        [ FieldExp   <$> arbitrary <*> arbitrary <*> arbitrary
        , FieldIdent <$> arbitrary <*> arbitrary <*> arbitrary
        , Field      <$> arbitrary <*> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (Binop a) where
    arbitrary = oneof
        [ Plus       <$> arbitrary
        , Minus      <$> arbitrary
        , Mult       <$> arbitrary
        , FloatDiv   <$> arbitrary
        , FloorDiv   <$> arbitrary
        , Exponent   <$> arbitrary
        , Modulo     <$> arbitrary
        , BitwiseAnd <$> arbitrary
        , BitwiseXor <$> arbitrary
        , BitwiseOr  <$> arbitrary
        , Rshift     <$> arbitrary
        , Lshift     <$> arbitrary
        , Concat     <$> arbitrary
        , Lt         <$> arbitrary
        , Leq        <$> arbitrary
        , Gt         <$> arbitrary
        , Geq        <$> arbitrary
        , Eq         <$> arbitrary
        , Neq        <$> arbitrary
        , And        <$> arbitrary
        , Or         <$> arbitrary
        ]

    shrink = genericShrink

instance Arbitrary a => Arbitrary (Unop a) where
    arbitrary = oneof
        [ Negate     <$> arbitrary
        , Not        <$> arbitrary
        , Length     <$> arbitrary
        , BitwiseNot <$> arbitrary
        ]

    shrink = genericShrink

-- Orphans

instance Arbitrary SrcLoc where
    arbitrary = pure (SrcLoc NoLoc)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = NE.fromList <$> listOf1 arbitrary
