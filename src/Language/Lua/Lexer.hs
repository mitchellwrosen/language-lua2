module Language.Lua.Lexer
    ( luaLexer

    -- * <https://hackage.haskell.org/package/lexer-applicative lexer-applicative> re-exports
    , LexicalError(..)
    , TokenStream(..)
    , runLexer
    , streamToList
    , streamToEitherList
    ) where

import Language.Lua.Token

import           Data.Char                  (chr, isDigit, isHexDigit, isSpace)
import           Data.List                  (foldl')
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid
import           Language.Lexer.Applicative
import           Numeric                    (readHex)
import           Text.Regex.Applicative

-- |
-- @
-- lex :: String -> [L Token]
-- lex = 'streamToList' . 'runLexer' 'luaLexer' ""
-- @
--
-- >>> lex "5+5"
-- [TkIntLit "5",TkPlus,TkIntLit "5"]
-- >>> lex "foo?"
-- [TkIdent "foo"*** Exception: Lexical error at :1:4
luaLexer :: Lexer Token
luaLexer = luaTokens <> luaWhitespace

luaTokens :: Lexer Token
luaTokens = token (longest luaToken)
         <> token (longestShortest luaLongBracketStringLitPrefix luaLongBracketStringLitSuffix)

luaToken :: RE Char Token
luaToken =
        TkAnd          <$ "and"
    <|> TkBreak        <$ "break"
    <|> TkDo           <$ "do"
    <|> TkElse         <$ "else"
    <|> TkElseif       <$ "elseif"
    <|> TkEnd          <$ "end"
    <|> TkFalse        <$ "false"
    <|> TkFor          <$ "for"
    <|> TkFunction     <$ "function"
    <|> TkGoto         <$ "goto"
    <|> TkIf           <$ "if"
    <|> TkIn           <$ "in"
    <|> TkLocal        <$ "local"
    <|> TkNil          <$ "nil"
    <|> TkNot          <$ "not"
    <|> TkOr           <$ "or"
    <|> TkRepeat       <$ "repeat"
    <|> TkReturn       <$ "return"
    <|> TkThen         <$ "then"
    <|> TkTrue         <$ "true"
    <|> TkUntil        <$ "until"
    <|> TkWhile        <$ "while"
    <|> TkEq           <$ "="
    <|> TkNeq          <$ "~="
    <|> TkPlus         <$ "+"
    <|> TkDash         <$ "-"
    <|> TkStar         <$ "*"
    <|> TkFslash       <$ "/"
    <|> TkPct          <$ "%"
    <|> TkCarrot       <$ "^"
    <|> TkHash         <$ "#"
    <|> TkAmp          <$ "&"
    <|> TkPipe         <$ "|"
    <|> TkLt           <$ "<"
    <|> TkGt           <$ ">"
    <|> TkLShift       <$ "<<"
    <|> TkRShift       <$ ">>"
    <|> TkDoubleFslash <$ "//"
    <|> TkDoubleEq     <$ "=="
    <|> TkLeq          <$ "<="
    <|> TkGeq          <$ ">="
    <|> TkLParen       <$ "("
    <|> TkRParen       <$ ")"
    <|> TkLBrace       <$ "{"
    <|> TkRBrace       <$ "}"
    <|> TkLBracket     <$ "["
    <|> TkRBracket     <$ "]"
    <|> TkSemi         <$ ";"
    <|> TkColon        <$ ":"
    <|> TkComma        <$ ","
    <|> TkDoubleColon  <$ "::"
    <|> TkDot          <$ "."
    <|> TkDoubleDot    <$ ".."
    <|> TkTripleDot    <$ "..."
    <|> TkTilde        <$ "~"
    <|> TkQuote        <$ "'"
    <|> TkDoubleQuote  <$ "\""
    <|> TkIdent        <$> luaIdentifier
    <|> TkStringLit    <$> luaStringLit
    <|> TkFloatLit     <$> luaFloatLit
    <|> TkIntLit       <$> luaIntLit

luaIdentifier :: RE Char String
luaIdentifier = (:)
    <$> psym identFirst
    <*> many (psym identRest)
  where
    identFirst :: Char -> Bool
    identFirst = (||) <$> (== '_') <*> isAlpha

    identRest :: Char -> Bool
    identRest = (||) <$> identFirst <*> isDigit

luaStringLit :: RE Char String
luaStringLit = singleQuoted <|> doubleQuoted
  where
    singleQuoted :: RE Char String
    singleQuoted = quoted '\''

    doubleQuoted :: RE Char String
    doubleQuoted = quoted '\"'

    quoted :: Char -> RE Char String
    quoted c = between c c $ many (escapeSequence <|> not_c)
      where
        not_c :: RE Char Char
        not_c = psym (/= c)

    escapeSequence :: RE Char Char
    escapeSequence = sym '\\' *> sequences
      where
        sequences :: RE Char Char
        sequences =
                '\a' <$ sym 'a'
            <|> '\b' <$ sym 'b'
            <|> '\f' <$ sym 'f'
            <|> '\n' <$ sym 'n'
            <|> '\r' <$ sym 'r'
            <|> '\t' <$ sym 't'
            <|> '\v' <$ sym 'v'
            <|>         sym '\\'
            <|>         sym '"'
            <|>         sym '\''
            <|>         sym '\n'
            <|> hexEscape
            <|> decimalEscape
            -- TODO: unicode escape
            -- TODO: \z
          where
            hexEscape :: RE Char Char
            hexEscape = go <$> (sym 'x' *> hexDigit) <*> hexDigit
              where
                go :: Char -> Char -> Char
                go x y = let [(n,"")] = readHex [x,y] in chr n

            decimalEscape :: RE Char Char
            decimalEscape = chr . read <$> betweenN 1 3 digit

            -- unicodeEscape :: RE Char String
            -- unicodeEscape = (\a b c -> a ++ b ++ [c])
            --     <$> "u{"
            --     <*> some hexDigit
            --     <*> sym '}'

luaIntLit :: RE Char String
luaIntLit = hexLit <|> some digit
  where
    hexLit :: RE Char String
    hexLit = (\a b c -> a:b:c)
        <$> sym '0'
        <*> oneOf "xX"
        <*> some hexDigit

luaFloatLit :: RE Char String
luaFloatLit = hexLit <|> decimalLit
  where
    hexLit :: RE Char String
    hexLit = (\a b cs ds -> a : b : cs ++ ds)
        <$> sym '0'
        <*> oneOf "xX"
        <*> some hexDigit
        <*> andOr (fractionalSuffix hexDigit) (exponentPart "pP")

    decimalLit :: RE Char String
    decimalLit = (++) <$> some digit <*> andOr (fractionalSuffix digit) (exponentPart "eE")

    exponentPart :: String -> RE Char String
    exponentPart cs = f
        <$> oneOf cs
        <*> optional (oneOf "-+")
        <*> some digit -- Yes, digit, even for binaryExponent: 0x0p+A is invalid
      where
        f :: Char -> Maybe Char -> String -> String
        f x Nothing  zs = x:zs
        f x (Just y) zs = x:y:zs

    fractionalSuffix :: RE Char Char -> RE Char String
    fractionalSuffix c = (:) <$> sym '.' <*> many c

luaLongBracketStringLitPrefix :: RE Char String
luaLongBracketStringLitPrefix = between '[' '[' (many (sym '='))

luaLongBracketStringLitSuffix :: String -> RE Char Token
luaLongBracketStringLitSuffix eqs = TkStringLit <$>
    -- "For convenience, when the opening long bracket is immediately followed
    -- by a newline, the newline is not included in the string."
    (optional (sym '\n') *> many anySym <* between ']' ']' (exactlyN (length eqs) (sym '=')))

luaWhitespace :: Lexer Token
luaWhitespace = mconcat
    [ whitespace $ longest (psym isSpace)
    , whitespace $ longestShortest luaBlockCommentPrefix luaBlockCommentSuffix
    , whitespace $ longest luaLineComment
    ]

luaBlockCommentPrefix :: RE Char String
luaBlockCommentPrefix = "--[" *> many (sym '=') <* sym '['

luaBlockCommentSuffix :: String -> RE Char Char
luaBlockCommentSuffix xs = many anySym *> "--]" *> exactlyN (length xs) (sym '=') *> sym ']'

luaLineComment :: RE Char String
luaLineComment = "--" *> many (psym (/= '\n'))

--------------------------------------------------------------------------------
-- Regex extras

oneOf :: Eq s => [s] -> RE s s
oneOf = foldl' (\acc x -> acc <|> sym x) empty

-- `andOr f x y` will succeed on `x`, `y`, and `xy`
andOr :: forall m. Monoid m => RE Char m -> RE Char m -> RE Char m
andOr rx ry = ry <|> liftA2 (\x my -> x <> fromMaybe mempty my) rx (optional ry)

between :: Char -> Char -> RE Char a -> RE Char a
between x y z = sym x *> z <* sym y

hexDigit :: RE Char Char
hexDigit = psym isHexDigit

digit :: RE Char Char
digit = psym isDigit

--------------------------------------------------------------------------------
-- Char extras

isAlpha :: Char -> Bool
isAlpha = (||) <$> isLowercase <*> isUppercase

isLowercase :: Char -> Bool
isLowercase = charBetween 'a' 'z'

isUppercase :: Char -> Bool
isUppercase = charBetween 'A' 'Z'

charBetween :: Char -> Char -> Char -> Bool
charBetween x y = (&&) <$> (>= x) <*> (<= y)

--------------------------------------------------------------------------------
-- Applicative/Alternative extras

exactlyN :: Alternative f => Int -> f a -> f [a]
exactlyN 0 _ = pure []
exactlyN 1 f = pure <$> f
exactlyN n f = (:) <$> f <*> exactlyN (n-1) f

-- Inclusive
betweenN :: Alternative f => Int -> Int -> f a -> f [a]
betweenN x y _ | x > y  = error "betweenN: x > y"
betweenN 0 y f          = upToN y f
betweenN x y f          = liftA2 (++) (exactlyN x f) (upToN (y-x) f)

-- Inclusive
upToN :: Alternative f => Int -> f a -> f [a]
upToN 0 _ = pure []
upToN 1 f = fmap pure f <|> pure []
upToN n f = let g = upToN (n-1) f
            in liftA2 (:) f g <|> g
