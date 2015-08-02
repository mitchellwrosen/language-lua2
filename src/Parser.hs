module Parser
    ( blockGrammar
    , expressionGrammar
    , statementGrammar
    ) where

import Syntax
import Token

import Control.Applicative
import Data.Maybe          (fromMaybe, isJust)
import Prelude             hiding (break, repeat, until)
import Text.Earley

type P r a = Prod r String Token a

blockGrammar :: Grammar r String (P r Block)
blockGrammar = (\(a,_,_) -> a) <$> grammar

statementGrammar :: Grammar r String (P r Statement)
statementGrammar = (\(_,b,_) -> b) <$> grammar

expressionGrammar :: Grammar r String (P r Expression)
expressionGrammar = (\(_,_,c) -> c) <$> grammar

grammar :: Grammar r String (P r Block, P r Statement, P r Expression)
grammar = mdo
    block :: P r Block <- rule $ Block
        <$> many statement
        <*> (blockReturnStatement <|> pure [])

    statement :: P r Statement <- rule $
            SSemi <$ semi
        <|> SAssign
            <$> varList1 <* eq
            <*> expressionList1
        <|> SFunctionCall
            <$> prefixExpression
            <*> optional (comma *> ident)
            <*> functionArgs
        <|> SLabel <$> (doubleColon *> ident <* doubleColon)
        <|> SBreak <$ break
        <|> SGoto <$> (goto *> ident)
        <|> SDo <$> (do' *> block <* end)
        <|> SWhile
            <$> (while *> expression <* do')
            <*> block
            <*  end
        <|> SRepeat
            <$> (repeat *> block)
            <*> (until *> expression)
        <|> SIf
            <$> (if' *> expression)
            <*> (then' *> block)
            <*> many ((,)
                <$> (elseif *> expression)
                <*> (then' *> block))
            <*> optional (else' *> block)
            <*  end
        <|> SFor
            <$> (for *> ident)
            <*> (eq *> expression)
            <*> (comma *> expression)
            <*> optional (comma *> expression)
            <*> (do' *> block)
            <*  end
        <|> SForIn
            <$> (for *> nameList1)
            <*> (in' *> expressionList1)
            <*> (do' *> block)
            <*  end
        <|> (\(a,b,c,(d,e,f)) -> SFunctionDef a b c d e f) <$> ((,,,)
            <$> (function *> ident)
            <*> (ident `sepBy` dot)
            <*> optional (colon *> ident)
            <*> functionBody)
        <|> (\(a,(b,c,d)) -> SLocalFunction a b c d) <$> ((,)
            <$> (local *> function *> ident)
            <*> functionBody)
        <|> SLocalAssign
            <$> (local *> nameList1)
            <*> ((eq *> expressionList1) <|> pure [])

    blockReturnStatement :: P r [Expression] <- rule $
        return' *> expressionList <* optional semi

    varList1 :: P r [Variable] <- rule $
        var `sepBy1` comma

    var :: P r Variable <- rule $
            VIdent <$> ident
        <|> VIndex
            <$> prefixExpression
            <*> brackets expression
        <|> VIndex
            <$> prefixExpression
            <*> (EStringLit . unIdent <$> (dot *> ident))

    expressionList :: P r [Expression] <- rule $
        expression `sepBy` comma

    expressionList1 :: P r [Expression] <- rule $
        expression `sepBy1` comma

    -- TODO: Binary operator precedence rules and associativities
    expression :: P r Expression <- rule $
            ENil <$ nil
        <|> ETrue <$ true
        <|> EFalse <$ false
        <|> EIntLit <$> intLit
        <|> EFloatLit <$> floatLit
        <|> EStringLit <$> stringLit
        <|> ETripleDot <$ tripleDot
        <|> (\(a,b,c) -> EFunctionDef a b c) <$> (function *> functionBody)
        <|> EPrefixExp <$> prefixExpression
        <|> ETableConstructor <$> tableConstructor
        <|> EBinop
            <$> expression
            <*> binop
            <*> expression
        <|> EUnop
            <$> unop
            <*> expression

    prefixExpression :: P r PrefixExpression <- rule $
            PEVar <$> var
        <|> PEFunctionCall
            <$> prefixExpression
            <*> optional (comma *> ident)
            <*> functionArgs
        <|> (PEExpr <$> parens expression)

    functionArgs :: P r FunctionArgs <- rule $
            FAExprs <$> parens expressionList
        <|> FATableConstructor <$> tableConstructor
        <|> FAStringLit <$> stringLit

    functionBody :: P r ([Ident], Bool, Block) <- rule $ (\(a,b) c -> (a,b,c))
        <$> parens (fromMaybe ([],False) <$> optional parList)
        <*> block
        <*  end

    tableConstructor :: P r [Field] <- rule $
        let sep = comma <|> semi
        in braces $ sepBy field sep <* optional sep

    field :: P r Field <- rule $
            FExprAssign
            <$> brackets expression
            <*> (eq *> expression)
        <|> FIdentAssign
            <$> ident
            <*> (eq *> expression)
        <|> FExpr <$> expression

    return (block, statement, expression)

--------------------------------------------------------------------------------
-- Non-recursive non-terminals

nameList1 :: P r [Ident]
nameList1 = ident `sepBy` comma

parList :: P r ([Ident], Bool)
parList = withNames <|> withoutNames
  where
    withNames :: P r ([Ident], Bool)
    withNames = (,)
        <$> nameList1
        <*> (isJust <$> optional (comma *> tripleDot))

    withoutNames :: P r ([Ident], Bool)
    withoutNames = ([],True) <$ tripleDot

binop :: P r Binop
binop =
        (BPlus         <$ symbol TkPlus         <?> "+")
    <|> (BDash         <$ symbol TkDash         <?> "-")
    <|> (BStar         <$ symbol TkStar         <?> "*")
    <|> (BFslash       <$ symbol TkFslash       <?> "/")
    <|> (BDoubleFslash <$ symbol TkDoubleFslash <?> "//")
    <|> (BCarrot       <$ symbol TkCarrot       <?> "^")
    <|> (BPct          <$ symbol TkPct          <?> "%")
    <|> (BAmp          <$ symbol TkAmp          <?> "&")
    <|> (BTilde        <$ symbol TkTilde        <?> "~")
    <|> (BPipe         <$ symbol TkPipe         <?> "|")
    <|> (BRShift       <$ symbol TkRShift       <?> ">>")
    <|> (BLShift       <$ symbol TkLShift       <?> "<<")
    <|> (BDoubleDot    <$ symbol TkDoubleDot    <?> "..")
    <|> (BLt           <$ symbol TkLt           <?> "<")
    <|> (BLeq          <$ symbol TkLeq          <?> "<=")
    <|> (BGt           <$ symbol TkGt           <?> ">")
    <|> (BGeq          <$ symbol TkGeq          <?> ">=")
    <|> (BDoubleEq     <$ symbol TkDoubleEq     <?> "==")
    <|> (BNeq          <$ symbol TkNeq          <?> "~=")
    <|> (BAnd          <$ symbol TkAnd          <?> "and")
    <|> (BOr           <$ symbol TkOr           <?> "or")

unop :: P r Unop
unop =
        (UDash  <$ symbol TkDash  <?> "-")
    <|> (UNot   <$ symbol TkNot   <?> "not")
    <|> (UHash  <$ symbol TkHash  <?> "#")
    <|> (UTilde <$ symbol TkTilde <?> "~")

--------------------------------------------------------------------------------
-- Combinators

between :: Applicative f => f left -> f right -> f a -> f a
between left right f = left *> f <* right

braces :: P r a -> P r a
braces = between lbrace rbrace

brackets :: P r a -> P r a
brackets = between lbracket rbracket

parens :: P r a -> P r a
parens = between lparen rparen

sepBy :: Alternative f => f a -> f sep -> f [a]
sepBy f sep = sepBy1 f sep <|> pure []

sepBy1 :: Alternative f => f a -> f sep -> f [a]
sepBy1 f sep = (:) <$> f <*> many (sep *> f)

--------------------------------------------------------------------------------
-- Token productions

break :: P r Token
break = symbol TkBreak <?> "break"

colon :: P r Token
colon = symbol TkColon <?> ":"

comma :: P r Token
comma = symbol TkComma <?> ","

do' :: P r Token
do' = symbol TkDo <?> "do"

dot :: P r Token
dot = symbol TkDot <?> "dot"

doubleColon :: P r Token
doubleColon = symbol TkDoubleColon <?> "::"

else' :: P r Token
else' = symbol TkElse <?> "else"

elseif :: P r Token
elseif = symbol TkElseif <?> "elseif"

end :: P r Token
end = symbol TkEnd <?> "end"

eq :: P r Token
eq = symbol TkEq <?> "="

false :: P r Token
false = symbol TkFalse <?> "false"

floatLit :: P r String
floatLit = (\(TkFloatLit s) -> s) <$> satisfy isFloatLit <?> "float literal"
  where
    isFloatLit :: Token -> Bool
    isFloatLit (TkFloatLit _) = True
    isFloatLit _ = False

for :: P r Token
for = symbol TkFor <?> "for"

function :: P r Token
function = symbol TkFunction <?> "function"

goto :: P r Token
goto = symbol TkGoto <?> "goto"

ident :: P r Ident
ident = (\(TkIdent s) -> Ident s) <$> satisfy isIdent <?> "ident"
  where
    isIdent :: Token -> Bool
    isIdent (TkIdent _) = True
    isIdent _ = False

if' :: P r Token
if' = symbol TkIf <?> "if"

in' :: P r Token
in' = symbol TkIn <?> "in"

intLit :: P r String
intLit = (\(TkIntLit s) -> s) <$> satisfy isIntLit <?> "int literal"
  where
    isIntLit :: Token -> Bool
    isIntLit (TkIntLit _) = True
    isIntLit _ = False

lbrace :: P r Token
lbrace = symbol TkLBrace <?> "{"

lbracket :: P r Token
lbracket = symbol TkLBracket <?> "["

local :: P r Token
local = symbol TkLocal <?> "local"

lparen :: P r Token
lparen = symbol TkLParen <?> "("

nil :: P r Token
nil = symbol TkNil <?> "nil"

rbrace :: P r Token
rbrace = symbol TkRBrace <?> "}"

rbracket :: P r Token
rbracket = symbol TkRBracket <?> "]"

repeat :: P r Token
repeat = symbol TkRepeat <?> "repeat"

return' :: P r Token
return' = symbol TkReturn <?> "return"

rparen :: P r Token
rparen = symbol TkRParen <?> ")"

semi :: P r Token
semi = symbol TkSemi <?> ";"

stringLit :: P r String
stringLit = (\(TkStringLit s) -> s) <$> satisfy isStringLit <?> "string literal"
  where
    isStringLit :: Token -> Bool
    isStringLit (TkStringLit _) = True
    isStringLit _ = False

then' :: P r Token
then' = symbol TkThen <?> "then"

tripleDot :: P r Token
tripleDot = symbol TkTripleDot <?> "..."

true :: P r Token
true = symbol TkTrue <?> "true"

until :: P r Token
until = symbol TkUntil <?> "until"

while :: P r Token
while = symbol TkWhile <?> "while"
