module Parser
    ( blockGrammar
    , expressionGrammar
    , statementGrammar
    ) where

import Syntax
import Token

import Control.Applicative
import Data.Loc
import Data.Maybe          (fromMaybe, isJust)
import Data.Monoid
import Prelude             hiding (break, repeat, until)
import Text.Earley
import Text.Earley.Mixfix
import Text.Printf         (printf)

type P r a = Prod r String (L Token) a

blockGrammar :: Grammar r String (P r (L Block))
blockGrammar = (\(a,_,_) -> a) <$> grammar

statementGrammar :: Grammar r String (P r (L Statement))
statementGrammar = (\(_,b,_) -> b) <$> grammar

expressionGrammar :: Grammar r String (P r (L Expression))
expressionGrammar = (\(_,_,c) -> c) <$> grammar

grammar :: Grammar r String (P r (L Block), P r (L Statement), P r (L Expression))
grammar = mdo
    block :: P r (L Block) <- rule $
        (\a b -> L (locOf a <> locOf b) (Block a (maybe [] unLoc b)))
            <$> many statement
            <*> optional blockReturnStatement

    statement :: P r (L Statement) <- rule $
            SSemi <$$ semi
        <|> (\a b -> L (locOf a <> locOf b) (SAssign a b))
            <$> varList1
            <*  eq
            <*> expressionList1
        <|> (\a b c -> L (locOf a <> locOf b <> locOf c) (SFunctionCall a b c))
            <$> prefixExpression
            <*> optional (comma *> ident)
            <*> functionArgs
        <|> (\a b c -> L (locOf a <> locOf c) (SLabel b))
            <$> doubleColon
            <*> ident
            <*> doubleColon
        <|> SBreak <$$ break
        <|> (\a b -> L (locOf a <> locOf b) (SGoto b))
            <$> goto
            <*> ident
        <|> (\a b c -> L (locOf a <> locOf c) (SDo b))
            <$> do'
            <*> block
            <*> end
        <|> (\a b c d -> L (locOf a <> locOf d) (SWhile b c))
            <$> while
            <*> expression
            <*  do'
            <*> block
            <*> end
        <|> (\a b c -> L (locOf a <> locOf c) (SRepeat b c))
            <$> repeat
            <*> block
            <*  until
            <*> expression
        <|> (\a b c d e f -> L (locOf a <> locOf f) (SIf b c d e))
            <$> if'
            <*> expression
            <*  then'
            <*> block
            <*> many ((,)
                <$> (elseif *> expression)
                <*  then'
                <*> block)
            <*> optional (else' *> block)
            <*> end
        <|> (\a b c d e f g -> L (locOf a <> locOf g) (SFor b c d e f))
            <$> for
            <*> ident
            <*  eq
            <*> expression
            <*  comma
            <*> expression
            <*> optional (comma *> expression)
            <*  do'
            <*> block
            <*> end
        <|> (\a b c d e -> L (locOf a <> locOf e) (SForIn b c d))
            <$> for
            <*> nameList1
            <*  in'
            <*> expressionList1
            <*  do'
            <*> block
            <*> end
        <|> (\a b c d (e,f,g,h) -> L (locOf a <> locOf h) (SFunctionDef b c d e f g))
            <$> function
            <*> ident
            <*> ident `sepBy` dot
            <*> optional (colon *> ident)
            <*> functionBody
        <|> (\a b (c,d,e,f) -> L (locOf a <> locOf f) (SLocalFunction b c d e))
            <$> local
            <*  function
            <*> ident
            <*> functionBody
        <|> (\a b c -> L (locOf a <> locOf b <> locOf c) (SLocalAssign b (fromMaybe [] c)))
            <$> local
            <*> nameList1
            <*> optional (eq *> expressionList1)

    blockReturnStatement :: P r (L [L Expression]) <- rule $
        (\a b c -> L (locOf a <> locOf b <> locOf c) b)
            <$> return'
            <*> expressionList
            <*> optional semi

    varList1 :: P r [L Variable] <- rule $
        var `sepBy1` comma

    var :: P r (L Variable) <- rule $
            (\a -> L (locOf a) (VIdent a))
                <$> ident
        <|> (\a b c -> L (locOf a <> locOf c) (VIndex a b))
            <$> prefixExpression
            <*  lbracket
            <*> expression
            <*> rbracket
        <|> (\a b -> L (locOf a <> locOf b) (VIndex a (EStringLit . unIdent <$> b)))
            <$> prefixExpression
            <*  dot
            <*> ident

    expressionList :: P r [L Expression] <- rule $
        expression `sepBy` comma

    expressionList1 :: P r [L Expression] <- rule $
        expression `sepBy1` comma

    atomicExpression :: P r (L Expression) <- rule $
            ENil              <$$  nil
        <|> ETrue             <$$  true
        <|> EFalse            <$$  false
        <|> EIntLit           <$$> intLit
        <|> EFloatLit         <$$> floatLit
        <|> EStringLit        <$$> stringLit
        <|> ETripleDot        <$$  tripleDot
        <|> EPrefixExp        <$$> prefixExpression
        <|> ETableConstructor <$$> tableConstructor

    expression :: P r (L Expression) <- mixfixExpression expressionTable atomicExpression combineMixfix

    prefixExpression :: P r (L PrefixExpression) <- rule $
            PEVar <$$> var
        <|> (\a b c -> L (locOf a <> locOf c) (PEFunctionCall a b c))
            <$> prefixExpression
            <*> optional (comma *> ident)
            <*> functionArgs
        <|> (\a b c -> L (locOf a <> locOf c) (PEExpr b))
            <$> lparen
            <*> expression
            <*> rparen

    functionArgs :: P r (L FunctionArgs) <- rule $
            (\a b c -> L (locOf a <> locOf c) (FAExprs b))
            <$> lparen
            <*> expressionList
            <*> rparen
        <|> FATableConstructor <$$> tableConstructor
        <|> FAStringLit <$$> stringLit

    functionBody :: P r ([L Ident], Bool, L Block, L Token) <- rule $
        pure (\(a,b) c d -> (a,b,c,d))
            <*  lparen
            <*> (fromMaybe ([], False) <$> optional parList)
            <*  rparen
            <*> block
            <*> end

    tableConstructor :: P r (L [L Field]) <- rule $
        let sep = comma <|> semi
        in (\a b c -> L (locOf a <> locOf c) b)
            <$> lbrace
            <*> sepBy field sep
            <*  optional sep
            <*> rbrace

    field :: P r (L Field) <- rule $
            (\a b c -> L (locOf a <> locOf c) (FExprAssign b c))
            <$> lbracket
            <*> expression
            <*  rbracket
            <*  eq
            <*> expression
        <|> (\a b -> L (locOf a <> locOf b) (FIdentAssign a b))
            <$> ident
            <*  eq
            <*> expression
        <|> FExpr <$$> expression

    return (block, statement, expression)
  where
    -- http://www.lua.org/manual/5.3/manual.html#3.4.8
    expressionTable :: [[([Maybe (P r (L Token))], Associativity)]]
    expressionTable =
        [ [ (binop TkOr           "or",  LeftAssoc)  ]

        , [ (binop TkAnd          "and", LeftAssoc)  ]

        , [ (binop TkLt           "<",   LeftAssoc)
          , (binop TkGt           ">",   LeftAssoc)
          , (binop TkLeq          "<=",  LeftAssoc)
          , (binop TkGeq          ">=",  LeftAssoc)
          , (binop TkNeq          "~=",  LeftAssoc)
          , (binop TkDoubleEq     "==",  LeftAssoc)  ]

        , [ (binop TkPipe         "|",   LeftAssoc)  ]

        , [ (binop TkTilde        "~",   LeftAssoc)  ]

        , [ (binop TkAmp          "&",   LeftAssoc)  ]

        , [ (binop TkLShift       "<<",  LeftAssoc)
          , (binop TkRShift       ">>",  LeftAssoc)  ]

        , [ (binop TkDoubleDot    "..",  RightAssoc) ]

        , [ (binop TkPlus         "+",   LeftAssoc)
          , (binop TkDash         "-",   LeftAssoc)  ]

        , [ (binop TkStar         "*",   LeftAssoc)
          , (binop TkFslash       "/",   LeftAssoc)
          , (binop TkDoubleFslash "//",  LeftAssoc)
          , (binop TkPct          "%",   LeftAssoc)  ]

        , [ unop   TkNot          "not"
          , unop   TkHash         "#"
          , unop   TkDash         "-"
          , unop   TkTilde        "~"                ]

        , [ (binop TkCarrot       "^",   RightAssoc) ]
        ]

    combineMixfix :: [Maybe (L Token)] -> [L Expression] -> L Expression
    combineMixfix (viewBinop -> Just (L loc tk)) [e1, e2] = L (locOf e1 <> locOf e2) (EBinop (L loc (tk2binop tk)) e1 e2)
    combineMixfix (viewUnop -> Just (L loc tk)) [e] = L (loc <> locOf e) (EUnop (L loc (tk2unop tk)) e)
    combineMixfix xs ys = error $ printf "Earley messed up?\nHoles: %s\nExprs: %s\n" (show xs) (show ys)

    viewBinop :: [Maybe (L Token)] -> Maybe (L Token)
    viewBinop [Nothing, x@(Just _), Nothing] = x
    viewBinop _ = Nothing

    viewUnop :: [Maybe (L Token)] -> Maybe (L Token)
    viewUnop [x@(Just _), Nothing] = x
    viewUnop _ = Nothing

    tk2binop :: Token -> Binop
    tk2binop TkPlus         = BPlus
    tk2binop TkDash         = BDash
    tk2binop TkStar         = BStar
    tk2binop TkFslash       = BFslash
    tk2binop TkDoubleFslash = BDoubleFslash
    tk2binop TkCarrot       = BCarrot
    tk2binop TkPct          = BPct
    tk2binop TkAmp          = BAmp
    tk2binop TkTilde        = BTilde
    tk2binop TkPipe         = BPipe
    tk2binop TkRShift       = BRShift
    tk2binop TkLShift       = BLShift
    tk2binop TkDoubleDot    = BDoubleDot
    tk2binop TkLt           = BLt
    tk2binop TkLeq          = BLeq
    tk2binop TkGt           = BGt
    tk2binop TkGeq          = BGeq
    tk2binop TkDoubleEq     = BDoubleEq
    tk2binop TkNeq          = BNeq
    tk2binop TkAnd          = BAnd
    tk2binop TkOr           = BOr
    tk2binop tk             = error $ printf "Token %s does not correspond to a binary op" (show tk)

    tk2unop :: Token -> Unop
    tk2unop TkNot   = UNot
    tk2unop TkHash  = UHash
    tk2unop TkDash  = UDash
    tk2unop TkTilde = UDash
    tk2unop tk      = error $ printf "Token %s does not correspond to a unary op" (show tk)

--------------------------------------------------------------------------------
-- Non-recursive non-terminals

nameList1 :: P r [L Ident]
nameList1 = ident `sepBy` comma

parList :: P r ([L Ident], Bool)
parList = withNames <|> withoutNames
  where
    withNames :: P r ([L Ident], Bool)
    withNames = (,)
        <$> nameList1
        <*> (isJust <$> optional (comma *> tripleDot))

    withoutNames :: P r ([L Ident], Bool)
    withoutNames = ([],True) <$ tripleDot

binop :: Token -> String -> [Maybe (P r (L Token))]
binop tk s = [Nothing, Just (locSymbol tk <?> s), Nothing]

unop :: Token -> String -> ([Maybe (P r (L Token))], Associativity)
unop tk s = ([Just (locSymbol tk <?> s), Nothing], NonAssoc)

--------------------------------------------------------------------------------
-- Combinators

sepBy :: Alternative f => f a -> f sep -> f [a]
sepBy f sep = sepBy1 f sep <|> pure []

sepBy1 :: Alternative f => f a -> f sep -> f [a]
sepBy1 f sep = (:) <$> f <*> many (sep *> f)

--------------------------------------------------------------------------------
-- Token productions

break :: P r (L Token)
break = locSymbol TkBreak <?> "break"

colon :: P r (L Token)
colon = locSymbol TkColon <?> ":"

comma :: P r (L Token)
comma = locSymbol TkComma <?> ","

do' :: P r (L Token)
do' = locSymbol TkDo <?> "do"

dot :: P r (L Token)
dot = locSymbol TkDot <?> "dot"

doubleColon :: P r (L Token)
doubleColon = locSymbol TkDoubleColon <?> "::"

else' :: P r (L Token)
else' = locSymbol TkElse <?> "else"

elseif :: P r (L Token)
elseif = locSymbol TkElseif <?> "elseif"

end :: P r (L Token)
end = locSymbol TkEnd <?> "end"

eq :: P r (L Token)
eq = locSymbol TkEq <?> "="

false :: P r (L Token)
false = locSymbol TkFalse <?> "false"

floatLit :: P r (L String)
floatLit = (\(TkFloatLit s) -> s) <$$> locSatisfy isFloatLit <?> "float literal"
  where
    isFloatLit :: Token -> Bool
    isFloatLit (TkFloatLit _) = True
    isFloatLit _ = False

for :: P r (L Token)
for = locSymbol TkFor <?> "for"

function :: P r (L Token)
function = locSymbol TkFunction <?> "function"

goto :: P r (L Token)
goto = locSymbol TkGoto <?> "goto"

ident :: P r (L Ident)
ident = (\(TkIdent s) -> Ident s) <$$> locSatisfy isIdent <?> "ident"
  where
    isIdent :: Token -> Bool
    isIdent (TkIdent _) = True
    isIdent _ = False

if' :: P r (L Token)
if' = locSymbol TkIf <?> "if"

in' :: P r (L Token)
in' = locSymbol TkIn <?> "in"

intLit :: P r (L String)
intLit = (\(TkIntLit s) -> s) <$$> locSatisfy isIntLit <?> "int literal"
  where
    isIntLit :: Token -> Bool
    isIntLit (TkIntLit _) = True
    isIntLit _ = False

lbrace :: P r (L Token)
lbrace = locSymbol TkLBrace <?> "{"

lbracket :: P r (L Token)
lbracket = locSymbol TkLBracket <?> "["

local :: P r (L Token)
local = locSymbol TkLocal <?> "local"

lparen :: P r (L Token)
lparen = locSymbol TkLParen <?> "("

nil :: P r (L Token)
nil = locSymbol TkNil <?> "nil"

rbrace :: P r (L Token)
rbrace = locSymbol TkRBrace <?> "}"

rbracket :: P r (L Token)
rbracket = locSymbol TkRBracket <?> "]"

repeat :: P r (L Token)
repeat = locSymbol TkRepeat <?> "repeat"

return' :: P r (L Token)
return' = locSymbol TkReturn <?> "return"

rparen :: P r (L Token)
rparen = locSymbol TkRParen <?> ")"

semi :: P r (L Token)
semi = locSymbol TkSemi <?> ";"

stringLit :: P r (L String)
stringLit = (\(TkStringLit s) -> s) <$$> locSatisfy isStringLit <?> "string literal"
  where
    isStringLit :: Token -> Bool
    isStringLit (TkStringLit _) = True
    isStringLit _ = False

then' :: P r (L Token)
then' = locSymbol TkThen <?> "then"

tripleDot :: P r (L Token)
tripleDot = locSymbol TkTripleDot <?> "..."

true :: P r (L Token)
true = locSymbol TkTrue <?> "true"

until :: P r (L Token)
until = locSymbol TkUntil <?> "until"

while :: P r (L Token)
while = locSymbol TkWhile <?> "while"

--------------------------------------------------------------------------------
-- Misc. extras

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$$
(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
x <$$ f = const x <$$> f

--------------------------------------------------------------------------------
-- Earley extras

locSymbol :: Eq t => t -> Prod r e (L t) (L t)
locSymbol = symbol . L NoLoc

locSatisfy :: (t -> Bool) -> Prod r e (L t) (L t)
locSatisfy p = satisfy (p . unLoc)

--------------------------------------------------------------------------------
-- Unfortunate orphans

instance Functor L where
    fmap f (L loc a) = L loc (f a)
