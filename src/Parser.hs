module Parser
    ( blockGrammar
    , expressionGrammar
    , statementGrammar
    ) where

import Internal
import Syntax
import Token

import Control.Applicative
import Data.List.NonEmpty  (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Loc
import Data.Maybe          (fromMaybe)
import Data.Monoid
import Lens.Micro
import Prelude             hiding (break, repeat, until)
import Text.Earley
import Text.Earley.Mixfix
import Text.Printf         (printf)

type P r a = Prod r String (L Token) a
type G r a = Grammar r String (P r a)

blockGrammar :: G r (Block SrcLoc)
blockGrammar = (\(a,_,_) -> a) <$> grammar

statementGrammar :: G r (Statement SrcLoc)
statementGrammar = (\(_,b,_) -> b) <$> grammar

expressionGrammar :: G r (Expression SrcLoc)
expressionGrammar = (\(_,_,c) -> c) <$> grammar

grammar :: Grammar r String (P r (Block SrcLoc), P r (Statement SrcLoc), P r (Expression SrcLoc))
grammar = mdo
    block :: P r (Block Loc) <- rule $
        (\a b -> Block (annF a <> annF b) a b)
            <$> many statement
            <*> optional returnStatement

    statement :: P r (Statement Loc) <-
        sepBy1 ident dot        >>= \identSepByDot   ->
        sepBy1 var comma        >>= \varList1        ->
        sepBy1 expression comma >>= \expressionList1 -> rule $

            EmptyStmt . locOf <$> semi
        <|> (\a b -> Assign (annF a <> annF b) a b)
            <$> varList1
            <*  eq
            <*> expressionList1
        <|> (\a -> FunCall (a^.ann) a) <$> functionCall
        <|> (\a b c -> Label (locOf a <> locOf c) b)
            <$> doubleColon
            <*> ident
            <*> doubleColon
        <|> Break . locOf <$> break
        <|> (\a b -> Goto (locOf a <> b^.ann) b)
            <$> goto
            <*> ident
        <|> (\a b c -> Do (locOf a <> locOf c) b)
            <$> do'
            <*> block
            <*> end
        <|> (\a b c d -> While (locOf a <> locOf d) b c)
            <$> while
            <*> expression
            <*  do'
            <*> block
            <*> end
        <|> (\a b c -> Repeat (locOf a <> c^.ann) b c)
            <$> repeat
            <*> block
            <*  until
            <*> expression
        <|> (\a b c d e f -> If (locOf a <> locOf f) ((b,c) :| d) e)
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
        <|> (\a b c d e f g -> For (locOf a <> locOf g) b c d e f)
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
        <|> (\a b c d e -> ForIn (locOf a <> locOf e) b c d)
            <$> for
            <*> nameList1
            <*  in'
            <*> expressionList1
            <*  do'
            <*> block
            <*> end
        <|> (\a b c d -> FunAssign (locOf a <> d^.ann) b c d)
            <$> function
            <*> identSepByDot
            <*> optional (colon *> ident)
            <*> functionBody
        <|> (\a b c -> LocalFunAssign (locOf a <> c^.ann) b c)
            <$> local
            <*  function
            <*> ident
            <*> functionBody
        <|> (\a b c -> LocalAssign (locOf a <> annF b <> maybe mempty annF c) b (maybe [] NE.toList c))
            <$> local
            <*> nameList1
            <*> optional (eq *> expressionList1)

    returnStatement :: P r (ReturnStatement Loc) <- rule $
        (\a b c -> ReturnStatement (locOf a <> annF b <> locOf c) b)
            <$> return'
            <*> expressionList
            <*> optional semi

    var :: P r (Variable Loc) <- rule $
            (\a -> VarIdent (a^.ann) a)
                <$> ident
        <|> (\a b c -> VarField (a^.ann <> locOf c) a b)
            <$> prefixExpression
            <*  lbracket
            <*> expression
            <*> rbracket
        <|> (\a b -> VarFieldName (a^.ann <> b^.ann) a b)
            <$> prefixExpression
            <*  dot
            <*> ident

    nameList1 :: P r (NonEmpty (Ident Loc)) <- ident `sepBy1` comma

    expressionList :: P r [Expression Loc] <- expression `sepBy` comma

    expression :: P r (Expression Loc) <-
        mixfixExpression expressionTable atomicExpression combineMixfix

    atomicExpression :: P r (Expression Loc) <- rule $
            Nil . locOf                            <$> nil
        <|> (\a -> Bool (locOf a) True)            <$> true
        <|> (\a -> Bool (locOf a) False)           <$> false
        <|> (\(L loc a) -> Integer loc a)          <$> intLit
        <|> (\(L loc a) -> Float loc a)            <$> floatLit
        <|> (\(L loc a) -> String loc a)           <$> stringLit
        <|> Vararg . locOf                         <$> tripleDot
        <|> (\a -> PrefixExp (a^.ann) a)           <$> prefixExpression
        <|> (\(L loc a) -> TableConstructor loc a) <$> tableConstructor

    prefixExpression :: P r (PrefixExpression Loc) <- rule $
            (\a -> PrefixVar (a^.ann) a) <$> var
        <|> (\a -> PrefixFunCall (a^.ann) a) <$> functionCall
        <|> (\a b c -> Parens (locOf a <> locOf c) b)
            <$> lparen
            <*> expression
            <*> rparen

    functionCall :: P r (FunctionCall Loc) <- rule $
        (\a mb c -> case mb of
                        Nothing -> FunctionCall (a^.ann <> c^.ann) a c
                        Just b  -> MethodCall (a^.ann <> c^.ann) a b c)
            <$> prefixExpression
            <*> optional (comma *> ident)
            <*> functionArgs

    functionArgs :: P r (FunctionArgs Loc) <- rule $
            (\a b c -> Args (locOf a <> locOf c) b)
            <$> lparen
            <*> expressionList
            <*> rparen
        <|> (\(L loc a) -> ArgsTable loc a) <$> tableConstructor
        <|> (\(L loc a) -> ArgsString loc a) <$> stringLit

    functionBody :: P r (FunctionBody Loc) <- rule $ -- ([Ident Loc], Bool, Block Loc, L Token) <- rule $
        (\a (b,c) d e -> FunctionBody (locOf a <> locOf e) b c d)
            <$> lparen
            <*> (fromMaybe ([], Nothing) <$> optional parList)
            <*  rparen
            <*> block
            <*> end

    parList :: P r ([Ident Loc], Maybe Loc) <- rule $
        let withNames = (,)
                <$> (NE.toList <$> nameList1)
                <*> optional ((\a b -> locOf a <> locOf b) <$> comma <*> tripleDot)

            withoutNames = ([],) . Just . locOf <$> tripleDot

        in withNames <|> withoutNames

    tableConstructor :: P r (L [Field Loc]) <-
        let sep = comma <|> semi
        in sepBy field sep >>= \fields -> rule $
            (\a b c -> L (locOf a <> locOf c) b)
            <$> lbrace
            <*> fields
            <*  optional sep
            <*> rbrace

    field :: P r (Field Loc) <- rule $
            (\a b c -> FieldExp (locOf a <> c^.ann) b c)
            <$> lbracket
            <*> expression
            <*  rbracket
            <*  eq
            <*> expression
        <|> (\a b -> FieldIdent (a^.ann <> b^.ann) a b)
            <$> ident
            <*  eq
            <*> expression
        <|> (\a -> Field (a^.ann) a) <$> expression

    return (SrcLoc <$$> block, SrcLoc <$$> statement, SrcLoc <$$> expression)
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
      where
        binop :: Token -> String -> [Maybe (P r (L Token))]
        binop tk s = [Nothing, Just (locSymbol tk <?> s), Nothing]

        unop :: Token -> String -> ([Maybe (P r (L Token))], Associativity)
        unop tk s = ([Just (locSymbol tk <?> s), Nothing], RightAssoc)

    combineMixfix :: [Maybe (L Token)] -> [Expression Loc] -> Expression Loc
    combineMixfix (viewBinop -> Just tk) [e1, e2] = Binop (e1^.ann <> e2^.ann) (tk2binop tk) e1 e2
    combineMixfix (viewUnop -> Just tk) [e] = Unop (locOf tk <> e^.ann) (tk2unop tk) e
    combineMixfix xs ys = error $ printf "Earley messed up?\nHoles: %s\nExprs: %s\n" (show xs) (show ys)

    viewBinop :: [Maybe (L Token)] -> Maybe (L Token)
    viewBinop [Nothing, x@(Just _), Nothing] = x
    viewBinop _ = Nothing

    viewUnop :: [Maybe (L Token)] -> Maybe (L Token)
    viewUnop [x@(Just _), Nothing] = x
    viewUnop _ = Nothing

    tk2binop :: L Token -> Binop Loc
    tk2binop (L loc TkPlus)         = Plus loc
    tk2binop (L loc TkDash)         = Minus loc
    tk2binop (L loc TkStar)         = Mult loc
    tk2binop (L loc TkFslash)       = FloatDiv loc
    tk2binop (L loc TkDoubleFslash) = FloorDiv loc
    tk2binop (L loc TkCarrot)       = Exponent loc
    tk2binop (L loc TkPct)          = Modulo loc
    tk2binop (L loc TkAmp)          = BitwiseAnd loc
    tk2binop (L loc TkTilde)        = BitwiseXor loc
    tk2binop (L loc TkPipe)         = BitwiseOr loc
    tk2binop (L loc TkRShift)       = Rshift loc
    tk2binop (L loc TkLShift)       = Lshift loc
    tk2binop (L loc TkDoubleDot)    = Concat loc
    tk2binop (L loc TkLt)           = Lt loc
    tk2binop (L loc TkLeq)          = Leq loc
    tk2binop (L loc TkGt)           = Gt loc
    tk2binop (L loc TkGeq)          = Geq loc
    tk2binop (L loc TkDoubleEq)     = Eq loc
    tk2binop (L loc TkNeq)          = Neq loc
    tk2binop (L loc TkAnd)          = And loc
    tk2binop (L loc TkOr)           = Or loc
    tk2binop (L _ tk)               = error $ printf "Token %s does not correspond to a binary op" (show tk)

    tk2unop :: L Token -> Unop Loc
    tk2unop (L loc TkNot)   = Not loc
    tk2unop (L loc TkHash)  = Length loc
    tk2unop (L loc TkDash)  = Negate loc
    tk2unop (L loc TkTilde) = BitwiseNot loc
    tk2unop (L _ tk)        = error $ printf "Token %s does not correspond to a unary op" (show tk)

annF :: (Foldable t, Monoid m, Annotated ast) => t (ast m) -> m
annF = foldMap (^. ann)

--------------------------------------------------------------------------------
-- Combinators

sepBy :: Prod r e t a -> Prod r e t sep -> Grammar r e (Prod r e t [a])
sepBy f sep = (<|> pure []) <$> (NE.toList <$$> sepBy1 f sep)

sepBy1 :: Prod r e t a -> Prod r e t sep -> Grammar r e (Prod r e t (NonEmpty a))
sepBy1 f sep = mdo
    fs <- rule $ liftA3 (const (:)) sep f fs <|> pure []
    rule $ liftA2 (:|) f fs

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

ident :: P r (Ident Loc)
ident = (\(L loc (TkIdent s)) -> Ident loc s) <$> locSatisfy isIdent <?> "ident"
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
-- Earley extras

locSymbol :: Eq t => t -> Prod r e (L t) (L t)
locSymbol = symbol . L NoLoc

locSatisfy :: (t -> Bool) -> Prod r e (L t) (L t)
locSatisfy p = satisfy (p . unLoc)
