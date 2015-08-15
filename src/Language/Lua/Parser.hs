module Language.Lua.Parser
    ( NodeInfo(..)
    , luaBlock
    , luaExpression
    , luaStatement

    -- * Re-exports
    , Report(..)
    , Result(..)
    , allParses
    , fullParses
    , parser
    , report
    ) where

import Language.Lua.Internal
import Language.Lua.Syntax
import Language.Lua.Token

import           Control.Applicative
import           Data.List.NonEmpty  (NonEmpty((:|)))
import qualified Data.List.NonEmpty  as NE
import           Data.Loc
import           Data.Maybe          (fromMaybe)
import           Data.Monoid
import           Data.Sequence       (Seq)
import           Lens.Micro
import           Prelude             hiding (break, repeat, until)
import           Text.Earley
import           Text.Earley.Mixfix
import           Text.Printf         (printf)

type P r a = Prod r String (L Token) a
type G r a = Grammar r String (P r a)

data NodeInfo = NodeInfo
    { nodeLoc    :: !Loc
    , nodeTokens :: !(Seq (L Token))
    } deriving (Show)

instance Monoid NodeInfo where
    mempty = NodeInfo mempty mempty
    mappend (NodeInfo x1 y1) (NodeInfo x2 y2) = NodeInfo (x1 <> x2) (y1 <> y2)

class HasNodeInfo a where
    nodeInfo :: a -> NodeInfo

instance HasNodeInfo NodeInfo where
    nodeInfo = id

instance HasNodeInfo (L Token) where
    nodeInfo tk = NodeInfo (locOf tk) [tk]

instance Annotated ast => HasNodeInfo (ast NodeInfo) where
    nodeInfo = (^. ann)

instance (HasNodeInfo a, HasNodeInfo b) => HasNodeInfo (a, b) where
    nodeInfo (a, b) = nodeInfo a <> nodeInfo b

instance (HasNodeInfo a, HasNodeInfo b, HasNodeInfo c) => HasNodeInfo (a, b, c) where
    nodeInfo (a, b, c) = nodeInfo a <> nodeInfo b <> nodeInfo c

instance (HasNodeInfo a, HasNodeInfo b, HasNodeInfo c, HasNodeInfo d) => HasNodeInfo (a, b, c, d) where
    nodeInfo (a, b, c, d) = nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d

-- Make explicit instances for these Foldable containers to avoid overlapping instances.

instance HasNodeInfo a => HasNodeInfo [a] where
    nodeInfo = foldMap nodeInfo

instance HasNodeInfo a => HasNodeInfo (Seq a) where
    nodeInfo = foldMap nodeInfo

instance HasNodeInfo a => HasNodeInfo (NonEmpty a) where
    nodeInfo = foldMap nodeInfo

instance HasNodeInfo a => HasNodeInfo (Maybe a) where
    nodeInfo = foldMap nodeInfo

luaBlock :: G r (Block NodeInfo)
luaBlock = (\(a,_,_) -> a) <$> grammar

luaStatement :: G r (Statement NodeInfo)
luaStatement = (\(_,b,_) -> b) <$> grammar

luaExpression :: G r (Expression NodeInfo)
luaExpression = (\(_,_,c) -> c) <$> grammar

grammar :: Grammar r String (P r (Block NodeInfo), P r (Statement NodeInfo), P r (Expression NodeInfo))
grammar = mdo
    block :: P r (Block NodeInfo) <- rule $
        mkBlock
        <$> many statement
        <*> optional returnStatement

    statement :: P r (Statement NodeInfo) <- rule $
            mkEmptyStmt
            <$> semi
        <|> mkAssign
            <$> varList1
            <*> eq
            <*> expressionList1
        <|> mkFunCall
            <$> functionCall
        <|> mkLabel
            <$> doubleColon
            <*> ident
            <*> doubleColon
        <|> mkBreak
            <$> break
        <|> mkGoto
            <$> goto
            <*> ident
        <|> mkDo
            <$> do'
            <*> block
            <*> end
        <|> mkWhile
            <$> while
            <*> expression
            <*> do'
            <*> block
            <*> end
        <|> mkRepeat
            <$> repeat
            <*> block
            <*> until
            <*> expression
        <|> mkIf
            <$> if'
            <*> expression
            <*> then'
            <*> block
            <*> many ((,,,)
                <$> elseif
                <*> expression
                <*> then'
                <*> block)
            <*> optional ((,)
                <$> else'
                <*> block)
            <*> end
        <|> mkFor
            <$> for
            <*> ident
            <*> eq
            <*> expression
            <*> comma
            <*> expression
            <*> optional ((,)
                <$> comma
                <*> expression)
            <*> do'
            <*> block
            <*> end
        <|> mkForIn
            <$> for
            <*> identList1
            <*> in'
            <*> expressionList1
            <*> do'
            <*> block
            <*> end
        <|> mkFunAssign
            <$> function
            <*> functionName
            <*> functionBody
        <|> mkLocalFunAssign
            <$> local
            <*> function
            <*> ident
            <*> functionBody
        <|> mkLocalAssign
            <$> local
            <*> identList1
            <*> optional ((,)
                <$> eq
                <*> expressionList1)

    returnStatement :: P r (ReturnStatement NodeInfo) <- rule $
        mkReturnStatement
        <$> return'
        <*> expressionList
        <*> optional semi

    functionName :: P r (FunctionName NodeInfo) <- rule $
        mkFunctionName
        <$> identSepByDot
        <*> optional ((,)
            <$> colon
            <*> ident)

    identSepByDot :: P r (IdentList1 NodeInfo) <-
        uncurry IdentList1 <$$> sepBy1 ident dot

    varList1 :: P r (VariableList1 NodeInfo) <-
        uncurry VariableList1 <$$> sepBy1 var comma

    var :: P r (Variable NodeInfo) <- rule $
            mkVarIdent
            <$> ident
        <|> mkVarField
            <$> prefixExpression
            <*> lbracket
            <*> expression
            <*> rbracket
        <|> mkVarFieldName
            <$> prefixExpression
            <*> dot
            <*> ident

    identList1 :: P r (IdentList1 NodeInfo) <-
        uncurry IdentList1 <$$> sepBy1 ident comma

    expressionList :: P r (ExpressionList NodeInfo) <-
        uncurry ExpressionList <$$> sepBy expression comma

    expressionList1 :: P r (ExpressionList1 NodeInfo) <-
        uncurry ExpressionList1 <$$> sepBy1 expression comma

    expression :: P r (Expression NodeInfo) <-
        mixfixExpression expressionTable atomicExpression combineMixfix

    atomicExpression :: P r (Expression NodeInfo) <- rule $
            mkNil        <$> nil
        <|> mkBool True  <$> true
        <|> mkBool False <$> false
        <|> mkInteger    <$> intLit
        <|> mkFloat      <$> floatLit
        <|> mkString     <$> stringLit
        <|> mkVararg     <$> tripleDot
        <|> mkPrefixExp  <$> prefixExpression
        <|> mkTableCtor  <$> tableConstructor

    prefixExpression :: P r (PrefixExpression NodeInfo) <- rule $
            mkPrefixVar
            <$> var
        <|> mkPrefixFunCall
            <$> functionCall
        <|> mkParens
            <$> lparen
            <*> expression
            <*> rparen

    functionCall :: P r (FunctionCall NodeInfo) <- rule $
        mkFunctionCall
        <$> prefixExpression
        <*> optional ((,)
            <$> colon
            <*> ident)
        <*> functionArgs

    functionArgs :: P r (FunctionArgs NodeInfo) <- rule $
            mkArgs
            <$> lparen
            <*> expressionList
            <*> rparen
        <|> mkArgsTable
            <$> tableConstructor
        <|> mkArgsString
            <$> stringLit

    functionBody :: P r (FunctionBody NodeInfo) <- rule $
            mkFunctionBody
            <$> lparen
            <*> identList1
            <*> optional ((,)
                <$> comma
                <*> tripleDot)
            <*> rparen
            <*> block
            <*> end
        <|> mkFunctionBodyVararg
            <$> lparen
            <*> tripleDot
            <*> rparen
            <*> block
            <*> end

    tableConstructor :: P r (TableConstructor NodeInfo) <- rule $
        mkTableConstructor
        <$> lbrace
        <*> optional fieldList
        <*> rbrace

    field :: P r (Field NodeInfo) <- rule $
            mkFieldExp
            <$> lbracket
            <*> expression
            <*> rbracket
            <*> eq
            <*> expression
        <|> mkFieldIdent
            <$> ident
            <*> eq
            <*> expression
        <|> mkField
            <$> expression

    fieldList <- let sep = comma <|> semi in
        sepBy1 field sep >>= \fields -> rule (mkFieldList <$> fields <*> optional sep)

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
      where
        binop :: Token -> String -> [Maybe (P r (L Token))]
        binop tk s = [Nothing, Just (locSymbol tk <?> s), Nothing]

        unop :: Token -> String -> ([Maybe (P r (L Token))], Associativity)
        unop tk s = ([Just (locSymbol tk <?> s), Nothing], RightAssoc)

    combineMixfix :: [Maybe (L Token)] -> [Expression NodeInfo] -> Expression NodeInfo
    combineMixfix (viewBinop -> Just tk) [e1, e2] = Binop (nodeInfo e1 <> nodeInfo tk <> nodeInfo e2) (tk2binop tk) e1 e2
    combineMixfix (viewUnop -> Just tk) [e] = Unop (nodeInfo tk <> nodeInfo e) (tk2unop tk) e
    combineMixfix xs ys = error $ printf "Whoops, parser is busted.\nHoles: %s\nExprs: %s\n" (show xs) (show ys)

    viewBinop :: [Maybe (L Token)] -> Maybe (L Token)
    viewBinop [Nothing, x@(Just _), Nothing] = x
    viewBinop _ = Nothing

    viewUnop :: [Maybe (L Token)] -> Maybe (L Token)
    viewUnop [x@(Just _), Nothing] = x
    viewUnop _ = Nothing

    tk2binop :: L Token -> Binop NodeInfo
    tk2binop tk@(L _ TkPlus)         = Plus       (nodeInfo tk)
    tk2binop tk@(L _ TkDash)         = Minus      (nodeInfo tk)
    tk2binop tk@(L _ TkStar)         = Mult       (nodeInfo tk)
    tk2binop tk@(L _ TkFslash)       = FloatDiv   (nodeInfo tk)
    tk2binop tk@(L _ TkDoubleFslash) = FloorDiv   (nodeInfo tk)
    tk2binop tk@(L _ TkCarrot)       = Exponent   (nodeInfo tk)
    tk2binop tk@(L _ TkPct)          = Modulo     (nodeInfo tk)
    tk2binop tk@(L _ TkAmp)          = BitwiseAnd (nodeInfo tk)
    tk2binop tk@(L _ TkTilde)        = BitwiseXor (nodeInfo tk)
    tk2binop tk@(L _ TkPipe)         = BitwiseOr  (nodeInfo tk)
    tk2binop tk@(L _ TkRShift)       = Rshift     (nodeInfo tk)
    tk2binop tk@(L _ TkLShift)       = Lshift     (nodeInfo tk)
    tk2binop tk@(L _ TkDoubleDot)    = Concat     (nodeInfo tk)
    tk2binop tk@(L _ TkLt)           = Lt         (nodeInfo tk)
    tk2binop tk@(L _ TkLeq)          = Leq        (nodeInfo tk)
    tk2binop tk@(L _ TkGt)           = Gt         (nodeInfo tk)
    tk2binop tk@(L _ TkGeq)          = Geq        (nodeInfo tk)
    tk2binop tk@(L _ TkDoubleEq)     = Eq         (nodeInfo tk)
    tk2binop tk@(L _ TkNeq)          = Neq        (nodeInfo tk)
    tk2binop tk@(L _ TkAnd)          = And        (nodeInfo tk)
    tk2binop tk@(L _ TkOr)           = Or         (nodeInfo tk)
    tk2binop (L _ tk)                = error $ printf "Token %s does not correspond to a binary op" (show tk)

    tk2unop :: L Token -> Unop NodeInfo
    tk2unop tk@(L _ TkNot)   = Not        (nodeInfo tk)
    tk2unop tk@(L _ TkHash)  = Length     (nodeInfo tk)
    tk2unop tk@(L _ TkDash)  = Negate     (nodeInfo tk)
    tk2unop tk@(L _ TkTilde) = BitwiseNot (nodeInfo tk)
    tk2unop (L _ tk)           = error $ printf "Token %s does not correspond to a unary op" (show tk)

mkBlock :: [Statement NodeInfo] -> Maybe (ReturnStatement NodeInfo) -> Block NodeInfo
mkBlock a b = Block (nodeInfo a <> nodeInfo b) a b

mkEmptyStmt :: L Token -> Statement NodeInfo
mkEmptyStmt = EmptyStmt . nodeInfo

mkAssign :: VariableList1 NodeInfo -> L Token -> ExpressionList1 NodeInfo -> Statement NodeInfo
mkAssign a b c = Assign (nodeInfo a <> nodeInfo b <> nodeInfo c) a c

mkFunCall :: FunctionCall NodeInfo -> Statement NodeInfo
mkFunCall a = FunCall (nodeInfo a) a

mkLabel :: L Token -> Ident NodeInfo -> L Token -> Statement NodeInfo
mkLabel a b c = Label (nodeInfo a <> nodeInfo b <> nodeInfo c) b

mkBreak :: L Token -> Statement NodeInfo
mkBreak = Break . nodeInfo

mkGoto :: L Token -> Ident NodeInfo -> Statement NodeInfo
mkGoto a b = Goto (nodeInfo a <> nodeInfo b) b

mkDo :: L Token -> Block NodeInfo -> L Token -> Statement NodeInfo
mkDo a b c = Do (nodeInfo a <> nodeInfo b <> nodeInfo c) b

mkWhile :: L Token -> Expression NodeInfo -> L Token -> Block NodeInfo -> L Token -> Statement NodeInfo
mkWhile a b c d e = While (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e) b d

mkRepeat :: L Token -> Block NodeInfo -> L Token -> Expression NodeInfo -> Statement NodeInfo
mkRepeat a b c d = Repeat (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d) b d

mkIf
    :: L Token
    -> Expression NodeInfo
    -> L Token
    -> Block NodeInfo
    -> [(L Token, Expression NodeInfo, L Token, Block NodeInfo)]
    -> Maybe (L Token, Block NodeInfo)
    -> L Token
    -> Statement NodeInfo
mkIf a b c d e f g =
    If (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e <> nodeInfo f <> nodeInfo g)
       ((b,d) :| map (\(_,x,_,y) -> (x,y)) e)
       (snd <$> f)

mkFor
    :: L Token
    -> Ident NodeInfo
    -> L Token
    -> Expression NodeInfo
    -> L Token
    -> Expression NodeInfo
    -> Maybe (L Token, Expression NodeInfo)
    -> L Token
    -> Block NodeInfo
    -> L Token
    -> Statement NodeInfo
mkFor a b c d e f g h i j = For ni b d f (snd <$> g) i
  where
    ni :: NodeInfo
    ni = nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e <>
         nodeInfo f <> nodeInfo g <> nodeInfo h <> nodeInfo i <> nodeInfo j

mkForIn
    :: L Token
    -> IdentList1 NodeInfo
    -> L Token
    -> ExpressionList1 NodeInfo
    -> L Token
    -> Block NodeInfo
    -> L Token
    -> Statement NodeInfo
mkForIn a b c d e f g = ForIn ni b d f
  where
    ni :: NodeInfo
    ni = nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <>
         nodeInfo e <> nodeInfo f <> nodeInfo g

mkFunAssign
    :: L Token
    -> FunctionName NodeInfo
    -> FunctionBody NodeInfo
    -> Statement NodeInfo
mkFunAssign a b c = FunAssign (nodeInfo a <> nodeInfo b <> nodeInfo c) b c

mkLocalFunAssign
    :: L Token
    -> L Token
    -> Ident NodeInfo
    -> FunctionBody NodeInfo
    -> Statement NodeInfo
mkLocalFunAssign a b c d = LocalFunAssign (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d) c d

mkLocalAssign
    :: L Token
    -> IdentList1 NodeInfo
    -> Maybe (L Token, ExpressionList1 NodeInfo)
    -> Statement NodeInfo
mkLocalAssign a b c = LocalAssign (nodeInfo a <> nodeInfo b <> nodeInfo c) b c'
  where
    c' :: ExpressionList NodeInfo
    c' = maybe (ExpressionList mempty [])
               (\(_, ExpressionList1 n es) -> ExpressionList n (NE.toList es))
               c

mkReturnStatement :: L Token -> ExpressionList NodeInfo -> Maybe (L Token) -> ReturnStatement NodeInfo
mkReturnStatement a b c = ReturnStatement (nodeInfo a <> nodeInfo b <> nodeInfo c) b

mkFunctionName
    :: IdentList1 NodeInfo
    -> Maybe (L Token, Ident NodeInfo)
    -> FunctionName NodeInfo
mkFunctionName a b = FunctionName (nodeInfo a <> nodeInfo b) a (snd <$> b)

mkVarIdent :: Ident NodeInfo -> Variable NodeInfo
mkVarIdent a = VarIdent (nodeInfo a) a

mkVarField :: PrefixExpression NodeInfo -> L Token -> Expression NodeInfo -> L Token -> Variable NodeInfo
mkVarField a b c d = VarField (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d) a c

mkVarFieldName :: PrefixExpression NodeInfo -> L Token -> Ident NodeInfo -> Variable NodeInfo
mkVarFieldName a b c = VarFieldName (nodeInfo a <> nodeInfo b <> nodeInfo c) a c

mkNil :: L Token -> Expression NodeInfo
mkNil = Nil . nodeInfo

mkBool :: Bool -> L Token -> Expression NodeInfo
mkBool a b = Bool (nodeInfo b) a

mkInteger :: L Token -> Expression NodeInfo
mkInteger a@(L _ (TkIntLit s)) = Integer (nodeInfo a) s
mkInteger (L _ tk) = error $ printf "mkInteger: %s is not a TkIntLit" (show tk)

mkFloat :: L Token -> Expression NodeInfo
mkFloat a@(L _ (TkFloatLit s)) = Float (nodeInfo a) s
mkFloat (L _ tk) = error $ printf "mkFloat: %s is not a TkFloatLit" (show tk)

mkString :: L Token -> Expression NodeInfo
mkString a@(L _ (TkStringLit s)) = String (nodeInfo a) s
mkString (L _ tk) = error $ printf "mkString: %s is not a TkStringLit" (show tk)

mkVararg :: L Token -> Expression NodeInfo
mkVararg = Vararg . nodeInfo

mkPrefixExp :: PrefixExpression NodeInfo -> Expression NodeInfo
mkPrefixExp a = PrefixExp (nodeInfo a) a

mkTableCtor :: TableConstructor NodeInfo -> Expression NodeInfo
mkTableCtor a = TableCtor (nodeInfo a) a

mkPrefixVar :: Variable NodeInfo -> PrefixExpression NodeInfo
mkPrefixVar a = PrefixVar (nodeInfo a) a

mkPrefixFunCall :: FunctionCall NodeInfo -> PrefixExpression NodeInfo
mkPrefixFunCall a = PrefixFunCall (nodeInfo a) a

mkParens :: L Token -> Expression NodeInfo -> L Token -> PrefixExpression NodeInfo
mkParens a b c = Parens (nodeInfo a <> nodeInfo b <> nodeInfo c) b

mkFunctionCall
    :: PrefixExpression NodeInfo
    -> Maybe (L Token, Ident NodeInfo)
    -> FunctionArgs NodeInfo
    -> FunctionCall NodeInfo
mkFunctionCall a (Just (b,c)) d = MethodCall (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d) a c d
mkFunctionCall a Nothing b      = FunctionCall (nodeInfo a <> nodeInfo b) a b

mkArgs :: L Token -> ExpressionList NodeInfo -> L Token -> FunctionArgs NodeInfo
mkArgs a b c = Args (nodeInfo a <> nodeInfo b <> nodeInfo c) b

mkArgsTable :: TableConstructor NodeInfo -> FunctionArgs NodeInfo
mkArgsTable a = ArgsTable (nodeInfo a) a

mkArgsString :: L Token -> FunctionArgs NodeInfo
mkArgsString a@(L _ (TkStringLit s)) = ArgsString (nodeInfo a) s
mkArgsString tk = error $ printf "mkArgsString: %s is not a TkStringLit" (show tk)

mkFunctionBody
    :: L Token
    -> IdentList1 NodeInfo
    -> Maybe (L Token, L Token)
    -> L Token
    -> Block NodeInfo
    -> L Token
    -> FunctionBody NodeInfo
mkFunctionBody a b (Just (c,d)) e f g =
    FunctionBody (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e <> nodeInfo f <> nodeInfo g) b True f
mkFunctionBody a b Nothing c d e =
    FunctionBody (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e) b False d

mkFunctionBodyVararg :: L Token -> L Token -> L Token -> Block NodeInfo -> L Token -> FunctionBody NodeInfo
mkFunctionBodyVararg a b c d e = FunctionBodyVararg (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e) d

mkTableConstructor :: L Token -> Maybe (FieldList NodeInfo) -> L Token -> TableConstructor NodeInfo
mkTableConstructor a b c = TableConstructor (nodeInfo a <> nodeInfo b <> nodeInfo c) (fromMaybe (FieldList mempty []) b)

mkFieldExp :: L Token -> Expression NodeInfo -> L Token -> L Token -> Expression NodeInfo -> Field NodeInfo
mkFieldExp a b c d e = FieldExp (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e) b e

mkFieldIdent :: Ident NodeInfo -> L Token -> Expression NodeInfo -> Field NodeInfo
mkFieldIdent a b c = FieldIdent (nodeInfo a <> nodeInfo b <> nodeInfo c) a c

mkField :: Expression NodeInfo -> Field NodeInfo
mkField a = Field (nodeInfo a) a

mkFieldList :: (NodeInfo, NonEmpty (Field NodeInfo)) -> Maybe (L Token) -> FieldList NodeInfo
mkFieldList a b = FieldList (nodeInfo a <> nodeInfo b) (NE.toList (snd a))

--------------------------------------------------------------------------------
-- Combinators

sepBy :: (HasNodeInfo a, HasNodeInfo sep)
      => Prod r e t a
      -> Prod r e t sep
      -> Grammar r e (Prod r e t (NodeInfo, [a]))
sepBy f sep = do
    fs <- sepBy1 f sep
    rule $ (_2 %~ NE.toList) <$> fs
       <|> pure mempty

sepBy1 :: forall r e t a sep. (HasNodeInfo a, HasNodeInfo sep)
       => Prod r e t a
       -> Prod r e t sep
       -> Grammar r e (Prod r e t (NodeInfo, NonEmpty a))
sepBy1 f sep = mdo
    fs :: Prod r e t (NodeInfo, [a]) <-
        rule $ liftA3 (\a b (c,d) -> (nodeInfo a <> nodeInfo b <> c, b:d)) sep f fs
           <|> pure mempty
    rule $ liftA2 (\a (b,c) -> (nodeInfo a <> b, a :| c)) f fs

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

floatLit :: P r (L Token)
floatLit = locSatisfy isFloatLit <?> "float literal"
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

ident :: P r (Ident NodeInfo)
ident = (\tk@(L _ (TkIdent s)) -> Ident (nodeInfo tk) s) <$> locSatisfy isIdent <?> "ident"
  where
    isIdent :: Token -> Bool
    isIdent (TkIdent _) = True
    isIdent _ = False

if' :: P r (L Token)
if' = locSymbol TkIf <?> "if"

in' :: P r (L Token)
in' = locSymbol TkIn <?> "in"

intLit :: P r (L Token)
intLit = locSatisfy isIntLit <?> "int literal"
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

stringLit :: P r (L Token)
stringLit = locSatisfy isStringLit <?> "string literal"
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
