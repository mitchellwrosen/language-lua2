{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ViewPatterns        #-}

module Language.Lua.Parser.Internal where

import Language.Lua.Internal
import Language.Lua.Syntax
import Language.Lua.Token

import           Control.Applicative
import           Data.Data
#if !MIN_VERSION_base(4,8,0)
import           Data.Foldable       (foldMap)
#endif
import           Data.List.NonEmpty  (NonEmpty((:|)))
import qualified Data.List.NonEmpty  as NE
import           Data.Loc
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Monoid
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           GHC.Generics        (Generic)
import           Lens.Micro
import           Prelude             hiding (break, repeat, until)
import           Text.Earley
import           Text.Earley.Mixfix
import           Text.Printf         (printf)

-- | AST node source location and constituent tokens. The tokens are provided
-- for style-checking purposes; with them, you may assert proper whitespace
-- protocol, alignment, trailing commas on table constructors, and whatever
-- other subjectivities.
data NodeInfo = NodeInfo
    { _nodeLoc    :: !Loc             -- ^ Source location; spans the entirety of the node.
    , _nodeTokens :: !(Seq (L Token)) -- ^ Parsed tokens involved in node production.
    } deriving (Data, Eq, Generic, Show, Typeable)

instance Monoid NodeInfo where
    mempty = NodeInfo mempty mempty
    mappend (NodeInfo x1 y1) (NodeInfo x2 y2) = NodeInfo (x1 <> x2) (y1 <> y2)

nodeLoc :: Lens' NodeInfo Loc
nodeLoc = lens (\(NodeInfo a _) -> a) (\(NodeInfo _ b) a -> NodeInfo a b)

nodeTokens :: Lens' NodeInfo (Seq (L Token))
nodeTokens = lens (\(NodeInfo _ b) -> b) (\(NodeInfo a _) b -> NodeInfo a b)

class HasNodeInfo a where
    nodeInfo :: a -> NodeInfo

instance HasNodeInfo NodeInfo where
    nodeInfo = id

instance HasNodeInfo (L Token) where
    nodeInfo tk = NodeInfo (locOf tk) (Seq.singleton tk)

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

luaReturnStatement  = (\(_,_,x,_,_,_,_,_,_,_,_,_,_,_,_) -> x) <$> grammar
luaFunctionName     = (\(_,_,_,x,_,_,_,_,_,_,_,_,_,_,_) -> x) <$> grammar
luaVariable         = (\(_,_,_,_,x,_,_,_,_,_,_,_,_,_,_) -> x) <$> grammar
luaAtomicExpression = (\(_,_,_,_,_,_,x,_,_,_,_,_,_,_,_) -> x) <$> grammar
luaPrefixExpression = (\(_,_,_,_,_,_,_,x,_,_,_,_,_,_,_) -> x) <$> grammar
luaFunctionCall     = (\(_,_,_,_,_,_,_,_,x,_,_,_,_,_,_) -> x) <$> grammar
luaFunctionArgs     = (\(_,_,_,_,_,_,_,_,_,x,_,_,_,_,_) -> x) <$> grammar
luaFunctionBody     = (\(_,_,_,_,_,_,_,_,_,_,x,_,_,_,_) -> x) <$> grammar
luaParamList        = (\(_,_,_,_,_,_,_,_,_,_,_,x,_,_,_) -> x) <$> grammar
luaTableConstructor = (\(_,_,_,_,_,_,_,_,_,_,_,_,x,_,_) -> x) <$> grammar
luaField            = (\(_,_,_,_,_,_,_,_,_,_,_,_,_,x,_) -> x) <$> grammar
luaFieldList        = (\(_,_,_,_,_,_,_,_,_,_,_,_,_,_,x) -> x) <$> grammar

grammar :: Grammar r ( Prod r String (L Token) (Block NodeInfo)
                     , Prod r String (L Token) (Statement NodeInfo)
                     , Prod r String (L Token) (ReturnStatement NodeInfo)
                     , Prod r String (L Token) (FunctionName NodeInfo)
                     , Prod r String (L Token) (Variable NodeInfo)
                     , Prod r String (L Token) (Expression NodeInfo)
                     , Prod r String (L Token) (Expression NodeInfo)
                     , Prod r String (L Token) (PrefixExpression NodeInfo)
                     , Prod r String (L Token) (FunctionCall NodeInfo)
                     , Prod r String (L Token) (FunctionArgs NodeInfo)
                     , Prod r String (L Token) (FunctionBody NodeInfo)
                     , Prod r String (L Token) (ParamList NodeInfo)
                     , Prod r String (L Token) (TableConstructor NodeInfo)
                     , Prod r String (L Token) (Field NodeInfo)
                     , Prod r String (L Token) (FieldList NodeInfo)
                     )
grammar = mdo
    block :: Prod r String (L Token) (Block NodeInfo) <- rule $
        mkBlock
        <$> many statement
        <*> optional returnStatement

    identList1 :: Prod r String (L Token) (IdentList1 NodeInfo) <- identList1G

    statement :: Prod r String (L Token) (Statement NodeInfo) <- rule $
            mkEmptyStmt
            <$> semi
        <|> mkAssign
            <$> varList1
            <*> assign
            <*> expressionList1
        <|> mkFunCall
            <$> functionCall
        <|> mkLabel
            <$> label
            <*> ident
            <*> label
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
            <*> assign
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
                <$> assign
                <*> expressionList1)

    returnStatement :: Prod r String (L Token) (ReturnStatement NodeInfo) <- rule $
        mkReturnStatement
        <$> return'
        <*> expressionList
        <*> optional semi

    identSepByDot <- identSepByDotG

    functionName :: Prod r String (L Token) (FunctionName NodeInfo) <- rule $
        mkFunctionName
        <$> identSepByDot
        <*> optional ((,)
            <$> colon
            <*> ident)

    varList1 :: Prod r String (L Token) (VariableList1 NodeInfo) <-
        uncurry VariableList1 <$$> sepBy1 var comma

    var :: Prod r String (L Token) (Variable NodeInfo) <- rule $
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

    expressionList :: Prod r String (L Token) (ExpressionList NodeInfo) <-
        uncurry ExpressionList <$$> sepBy expression comma

    expressionList1 :: Prod r String (L Token) (ExpressionList1 NodeInfo) <-
        uncurry ExpressionList1 <$$> sepBy1 expression comma

    expression :: Prod r String (L Token) (Expression NodeInfo) <-
        mixfixExpression expressionTable atomicExpression combineMixfix

    atomicExpression :: Prod r String (L Token) (Expression NodeInfo) <- rule $
            mkNil        <$> nil
        <|> mkBool True  <$> true
        <|> mkBool False <$> false
        <|> mkInteger    <$> intLit
        <|> mkFloat      <$> floatLit
        <|> mkString     <$> stringLit
        <|> mkVararg     <$> vararg
        <|> mkFunDef     <$> function <*> functionBody
        <|> mkPrefixExp  <$> prefixExpression
        <|> mkTableCtor  <$> tableConstructor

    prefixExpression :: Prod r String (L Token) (PrefixExpression NodeInfo) <- rule $
            mkPrefixVar
            <$> var
        <|> mkPrefixFunCall
            <$> functionCall
        <|> mkParens
            <$> lparen
            <*> expression
            <*> rparen

    functionCall :: Prod r String (L Token) (FunctionCall NodeInfo) <- rule $
        mkFunctionCall
        <$> prefixExpression
        <*> optional ((,)
            <$> colon
            <*> ident)
        <*> functionArgs

    functionArgs :: Prod r String (L Token) (FunctionArgs NodeInfo) <- rule $
            mkArgs
            <$> lparen
            <*> expressionList
            <*> rparen
        <|> mkArgsTable
            <$> tableConstructor
        <|> mkArgsString
            <$> stringLit

    identList <- identListG

    functionBody :: Prod r String (L Token) (FunctionBody NodeInfo) <- rule $
            mkFunctionBody
            <$> lparen
            <*> optional paramList
            <*> rparen
            <*> block
            <*> end

    paramList :: Prod r String (L Token) (ParamList NodeInfo) <- rule $
            ParamListVararg . nodeInfo <$> vararg
        <|> mkParamList
            <$> identList
            <*> optional ((,)
                <$> comma
                <*> vararg)


    tableConstructor :: Prod r String (L Token) (TableConstructor NodeInfo) <- rule $
        mkTableConstructor
        <$> lbrace
        <*> optional fieldList
        <*> rbrace

    field :: Prod r String (L Token) (Field NodeInfo) <- rule $
            mkFieldExp
            <$> lbracket
            <*> expression
            <*> rbracket
            <*> assign
            <*> expression
        <|> mkFieldIdent
            <$> ident
            <*> assign
            <*> expression
        <|> mkField
            <$> expression

    let fieldSep = comma <|> semi
    fields <- sepBy1 field fieldSep
    fieldList :: Prod r String (L Token) (FieldList NodeInfo) <- rule $
            mkFieldList
            <$> fields
            <*> optional fieldSep

    return ( block
           , statement
           , returnStatement
           , functionName
           , var
           , expression
           , atomicExpression
           , prefixExpression
           , functionCall
           , functionArgs
           , functionBody
           , paramList
           , tableConstructor
           , field
           , fieldList
           )
  where
    -- http://www.lua.org/manual/5.3/manual.html#3.4.8
    expressionTable :: [[([Maybe (Prod r String (L Token) (L Token))], Associativity)]]
    expressionTable =
        [ [ (binop TkOr           "'or'",  LeftAssoc)  ]

        , [ (binop TkAnd          "'and'", LeftAssoc)  ]

        , [ (binop TkLt           "'<'",   LeftAssoc)
          , (binop TkGt           "'>'",   LeftAssoc)
          , (binop TkLeq          "'<='",  LeftAssoc)
          , (binop TkGeq          "'>='",  LeftAssoc)
          , (binop TkNeq          "'~='",  LeftAssoc)
          , (binop TkEq           "'=='",  LeftAssoc)  ]

        , [ (binop TkBitwiseOr    "'|'",   LeftAssoc)  ]

        , [ (binop TkTilde        "'~'",   LeftAssoc)  ]

        , [ (binop TkBitwiseAnd   "'&'",   LeftAssoc)  ]

        , [ (binop TkLShift       "'<<'",  LeftAssoc)
          , (binop TkRShift       "'>>'",  LeftAssoc)  ]

        , [ (binop TkConcat       "'..'",  RightAssoc) ]

        , [ (binop TkPlus         "'+'",   LeftAssoc)
          , (binop TkDash         "'-'",   LeftAssoc)  ]

        , [ (binop TkMult         "'*'",   LeftAssoc)
          , (binop TkFloatDiv     "'/'",   LeftAssoc)
          , (binop TkFloorDiv     "'//'",  LeftAssoc)
          , (binop TkModulo       "'%'",   LeftAssoc)  ]

        , [ unop   TkNot          "'not'"
          , unop   TkLength       "'#'"
          , unop   TkDash         "'-'"
          , unop   TkTilde        "'~'"                ]

        , [ (binop TkExponent     "'^'",   RightAssoc) ]
        ]
      where
        binop :: Token -> String -> [Maybe (Prod r String (L Token) (L Token))]
        binop tk s = [Nothing, Just (locSymbol tk <?> s), Nothing]

        unop :: Token -> String -> ([Maybe (Prod r String (L Token) (L Token))], Associativity)
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
    tk2binop tk@(L _ TkPlus)       = Plus       (nodeInfo tk)
    tk2binop tk@(L _ TkDash)       = Minus      (nodeInfo tk)
    tk2binop tk@(L _ TkMult)       = Mult       (nodeInfo tk)
    tk2binop tk@(L _ TkFloatDiv)   = FloatDiv   (nodeInfo tk)
    tk2binop tk@(L _ TkFloorDiv)   = FloorDiv   (nodeInfo tk)
    tk2binop tk@(L _ TkExponent)   = Exponent   (nodeInfo tk)
    tk2binop tk@(L _ TkModulo)     = Modulo     (nodeInfo tk)
    tk2binop tk@(L _ TkBitwiseAnd) = BitwiseAnd (nodeInfo tk)
    tk2binop tk@(L _ TkTilde)      = BitwiseXor (nodeInfo tk)
    tk2binop tk@(L _ TkBitwiseOr)  = BitwiseOr  (nodeInfo tk)
    tk2binop tk@(L _ TkRShift)     = Rshift     (nodeInfo tk)
    tk2binop tk@(L _ TkLShift)     = Lshift     (nodeInfo tk)
    tk2binop tk@(L _ TkConcat)     = Concat     (nodeInfo tk)
    tk2binop tk@(L _ TkLt)         = Lt         (nodeInfo tk)
    tk2binop tk@(L _ TkLeq)        = Leq        (nodeInfo tk)
    tk2binop tk@(L _ TkGt)         = Gt         (nodeInfo tk)
    tk2binop tk@(L _ TkGeq)        = Geq        (nodeInfo tk)
    tk2binop tk@(L _ TkEq)         = Eq         (nodeInfo tk)
    tk2binop tk@(L _ TkNeq)        = Neq        (nodeInfo tk)
    tk2binop tk@(L _ TkAnd)        = And        (nodeInfo tk)
    tk2binop tk@(L _ TkOr)         = Or         (nodeInfo tk)
    tk2binop (L _ tk)              = error $ printf "Token %s does not correspond to a binary op" (show tk)

    tk2unop :: L Token -> Unop NodeInfo
    tk2unop tk@(L _ TkNot)    = Not        (nodeInfo tk)
    tk2unop tk@(L _ TkLength) = Length     (nodeInfo tk)
    tk2unop tk@(L _ TkDash)   = Negate     (nodeInfo tk)
    tk2unop tk@(L _ TkTilde)  = BitwiseNot (nodeInfo tk)
    tk2unop (L _ tk)          = error $ printf "Token %s does not correspond to a unary op" (show tk)

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
mkDo a b c = Do (nodeInfo a <> nodeInfo b <> nodeInfo c) (injectLoc a b)

mkWhile :: L Token -> Expression NodeInfo -> L Token -> Block NodeInfo -> L Token -> Statement NodeInfo
mkWhile a b c d e = While (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e) b (injectLoc c d)

mkRepeat :: L Token -> Block NodeInfo -> L Token -> Expression NodeInfo -> Statement NodeInfo
mkRepeat a b c d = Repeat (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d) (injectLoc a b) d

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
       ((b, injectLoc c d) :| map (\(_,x,y,z) -> (x, injectLoc y z)) e)
       (uncurry injectLoc <$> f)

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
mkFor a b c d e f g h i j = For ni b d f (snd <$> g) (injectLoc h i)
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
mkForIn a b c d e f g = ForIn ni b d (injectLoc e f)
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

mkFunDef :: L Token -> FunctionBody NodeInfo -> Expression NodeInfo
mkFunDef a b = FunDef (nodeInfo a <> nodeInfo b) b

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

mkParamList
    :: IdentList NodeInfo
    -> Maybe (L Token, L Token)
    -> ParamList NodeInfo
mkParamList a b = ParamList (nodeInfo a <> nodeInfo b) a (isJust b)

mkFunctionBody
    :: L Token
    -> Maybe (ParamList NodeInfo)
    -> L Token
    -> Block NodeInfo
    -> L Token
    -> FunctionBody NodeInfo
mkFunctionBody a b c d e = FunctionBody ni b (injectLoc c d)
  where
    ni = nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e

mkTableConstructor :: L Token -> Maybe (FieldList NodeInfo) -> L Token -> TableConstructor NodeInfo
mkTableConstructor a b c = TableConstructor (nodeInfo a <> nodeInfo b <> nodeInfo c) (fromMaybe (FieldList mempty []) b)

mkFieldExp :: L Token -> Expression NodeInfo -> L Token -> L Token -> Expression NodeInfo -> Field NodeInfo
mkFieldExp a b c d e = FieldExp (nodeInfo a <> nodeInfo b <> nodeInfo c <> nodeInfo d <> nodeInfo e) b e

mkFieldIdent :: Ident NodeInfo -> L Token -> Expression NodeInfo -> Field NodeInfo
mkFieldIdent a b c = FieldIdent (nodeInfo a <> nodeInfo b <> nodeInfo c) a c

mkField :: Expression NodeInfo -> Field NodeInfo
mkField a = Field (nodeInfo a) a

mkFieldList :: (NodeInfo, NonEmpty (Field NodeInfo)) -> Maybe (L Token) -> FieldList NodeInfo
mkFieldList (a,b) c = FieldList (a <> nodeInfo c) (NE.toList b)

-- Inspect a block to see if it contains a location or not; if not (i.e. it's
-- completely empty, as in "do end", we simply set its location to where it
-- would have been (the end of the preceding token). This is so a client can
-- report the location of an empty block as a warning, if they so desire.
injectLoc :: L Token -> Block NodeInfo -> Block NodeInfo
injectLoc a b =
    case b^.ann.nodeLoc of
        NoLoc -> b & ann.nodeLoc .~ locEnd (locOf a)
        _     -> b

--------------------------------------------------------------------------------
-- Combinators

sepBy :: (HasNodeInfo a, HasNodeInfo sep)
      => Prod r e t a
      -> Prod r e t sep
      -> Grammar r (Prod r e t (NodeInfo, [a]))
sepBy f sep = do
    fs <- sepBy1 f sep
    rule $ (_2 %~ NE.toList) <$> fs
       <|> pure mempty

sepBy1 :: forall r e t a sep. (HasNodeInfo a, HasNodeInfo sep)
       => Prod r e t a
       -> Prod r e t sep
       -> Grammar r (Prod r e t (NodeInfo, NonEmpty a))
sepBy1 f sep = mdo
    fs :: Prod r e t (NodeInfo, [a]) <-
        rule $ liftA3 (\a b (c,d) -> (nodeInfo a <> nodeInfo b <> c, b:d)) sep f fs
           <|> pure mempty
    rule $ liftA2 (\a (b,c) -> (nodeInfo a <> b, a :| c)) f fs

--------------------------------------------------------------------------------
-- Token productions

break :: Prod r String (L Token) (L Token)
break = locSymbol TkBreak <?> "'break'"

colon :: Prod r String (L Token) (L Token)
colon = locSymbol TkColon <?> "':'"

comma :: Prod r String (L Token) (L Token)
comma = locSymbol TkComma <?> "','"

do' :: Prod r String (L Token) (L Token)
do' = locSymbol TkDo <?> "'do'"

dot :: Prod r String (L Token) (L Token)
dot = locSymbol TkDot <?> "'.'"

label :: Prod r String (L Token) (L Token)
label = locSymbol TkLabel <?> "'::'"

else' :: Prod r String (L Token) (L Token)
else' = locSymbol TkElse <?> "'else'"

elseif :: Prod r String (L Token) (L Token)
elseif = locSymbol TkElseif <?> "'elseif'"

end :: Prod r String (L Token) (L Token)
end = locSymbol TkEnd <?> "'end'"

assign :: Prod r String (L Token) (L Token)
assign = locSymbol TkAssign <?> "'='"

false :: Prod r String (L Token) (L Token)
false = locSymbol TkFalse <?> "'false'"

floatLit :: Prod r String (L Token) (L Token)
floatLit = locSatisfy isFloatLit <?> "float"
  where
    isFloatLit :: Token -> Bool
    isFloatLit (TkFloatLit _) = True
    isFloatLit _ = False

for :: Prod r String (L Token) (L Token)
for = locSymbol TkFor <?> "'for'"

function :: Prod r String (L Token) (L Token)
function = locSymbol TkFunction <?> "'function'"

goto :: Prod r String (L Token) (L Token)
goto = locSymbol TkGoto <?> "'goto'"

ident :: Prod r String (L Token) (Ident NodeInfo)
ident = (\tk@(L _ (TkIdent s)) -> Ident (nodeInfo tk) s) <$> locSatisfy isIdent <?> "'ident'"
  where
    isIdent :: Token -> Bool
    isIdent (TkIdent _) = True
    isIdent _ = False

identListG :: Grammar r (Prod r String (L Token) (IdentList NodeInfo))
identListG = uncurry IdentList <$$> sepBy ident comma

identList1G :: Grammar r (Prod r String (L Token) (IdentList1 NodeInfo))
identList1G = uncurry IdentList1 <$$> sepBy1 ident comma

identSepByDotG :: Grammar r (Prod r String (L Token) (IdentList1 NodeInfo))
identSepByDotG = uncurry IdentList1 <$$> sepBy1 ident dot

if' :: Prod r String (L Token) (L Token)
if' = locSymbol TkIf <?> "'if'"

in' :: Prod r String (L Token) (L Token)
in' = locSymbol TkIn <?> "'in'"

intLit :: Prod r String (L Token) (L Token)
intLit = locSatisfy isIntLit <?> "integer"
  where
    isIntLit :: Token -> Bool
    isIntLit (TkIntLit _) = True
    isIntLit _ = False

lbrace :: Prod r String (L Token) (L Token)
lbrace = locSymbol TkLBrace <?> "'{'"

lbracket :: Prod r String (L Token) (L Token)
lbracket = locSymbol TkLBracket <?> "'['"

local :: Prod r String (L Token) (L Token)
local = locSymbol TkLocal <?> "'local'"

lparen :: Prod r String (L Token) (L Token)
lparen = locSymbol TkLParen <?> "'('"

nil :: Prod r String (L Token) (L Token)
nil = locSymbol TkNil <?> "'nil'"

rbrace :: Prod r String (L Token) (L Token)
rbrace = locSymbol TkRBrace <?> "'}'"

rbracket :: Prod r String (L Token) (L Token)
rbracket = locSymbol TkRBracket <?> "']'"

repeat :: Prod r String (L Token) (L Token)
repeat = locSymbol TkRepeat <?> "'repeat'"

return' :: Prod r String (L Token) (L Token)
return' = locSymbol TkReturn <?> "'return'"

rparen :: Prod r String (L Token) (L Token)
rparen = locSymbol TkRParen <?> "')'"

semi :: Prod r String (L Token) (L Token)
semi = locSymbol TkSemi <?> "';'"

stringLit :: Prod r String (L Token) (L Token)
stringLit = locSatisfy isStringLit <?> "string"
  where
    isStringLit :: Token -> Bool
    isStringLit (TkStringLit _) = True
    isStringLit _ = False

then' :: Prod r String (L Token) (L Token)
then' = locSymbol TkThen <?> "'then'"

vararg :: Prod r String (L Token) (L Token)
vararg = locSymbol TkVararg <?> "'...'"

true :: Prod r String (L Token) (L Token)
true = locSymbol TkTrue <?> "'true'"

until :: Prod r String (L Token) (L Token)
until = locSymbol TkUntil <?> "'until'"

while :: Prod r String (L Token) (L Token)
while = locSymbol TkWhile <?> "'while'"

--------------------------------------------------------------------------------
-- Earley extras

locSymbol :: Eq t => t -> Prod r e (L t) (L t)
locSymbol = symbol . L NoLoc

locSatisfy :: (t -> Bool) -> Prod r e (L t) (L t)
locSatisfy p = satisfy (p . unLoc)
