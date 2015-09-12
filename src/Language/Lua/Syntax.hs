{-# LANGUAGE CPP #-}

-- | Abstract syntax of Lua 5.3 source files. See
-- <http://www.lua.org/manual/5.3/> for more information.

module Language.Lua.Syntax
    ( -- * AST nodes
      Ident(..)
    , IdentList(..)
    , IdentList1(..)
    , Chunk
    , Block(..)
    , Statement(..)
    , ReturnStatement(..)
    , FunctionName(..)
    , Variable(..)
    , VariableList1(..)
    , Expression(..)
    , ExpressionList(..)
    , ExpressionList1(..)
    , PrefixExpression(..)
    , FunctionCall(..)
    , FunctionArgs(..)
    , FunctionBody(..)
    , TableConstructor(..)
    , Field(..)
    , FieldList(..)
    , Binop(..)
    , Unop(..)

    -- * Annotated typeclass
    , Annotated(..)
    ) where

import Data.Data
import Data.List.NonEmpty      (NonEmpty(..))
import GHC.Generics            (Generic)
import Lens.Micro
#if MIN_VERSION_base(4,8,0)
import Prelude                 hiding ((<$>))
#endif
import Text.PrettyPrint.Leijen

-- | An identifier, defined as any string of letters, digits, or underscores,
-- not beginning with a digit.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.1>
data Ident a
    = Ident !a !String
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | Zero or more 'Ident's.
data IdentList a
    = IdentList !a ![Ident a]
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | One or more 'Ident's.
data IdentList1 a
    = IdentList1 !a !(NonEmpty (Ident a))
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | A chunk; Lua's compilation unit.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.3.2>
type Chunk = Block

-- | A block of statements, possibly ending in a return statement.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.3.1>
data Block a
    = Block !a ![Statement a] !(Maybe (ReturnStatement a))
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | A statement.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.3>
data Statement a
    = EmptyStmt      !a                                                                               -- ^ @;@
    | Assign         !a !(VariableList1 a) !(ExpressionList1 a)                                       -- ^ @/var1/, /var2/, /var3/ = /exp1/, /exp2/, /exp3/@
    | FunCall        !a !(FunctionCall a)                                                             -- ^ @foo.bar(/args/)@
    | Label          !a !(Ident a)                                                                    -- ^ @::label::@
    | Break          !a                                                                               -- ^ @__break__@
    | Goto           !a !(Ident a)                                                                    -- ^ @__goto__ label@
    | Do             !a !(Block a)                                                                    -- ^ @__do__ /block/ __end__@
    | While          !a !(Expression a) !(Block a)                                                    -- ^ @__while__ /exp/ __do__ /block/ __end__@
    | Repeat         !a !(Block a) !(Expression a)                                                    -- ^ @__repeat__ /block/ __until__ /exp/@
    | If             !a !(NonEmpty (Expression a, Block a)) !(Maybe (Block a))                        -- ^ @__if__ /exp/ __then__ /block/ __else__ /block/ __end__@
    | For            !a !(Ident a) !(Expression a) !(Expression a) !(Maybe (Expression a)) !(Block a) -- ^ @__for__ x = /exp/ __do__ /block/ __end__@
    | ForIn          !a !(IdentList1 a) !(ExpressionList1 a) !(Block a)                               -- ^ @__for__ a, b, c __in__ /exp1/, /exp2/, /exp3/ __do__ /block/ __end__@
    | FunAssign      !a !(FunctionName a) !(FunctionBody a)                                           -- ^ @__function__ name /body/@
    | LocalFunAssign !a !(Ident a) !(FunctionBody a)                                                  -- ^ @__local function__ name /body/@
    | LocalAssign    !a !(IdentList1 a) !(ExpressionList a)                                           -- ^ @__local__ x, y, z@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data ReturnStatement a
    = ReturnStatement !a !(ExpressionList a) -- ^ @__return__ /exp1/, /exp2/@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data FunctionName a
    = FunctionName !a !(IdentList1 a) !(Maybe (Ident a)) -- ^ @foo.bar:baz@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | A variable.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.2>
data Variable a
    = VarIdent     !a !(Ident a)                            -- ^ @x@
    | VarField     !a !(PrefixExpression a) !(Expression a) -- ^ @/table/[/exp/]@
    | VarFieldName !a !(PrefixExpression a) !(Ident a)      -- ^ @/table/.field@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | One or more 'Variable's.
data VariableList1 a
    = VariableList1 !a !(NonEmpty (Variable a))
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | An expression.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.4>
data Expression a
    = Nil       !a
    | Bool      !a !Bool
    | Integer   !a !String
    | Float     !a !String
    | String    !a !String
    | Vararg    !a
    | FunDef    !a !(FunctionBody a)
    | PrefixExp !a !(PrefixExpression a)
    | TableCtor !a !(TableConstructor a)
    | Binop     !a !(Binop a) !(Expression a) !(Expression a)
    | Unop      !a !(Unop a) !(Expression a)
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | Zero or more 'Expression's.
data ExpressionList a
    = ExpressionList !a ![Expression a]
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | One or more 'Expression's.
data ExpressionList1 a
    = ExpressionList1 !a !(NonEmpty (Expression a))
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data PrefixExpression a
    = PrefixVar     !a !(Variable a)
    | PrefixFunCall !a !(FunctionCall a)
    | Parens        !a !(Expression a)
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | A function call. May be a statement or an expression.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.3.6>
--
-- <http://www.lua.org/manual/5.3/manual.html#3.4.10>
data FunctionCall a
    = FunctionCall !a !(PrefixExpression a) !(FunctionArgs a)
    | MethodCall   !a !(PrefixExpression a) !(Ident a) !(FunctionArgs a)
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data FunctionArgs a
    = Args       !a !(ExpressionList a)   -- ^ @(/exp1/, /exp2/)@
    | ArgsTable  !a !(TableConstructor a) -- ^ @{ x = /exp/ }@
    | ArgsString !a !String               -- ^ @"str"@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data FunctionBody a
    = FunctionBody !a !(IdentList a) !Bool !(Block a) -- ^ @(x, y, ...) /block/ __end__@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | A table constructor.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.4.9>
data TableConstructor a
    = TableConstructor !a !(FieldList a) -- ^ @{ x = 5, [f(1)] = 6, 7 }@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data Field a
    = FieldExp   !a !(Expression a) !(Expression a) -- ^ @[/exp1/] = /exp2/@
    | FieldIdent !a !(Ident a) !(Expression a)      -- ^ @name = /exp/@
    | Field      !a !(Expression a)                 -- ^ @/exp/@
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

-- | Zero or more 'Field's, separated by @,@ or @;@.
data FieldList a
    = FieldList !a ![Field a]
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data Binop a
    = Plus       !a -- ^ +
    | Minus      !a -- ^ \-
    | Mult       !a -- ^ \*
    | FloatDiv   !a -- ^ /
    | FloorDiv   !a -- ^ //
    | Exponent   !a -- ^ \^
    | Modulo     !a -- ^ %
    | BitwiseAnd !a -- ^ &
    | BitwiseXor !a -- ^ ~
    | BitwiseOr  !a -- ^ |
    | Rshift     !a -- ^ \>\>
    | Lshift     !a -- ^ <<
    | Concat     !a -- ^ ..
    | Lt         !a -- ^ <
    | Leq        !a -- ^ <=
    | Gt         !a -- ^ \>
    | Geq        !a -- ^ \>=
    | Eq         !a -- ^ ==
    | Neq        !a -- ^ ~=
    | And        !a -- ^ and
    | Or         !a -- ^ or
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

data Unop a
    = Negate     !a -- ^ \-
    | Not        !a -- ^ not
    | Length     !a -- ^ #
    | BitwiseNot !a -- ^ ~
    deriving (Data, Eq, Functor, Generic, Show, Typeable)

--------------------------------------------------------------------------------
-- Pretty

instance Pretty (Ident a) where
    pretty (Ident _ s) = text s

instance Pretty (Block a) where
    pretty (Block _ ss mr) =
            vsep (map pretty ss)
        <$> maybe empty pretty mr

instance Pretty (Statement a) where
    pretty (EmptyStmt _) = char ';'
    pretty (Assign _ (VariableList1 _ (v:|vs)) (ExpressionList1 _ (e:|es))) =
            sepBy (text ", ") (v:vs)
        <+> char '='
        <+> sepBy (text ", ") (e:es)
    pretty (FunCall _ f) = pretty f
    pretty (Label _ i) = text "::" <> pretty i <> text "::"
    pretty (Break _) = text "break"
    pretty (Goto _ i) = text "goto" <+> pretty i
    pretty (Do _ b) =
            text "do"
        <$> indent 4 (pretty b)
        <$> text "end"
    pretty (While _ e b) =
            text "while" <+> pretty e <+> text "do"
        <$> nest 4 (pretty b)
        <$> text "end"
    pretty (Repeat _ b e) =
            text "repeat"
        <$> nest 4 (pretty b)
        <$> text "until" <+> pretty e
    pretty (If _ ((e,b):|es) mb'') =
            text "if" <+> pretty e <+> text "then"
        <$> indent 4 (pretty b)
        <$> vsep (map (\(e',b') -> text "elseif" <+> pretty e' <+> text "then"
                                   <$> indent 4 (pretty b')) es)
        <$> maybe empty (\b'' -> text "else"
                                 <$> indent 4 (pretty b'')) mb''
        <$> text "end"
    pretty (For _ i e1 e2 me3 b) =
            text "for" <+> pretty i <+> char '=' <+> pretty e1 <> char ',' <+> pretty e2 <> maybe empty ((char ',' <+>) . pretty) me3 <+> text "do"
        <$> indent 4 (pretty b)
        <$> text "end"
    pretty (ForIn _ (IdentList1 _ (i:|is)) (ExpressionList1 _ (e:|es)) b) =
            text "for" <+> sepBy (text ", ") (i:is) <+> text "in" <+> sepBy (text ", ") (e:es) <+> text "do"
        <$> indent 4 (pretty b)
        <$> text "end"
    pretty (FunAssign _ n b) =
            text "function" <+> pretty n <> pretty b
    pretty (LocalFunAssign _ i b) =
            text "local" <+> text "function" <+> pretty i <> pretty b
    pretty (LocalAssign _ (IdentList1 _ (i:|is)) (ExpressionList _ es)) =
            text "local"
        <+> sepBy (text ", ") (i:is)
        <+> case es of
                [] -> empty
                _  -> char '=' <+> sepBy (text ", ") es

instance Pretty (ReturnStatement a) where
    pretty (ReturnStatement _ (ExpressionList _ es)) = text "return" <+> sepBy (text ", ") es

instance Pretty (FunctionName a) where
    pretty (FunctionName _ (IdentList1 _ (i:|is)) mi) = sepBy (char '.') (i:is) <> maybe empty ((char ':' <>) . pretty) mi

instance Pretty (Variable a) where
    pretty (VarIdent _ i)       = pretty i
    pretty (VarField _ e1 e2)   = pretty e1 <> brackets (pretty e2)
    pretty (VarFieldName _ e i) = pretty e <> char '.' <> pretty i

instance Pretty (Expression a) where
    pretty (Nil _)            = text "nil"
    pretty (Bool _ True)      = text "true"
    pretty (Bool _ _)         = text "false"
    pretty (Integer _ s)      = text s
    pretty (Float _ s)        = text s
    pretty (String _ s)       = dquotes (string s)
    pretty (Vararg _)         = text "..."
    pretty (FunDef _ b)       = text "function" <> pretty b
    pretty (PrefixExp _ e)    = pretty e
    pretty (TableCtor _ t)    = pretty t
    pretty (Binop _ op e1 e2) = pretty e1 <+> pretty op <+> pretty e2
    pretty (Unop _ op e)      = pretty op <> pretty e

instance Pretty (PrefixExpression a) where
    pretty (PrefixVar _ v)     = pretty v
    pretty (PrefixFunCall _ c) = pretty c
    pretty (Parens _ e)        = parens (pretty e)

instance Pretty (FunctionCall a) where
    pretty (FunctionCall _ e a) = pretty e <> pretty a
    pretty (MethodCall _ e i a) = pretty e <> char ':' <> pretty i <> pretty a

instance Pretty (FunctionArgs a) where
    pretty (Args _ (ExpressionList _ es)) = encloseSep lparen rparen (text ", ") (map pretty es)
    pretty (ArgsTable _ t)                = pretty t
    pretty (ArgsString _ s)               = dquotes (string s)

instance Pretty (FunctionBody a) where
    pretty (FunctionBody _ (IdentList _ is) va b) =
            encloseSep lparen rhs (text ", ") (map pretty is)
        <$> indent 4 (pretty b)
        <$> text "end"
      where
        rhs = if va
                  then comma <+> text "..." <> rparen
                  else rparen

instance Pretty (TableConstructor a) where
    pretty (TableConstructor _ (FieldList _ []))  = lbrace <+> rbrace
    pretty (TableConstructor _ (FieldList _ [f])) = lbrace <+> pretty f <+> rbrace
    pretty (TableConstructor _ (FieldList _ fs))  =
            lbrace
        <$> indent 4 (vsep (map pretty fs))
        <$> rbrace

instance Pretty (Field a) where
    pretty (FieldExp _ e1 e2) = brackets (pretty e1) <+> char '=' <+> pretty e2
    pretty (FieldIdent _ i e) = pretty i <+> char '=' <+> pretty e
    pretty (Field _ e)        = pretty e

instance Pretty (Binop a) where
    pretty (Plus _)       = char '+'
    pretty (Minus _)      = char '-'
    pretty (Mult _)       = char '*'
    pretty (FloatDiv _)   = char '/'
    pretty (FloorDiv _)   = text "//"
    pretty (Exponent _)   = char '^'
    pretty (Modulo _)     = char '%'
    pretty (BitwiseAnd _) = char '&'
    pretty (BitwiseXor _) = char '~'
    pretty (BitwiseOr _)  = char '|'
    pretty (Rshift _)     = text ">>"
    pretty (Lshift _)     = text "<<"
    pretty (Concat _)     = text ".."
    pretty (Lt _)         = text "<"
    pretty (Leq _)        = text "<="
    pretty (Gt _)         = text ">"
    pretty (Geq _)        = text ">="
    pretty (Eq _)         = text "=="
    pretty (Neq _)        = text "~="
    pretty (And _)        = text "and"
    pretty (Or _)         = text "or"

instance Pretty (Unop a) where
    pretty (Negate _)     = char '-'
    pretty (Not _)        = text "not"
    pretty (Length _)     = char '#'
    pretty (BitwiseNot _) = char '~'

sepBy :: Pretty a => Doc -> [a] -> Doc
sepBy d = align . cat . punctuate d . map pretty

--------------------------------------------------------------------------------
-- Annotated

class Functor ast => Annotated ast where
    ann :: Lens' (ast a) a

instance Annotated Ident where
    ann = lens (\(Ident a _) -> a) (\(Ident _ b) a -> Ident a b)

instance Annotated IdentList where
    ann = lens (\(IdentList a _) -> a) (\(IdentList _ b) a -> IdentList a b)

instance Annotated IdentList1 where
    ann = lens (\(IdentList1 a _) -> a) (\(IdentList1 _ b) a -> IdentList1 a b)

instance Annotated Block where
    ann = lens (\(Block a _ _) -> a) (\(Block _ b c) a -> Block a b c)

instance Annotated Statement where
    ann = lens f g
      where
        f (EmptyStmt a)          = a
        f (Assign a _ _)         = a
        f (FunCall a _)          = a
        f (Label a _)            = a
        f (Break a)              = a
        f (Goto a _)             = a
        f (Do a _)               = a
        f (While a _ _)          = a
        f (Repeat a _ _)         = a
        f (If a _ _)             = a
        f (For a _ _ _ _ _)      = a
        f (ForIn a _ _ _)        = a
        f (FunAssign a _ _)      = a
        f (LocalFunAssign a _ _) = a
        f (LocalAssign a _ _)    = a

        g (EmptyStmt _)          a = EmptyStmt a
        g (Assign _ b c)         a = Assign a b c
        g (FunCall _ b)          a = FunCall a b
        g (Label _ b)            a = Label a b
        g (Break _)              a = Break a
        g (Goto _ b)             a = Goto a b
        g (Do _ b)               a = Do a b
        g (While _ b c)          a = While a b c
        g (Repeat _ b c)         a = Repeat a b c
        g (If _ b c)             a = If a b c
        g (For _ b c d e h)      a = For a b c d e h
        g (ForIn _ b c d)        a = ForIn a b c d
        g (FunAssign _ b c)      a = FunAssign a b c
        g (LocalFunAssign _ b c) a = LocalFunAssign a b c
        g (LocalAssign _ b c)    a = LocalAssign a b c

instance Annotated ReturnStatement where
    ann = lens (\(ReturnStatement a _) -> a) (\(ReturnStatement _ b) a -> ReturnStatement a b)

instance Annotated FunctionName where
    ann = lens (\(FunctionName a _ _) -> a) (\(FunctionName _ b c) a -> FunctionName a b c)

instance Annotated Variable where
    ann = lens f g
      where
        f (VarIdent a _)       = a
        f (VarField a _ _)     = a
        f (VarFieldName a _ _) = a

        g (VarIdent _ b)       a = VarIdent a b
        g (VarField _ b c)     a = VarField a b c
        g (VarFieldName _ b c) a = VarFieldName a b c

instance Annotated VariableList1 where
    ann = lens (\(VariableList1 a _) -> a) (\(VariableList1 _ b) a -> VariableList1 a b)

instance Annotated Expression where
    ann = lens f g
      where
        f (Nil a)         = a
        f (Bool a _)      = a
        f (Integer a _)   = a
        f (Float a _)     = a
        f (String a _)    = a
        f (Vararg a)      = a
        f (FunDef a _)    = a
        f (PrefixExp a _) = a
        f (TableCtor a _) = a
        f (Binop a _ _ _) = a
        f (Unop a _ _)    = a

        g (Nil _)         a = Nil a
        g (Bool _ b)      a = Bool a b
        g (Integer _ b)   a = Integer a b
        g (Float _ b)     a = Float a b
        g (String _ b)    a = String a b
        g (Vararg _)      a = Vararg a
        g (FunDef _ b)    a = FunDef a b
        g (PrefixExp _ b) a = PrefixExp a b
        g (TableCtor _ b) a = TableCtor a b
        g (Binop _ b c d) a = Binop a b c d
        g (Unop _ b c)    a = Unop a b c

instance Annotated ExpressionList where
    ann = lens (\(ExpressionList a _) -> a) (\(ExpressionList _ b) a -> ExpressionList a b)

instance Annotated ExpressionList1 where
    ann = lens (\(ExpressionList1 a _) -> a) (\(ExpressionList1 _ b) a -> ExpressionList1 a b)

instance Annotated PrefixExpression where
    ann = lens f g
      where
        f (PrefixVar a _)     = a
        f (PrefixFunCall a _) = a
        f (Parens a _)        = a

        g (PrefixVar _ b)     a = PrefixVar a b
        g (PrefixFunCall _ b) a = PrefixFunCall a b
        g (Parens _ b)        a = Parens a b

instance Annotated FunctionCall where
    ann = lens f g
      where
        f (FunctionCall a _ _) = a
        f (MethodCall a _ _ _) = a

        g (FunctionCall _ b c) a = FunctionCall a b c
        g (MethodCall _ b c d) a = MethodCall a b c d

instance Annotated FunctionArgs where
    ann = lens f g
      where
        f (Args a _)       = a
        f (ArgsTable a _)  = a
        f (ArgsString a _) = a

        g (Args _ b)       a = Args a b
        g (ArgsTable _ b)  a = ArgsTable a b
        g (ArgsString _ b) a = ArgsString a b

instance Annotated FunctionBody where
    ann = lens (\(FunctionBody a _ _ _) -> a) (\(FunctionBody _ b c d) a -> FunctionBody a b c d)

instance Annotated TableConstructor where
    ann = lens (\(TableConstructor a _) -> a) (\(TableConstructor _ b) a -> TableConstructor a b)

instance Annotated Field where
    ann = lens f g
      where
        f (FieldExp a _ _)   = a
        f (FieldIdent a _ _) = a
        f (Field a _)        = a

        g (FieldExp _ b c)   a = FieldExp a b c
        g (FieldIdent _ b c) a = FieldIdent a b c
        g (Field _ b)        a = Field a b

instance Annotated FieldList where
    ann = lens (\(FieldList a _) -> a) (\(FieldList _ b) a -> FieldList a b)

instance Annotated Binop where
    ann = lens f g
      where
        f (Plus a)       = a
        f (Minus a)      = a
        f (Mult a)       = a
        f (FloatDiv a)   = a
        f (FloorDiv a)   = a
        f (Exponent a)   = a
        f (Modulo a)     = a
        f (BitwiseAnd a) = a
        f (BitwiseXor a) = a
        f (BitwiseOr a)  = a
        f (Rshift a)     = a
        f (Lshift a)     = a
        f (Concat a)     = a
        f (Lt a)         = a
        f (Leq a)        = a
        f (Gt a)         = a
        f (Geq a)        = a
        f (Eq a)         = a
        f (Neq a)        = a
        f (And a)        = a
        f (Or a)         = a

        g (Plus _)       a = Plus a
        g (Minus _)      a = Minus a
        g (Mult _)       a = Mult a
        g (FloatDiv _)   a = FloatDiv a
        g (FloorDiv _)   a = FloorDiv a
        g (Exponent _)   a = Exponent a
        g (Modulo _)     a = Modulo a
        g (BitwiseAnd _) a = BitwiseAnd a
        g (BitwiseXor _) a = BitwiseXor a
        g (BitwiseOr _)  a = BitwiseOr a
        g (Rshift _)     a = Rshift a
        g (Lshift _)     a = Lshift a
        g (Concat _)     a = Concat a
        g (Lt _)         a = Lt a
        g (Leq _)        a = Leq a
        g (Gt _)         a = Gt a
        g (Geq _)        a = Geq a
        g (Eq _)         a = Eq a
        g (Neq _)        a = Neq a
        g (And _)        a = And a
        g (Or _)         a = Or a

instance Annotated Unop where
    ann = lens f g
      where
        f (Negate a)     = a
        f (Not a)        = a
        f (Length a)     = a
        f (BitwiseNot a) = a

        g (Negate _)     a = Negate a
        g (Not _)        a = Not a
        g (Length _)     a = Length a
        g (BitwiseNot _) a = BitwiseNot a
