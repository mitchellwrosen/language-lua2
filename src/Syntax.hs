-- | Abstract syntax of Lua 5.3 source files. See
-- <http://www.lua.org/manual/5.3/> for more information.

module Syntax where

import Data.Data
import Data.List.NonEmpty (NonEmpty(..))
import Lens.Micro

-- | An identifier, defined as any string of letters, digits, or underscores,
-- not beginning with a digit.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.1>
data Ident a = Ident a String
    deriving (Data, Functor, Show, Typeable)

-- | A chunk; Lua's compilation unit.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.3.2>
type Chunk a = Block a

-- | A block of statements, possibly ending in a return statement.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.3.1>
data Block a = Block a [Statement a] (Maybe (ReturnStatement a))
    deriving (Data, Functor, Show, Typeable)

data Statement a
    = EmptyStmt a                                                -- ^ @;@
    | Assign a (NonEmpty (Variable a)) (NonEmpty (Expression a)) -- ^ @var1, var2, var3 = exp1, exp2, exp3@
    | FunCall a (FunctionCall a)
    | Label a (Ident a)
    | Break a
    | Goto a (Ident a)
    | Do a (Block a)
    | While a (Expression a) (Block a)
    | Repeat a (Block a) (Expression a)
    | If a (NonEmpty (Expression a, Block a)) (Maybe (Block a))
    | For a (Ident a) (Expression a) (Expression a) (Maybe (Expression a)) (Block a)
    | ForIn a (NonEmpty (Ident a)) (NonEmpty (Expression a)) (Block a)
    | FunAssign a (NonEmpty (Ident a)) (Maybe (Ident a)) (FunctionBody a)
    | LocalFunAssign a (Ident a) (FunctionBody a)
    | LocalAssign a (NonEmpty (Ident a)) [Expression a]
    deriving (Data, Functor, Show, Typeable)

data ReturnStatement a = ReturnStatement a [Expression a]
    deriving (Data, Functor, Show, Typeable)

-- | There are three kinds of variables in Lua: global variables, local variables, and table fields.
--
-- <http://www.lua.org/manual/5.3/manual.html#3.2>
data Variable a
    = VarIdent a (Ident a)                           -- ^ A local or global variable.
    | VarField a (PrefixExpression a) (Expression a) -- ^ @table[exp]@
    | VarFieldName a (PrefixExpression a) (Ident a)  -- ^ @table.field@
    deriving (Data, Functor, Show, Typeable)

data Expression a
    = Nil a
    | Bool a Bool
    | Integer a String
    | Float a String
    | String a String
    | Vararg a
    | FunDef a (FunctionBody a)
    | PrefixExp a (PrefixExpression a)
    | TableConstructor a [Field a]
    | Binop a (Binop a) (Expression a) (Expression a)
    | Unop a (Unop a) (Expression a)
    deriving (Data, Functor, Show, Typeable)

data PrefixExpression a
    = PrefixVar a (Variable a)
    | PrefixFunCall a (FunctionCall a)
    | Parens a (Expression a)
    deriving (Data, Functor, Show, Typeable)

data FunctionCall a
    = FunctionCall a (PrefixExpression a) (FunctionArgs a)
    | MethodCall a (PrefixExpression a) (Ident a) (FunctionArgs a)
    deriving (Data, Functor, Show, Typeable)

data FunctionArgs a
    = Args a [Expression a]
    | ArgsTable a [Field a]
    | ArgsString a String
    deriving (Data, Functor, Show, Typeable)

data FunctionBody a
    = FunctionBody a [Ident a] (Maybe a) (Block a)
    deriving (Data, Functor, Show, Typeable)

data Field a
    = FieldExp a (Expression a) (Expression a) -- ^ @[exp1] = exp2@
    | FieldIdent a (Ident a) (Expression a)    -- ^ @name = exp@
    | Field a (Expression a)                   -- ^ @exp@
    deriving (Data, Functor, Show, Typeable)

data Binop a
    = Plus a       -- ^ +
    | Minus a      -- ^ -
    | Mult a       -- ^ *
    | FloatDiv a   -- ^ /
    | FloorDiv a   -- ^ //
    | Exponent a   -- ^ ^
    | Modulo a     -- ^ %
    | BitwiseAnd a -- ^ &
    | BitwiseXor a -- ^ ~
    | BitwiseOr a  -- ^ |
    | Rshift a     -- ^ >>
    | Lshift a     -- ^ <<
    | Concat a     -- ^ ..
    | Lt a         -- ^ <
    | Leq a        -- ^ <=
    | Gt a         -- ^ >
    | Geq a        -- ^ >=
    | Eq a         -- ^ ==
    | Neq a        -- ^ ~=
    | And a        -- ^ and
    | Or a         -- ^ or
    deriving (Data, Functor, Show, Typeable)

data Unop a
    = Negate a     -- -
    | Not a        -- not
    | Length a     -- #
    | BitwiseNot a -- ~
    deriving (Data, Functor, Show, Typeable)

--------------------------------------------------------------------------------
-- Annotated

class Functor ast => Annotated ast where
    ann :: Lens' (ast a) a

instance Annotated Ident where
    ann = lens (\(Ident a _) -> a) (\(Ident _ b) a -> Ident a b)

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
        f (FunAssign a _ _ _)    = a
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
        g (FunAssign _ b c d)    a = FunAssign a b c d
        g (LocalFunAssign _ b c) a = LocalFunAssign a b c
        g (LocalAssign _ b c)    a = LocalAssign a b c

instance Annotated ReturnStatement where
    ann = lens (\(ReturnStatement a _) -> a) (\(ReturnStatement _ b) a -> ReturnStatement a b)

instance Annotated Variable where
    ann = lens f g
      where
        f (VarIdent a _)       = a
        f (VarField a _ _)     = a
        f (VarFieldName a _ _) = a

        g (VarIdent _ b)       a = VarIdent a b
        g (VarField _ b c)     a = VarField a b c
        g (VarFieldName _ b c) a = VarFieldName a b c

instance Annotated Expression where
    ann = lens f g
      where
        f (Nil a)                = a
        f (Bool a _)             = a
        f (Integer a _)          = a
        f (Float a _)            = a
        f (String a _)           = a
        f (Vararg a)             = a
        f (FunDef a _)           = a
        f (PrefixExp a _)        = a
        f (TableConstructor a _) = a
        f (Binop a _ _ _)        = a
        f (Unop a _ _)           = a

        g (Nil _)                a = Nil a
        g (Bool _ b)             a = Bool a b
        g (Integer _ b)          a = Integer a b
        g (Float _ b)            a = Float a b
        g (String _ b)           a = String a b
        g (Vararg _)             a = Vararg a
        g (FunDef _ b)           a = FunDef a b
        g (PrefixExp _ b)        a = PrefixExp a b
        g (TableConstructor _ b) a = TableConstructor a b
        g (Binop _ b c d)        a = Binop a b c d
        g (Unop _ b c)           a = Unop a b c

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

instance Annotated Field where
    ann = lens f g
      where
        f (FieldExp a _ _)   = a
        f (FieldIdent a _ _) = a
        f (Field a _)        = a

        g (FieldExp _ b c)   a = FieldExp a b c
        g (FieldIdent _ b c) a = FieldIdent a b c
        g (Field _ b)        a = Field a b

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
