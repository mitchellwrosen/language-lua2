-- | Abstract syntax of Lua 5.3 source files. See
-- <http://www.lua.org/manual/5.3/> for more information.

module Syntax where

import Data.Data
import Data.List.NonEmpty
import Data.Loc

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
    = EmptyStmt a                          -- ^ @;@
    | Assign a [Variable a] [Expression a] -- ^ @var1, var2, var3 = exp1, exp2, exp3@
    | FunCall a (FunctionCall a)
    | Label a (Ident a)
    | Break a
    | Goto a (Ident a)
    | Do a (Block a)
    | While a (Expression a) (Block a)
    | Repeat a (Block a) (Expression a)
    | If a (NonEmpty (Expression a, Block a)) (Maybe (Block a))
    | For a (Ident a) (Expression a) (Expression a) (Maybe (Expression a)) (Block a)
    | ForIn a [Ident a] [Expression a] (Block a)
    | FunAssign a (Ident a) [Ident a] (Maybe (Ident a)) (FunctionBody a)
    | LocalFunAssign a (Ident a) (FunctionBody a)
    | LocalAssign a [Ident a] [Expression a]
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
    ann :: ast a -> a
    ann = undefined

    amap :: (a -> a) -> ast a -> ast a
    amap = undefined

-- TODO: All of these

instance Annotated Ident where

instance Annotated Block where

instance Annotated Statement where

instance Annotated ReturnStatement where

instance Annotated Variable where

instance Annotated Expression where

instance Annotated PrefixExpression where

instance Annotated FunctionCall where

instance Annotated FunctionArgs where

instance Annotated FunctionBody where

instance Annotated Field where

instance Annotated Binop where

instance Annotated Unop where

-- Unfortunate orphans
deriving instance Functor L
