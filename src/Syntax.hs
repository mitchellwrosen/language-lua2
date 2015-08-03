module Syntax where

import Data.Loc

-- http://www.lua.org/manual/5.3/manual.html#9
-- -------------------------------------------
--
-- chunk ::= block
--
-- block ::= {stat} [retstat]
--
-- stat ::=  ‘;’ |
--      varlist ‘=’ explist |
--      functioncall |
--      label |
--      break |
--      goto Name |
--      do block end |
--      while exp do block end |
--      repeat block until exp |
--      if exp then block {elseif exp then block} [else block] end |
--      for Name ‘=’ exp ‘,’ exp [‘,’ exp] do block end |
--      for namelist in explist do block end |
--      function funcname funcbody |
--      local function Name funcbody |
--      local namelist [‘=’ explist]
--
-- retstat ::= return [explist] [‘;’]
--
-- label ::= ‘::’ Name ‘::’
--
-- funcname ::= Name {‘.’ Name} [‘:’ Name]
--
-- varlist ::= var {‘,’ var}
--
-- var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
--
-- namelist ::= Name {‘,’ Name}
--
-- explist ::= exp {‘,’ exp}
--
-- exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef |
--      prefixexp | tableconstructor | exp binop exp | unop exp
--
-- prefixexp ::= var | functioncall | ‘(’ exp ‘)’
--
-- functioncall ::=  prefixexp args | prefixexp ‘:’ Name args
--
-- args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
--
-- functiondef ::= function funcbody
--
-- funcbody ::= ‘(’ [parlist] ‘)’ block end
--
-- parlist ::= namelist [‘,’ ‘...’] | ‘...’
--
-- tableconstructor ::= ‘{’ [fieldlist] ‘}’
--
-- fieldlist ::= field {fieldsep field} [fieldsep]
--
-- field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
--
-- fieldsep ::= ‘,’ | ‘;’
--
-- binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |
--      ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
--      ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ |
--      and | or
--
-- unop ::= ‘-’ | not | ‘#’ | ‘~’

newtype Ident = Ident { unIdent :: String }
    deriving Show

data Block = Block
    [L Statement]                 -- ^ Block statements.
    [L Expression]                -- ^ Block return expressions.
    deriving Show

data Statement
    = SSemi
    | SAssign
        [L Variable]              -- ^ Variables.
        [L Expression]            -- ^ Initializers.
    | SFunctionCall
        (L PrefixExpression)
        (Maybe (L Ident))
        (L FunctionArgs)
    | SLabel (L Ident)
    | SBreak
    | SGoto (L Ident)
    | SDo (L Block)
    | SWhile
        (L Expression)            -- ^ Condition.
        (L Block)                 -- ^ Body.
    | SRepeat
        (L Block)                 -- ^ Body.
        (L Expression)            -- ^ Condition.
    | SIf
        (L Expression)            -- ^ Condition.
        (L Block)                 -- ^ Body.
        [(L Expression, L Block)] -- ^ Elseif branches.
        (Maybe (L Block))         -- ^ Else branch.
    | SFor
        (L Ident)                 -- ^ Variable.
        (L Expression)            -- ^ Initializer.
        (L Expression)            -- ^ Condition.
        (Maybe (L Expression))    -- ^ Loop expression.
        (L Block)                 -- ^ Body.
    | SForIn
        [L Ident]                 -- ^ Variables.
        [L Expression]            -- ^ Initializers.
        (L Block)                 -- ^ Body.
    | SFunctionDef
        (L Ident)                 -- ^ Name.
        [L Ident]                 -- ^ More names interspersed with '.'
        (Maybe (L Ident))         -- ^ Final optional name after ':'
        [L Ident]                 -- ^ Arguments.
        Bool                      -- ^ Variadic?
        (L Block)                 -- ^ Body.
    | SLocalFunction
        (L Ident)                 -- ^ Name.
        [L Ident]                 -- ^ Arguments.
        Bool                      -- ^ Variadic?
        (L Block)                 -- ^ Body.
    | SLocalAssign
        [L Ident]                 -- ^ Variables.
        [L Expression]            -- ^ Initializers.
    deriving Show

data Variable
    = VIdent (L Ident)
    | VIndex (L PrefixExpression) (L Expression)
    deriving Show

data Expression
    = ENil
    | ETrue
    | EFalse
    | EIntLit String
    | EFloatLit String
    | EStringLit String
    | ETripleDot
    | EFunctionDef
        [L Ident]                 -- Arguments.
        Bool                      -- Variadic?
        (L Block)                 -- Body.
    | EPrefixExp PrefixExpression
    | ETableConstructor [L Field]
    | EBinop (L Binop) (L Expression) (L Expression)
    | EUnop (L Unop) (L Expression)
    deriving Show

data PrefixExpression
    = PEVar Variable
    | PEFunctionCall (L PrefixExpression) (Maybe (L Ident)) (L FunctionArgs)
    | PEExpr (L Expression)
    deriving Show

data FunctionArgs
    = FAExprs [L Expression]
    | FATableConstructor [L Field]
    | FAStringLit String
    deriving Show

data Field
    = FExprAssign (L Expression) (L Expression)
    | FIdentAssign (L Ident) (L Expression)
    | FExpr Expression
    deriving Show

data Binop
    = BPlus         -- +
    | BDash         -- -
    | BStar         -- *
    | BFslash       -- /
    | BDoubleFslash -- //
    | BCarrot       -- ^
    | BPct          -- %
    | BAmp          -- &
    | BTilde        -- ~
    | BPipe         -- |
    | BRShift       -- >>
    | BLShift       -- <<
    | BDoubleDot    -- ..
    | BLt           -- <
    | BLeq          -- <=
    | BGt           -- >
    | BGeq          -- >=
    | BDoubleEq     -- ==
    | BNeq          -- ~=
    | BAnd          -- and
    | BOr           -- or
    deriving Show

data Unop
    = UDash  -- -
    | UNot   -- not
    | UHash  -- #
    | UTilde -- ~
    deriving Show
