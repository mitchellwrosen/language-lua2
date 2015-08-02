module Syntax where

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
    [Statement]               -- ^ Block statements.
    [Expression]              -- ^ Block return expressions.
    deriving Show

data Statement
    = SSemi
    | SAssign
        [Variable]            -- ^ Variables.
        [Expression]          -- ^ Initializers.
    | SFunctionCall
        PrefixExpression
        (Maybe Ident)
        FunctionArgs
    | SLabel Ident
    | SBreak
    | SGoto Ident
    | SDo Block
    | SWhile
        Expression            -- ^ Condition.
        Block                 -- ^ Body.
    | SRepeat
        Block                 -- ^ Body.
        Expression            -- ^ Condition.
    | SIf
        Expression            -- ^ Condition.
        Block                 -- ^ Body.
        [(Expression, Block)] -- ^ Elseif branches.
        (Maybe Block)         -- ^ Else branch.
    | SFor
        Ident                 -- ^ Variable.
        Expression            -- ^ Initializer.
        Expression            -- ^ Condition.
        (Maybe Expression)    -- ^ Loop expression.
        Block                 -- ^ Body.
    | SForIn
        [Ident]               -- ^ Variables.
        [Expression]          -- ^ Initializers.
        Block                 -- ^ Body.
    | SFunctionDef
        Ident                 -- ^ Name.
        [Ident]               -- ^ More names interspersed with '.'
        (Maybe Ident)         -- ^ Final optional name after ':'
        [Ident]               -- ^ Arguments.
        Bool                  -- ^ Variadic?
        Block                 -- ^ Body.
    | SLocalFunction
        Ident                 -- ^ Name.
        [Ident]               -- ^ Arguments.
        Bool                  -- ^ Variadic?
        Block                 -- ^ Body.
    | SLocalAssign
        [Ident]               -- ^ Variables.
        [Expression]          -- ^ Initializers.
    deriving Show

data Variable
    = VIdent Ident
    | VIndex PrefixExpression Expression
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
        [Ident]               -- Arguments.
        Bool                  -- Variadic?
        Block                 -- Body.
    | EPrefixExp PrefixExpression
    | ETableConstructor [Field]
    | EBinop Expression Binop Expression
    | EUnop Unop Expression
    deriving Show

data PrefixExpression
    = PEVar Variable
    | PEFunctionCall PrefixExpression (Maybe Ident)
    | PEExpr Expression
    deriving Show

data FunctionArgs
    = FAExprs [Expression]
    | FATableConstructor [Field]
    | FAStringLit String
    deriving Show

data Field
    = FExprAssign Expression Expression
    | FIdentAssign Ident Expression
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
