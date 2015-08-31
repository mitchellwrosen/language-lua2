module Language.Lua.Token where

import Data.Data
import GHC.Generics (Generic)

data Token
    = TkAnd         -- ^ and
    | TkBreak       -- ^ break
    | TkDo          -- ^ do
    | TkElse        -- ^ else
    | TkElseif      -- ^ elseif
    | TkEnd         -- ^ end
    | TkFalse       -- ^ false
    | TkFor         -- ^ for
    | TkFunction    -- ^ function
    | TkGoto        -- ^ goto
    | TkIf          -- ^ if
    | TkIn          -- ^ in
    | TkLocal       -- ^ local
    | TkNil         -- ^ nil
    | TkNot         -- ^ not
    | TkOr          -- ^ or
    | TkRepeat      -- ^ repeat
    | TkReturn      -- ^ return
    | TkThen        -- ^ then
    | TkTrue        -- ^ true
    | TkUntil       -- ^ until
    | TkWhile       -- ^ while
    | TkPlus        -- ^ +
    | TkDash        -- ^ \-
    | TkMult        -- ^ \*
    | TkFloatDiv    -- ^ /
    | TkModulo      -- ^ %
    | TkExponent    -- ^ ^
    | TkLength      -- ^ #
    | TkBitwiseAnd  -- ^ &
    | TkTilde       -- ^ ~
    | TkBitwiseOr   -- ^ |
    | TkLShift      -- ^ <<
    | TkRShift      -- ^ \>\>
    | TkFloorDiv    -- ^ //
    | TkEq          -- ^ ==
    | TkNeq         -- ^ ~=
    | TkLeq         -- ^ <=
    | TkGeq         -- ^ \>=
    | TkLt          -- ^ <
    | TkGt          -- ^ >
    | TkAssign      -- ^ =
    | TkLParen      -- ^ (
    | TkRParen      -- ^ )
    | TkLBrace      -- ^ {
    | TkRBrace      -- ^ }
    | TkLBracket    -- ^ [
    | TkRBracket    -- ^ ]
    | TkLabel       -- ^ ::
    | TkSemi        -- ^ ;
    | TkColon       -- ^ :
    | TkComma       -- ^ ,
    | TkDot         -- ^ .
    | TkConcat      -- ^ ..
    | TkVararg      -- ^ ...
    | TkQuote       -- ^ '
    | TkDoubleQuote -- ^ "
    | TkIdent String
    | TkStringLit String
    | TkIntLit String
    | TkFloatLit String
    deriving (Data, Eq, Generic, Show, Typeable)

showToken :: Token -> String
showToken TkAnd           = "and"
showToken TkBreak         = "break"
showToken TkDo            = "do"
showToken TkElse          = "else"
showToken TkElseif        = "elseif"
showToken TkEnd           = "end"
showToken TkFalse         = "false"
showToken TkFor           = "for"
showToken TkFunction      = "function"
showToken TkGoto          = "goto"
showToken TkIf            = "if"
showToken TkIn            = "in"
showToken TkLocal         = "local"
showToken TkNil           = "nil"
showToken TkNot           = "not"
showToken TkOr            = "or"
showToken TkRepeat        = "repeat"
showToken TkReturn        = "return"
showToken TkThen          = "then"
showToken TkTrue          = "true"
showToken TkUntil         = "until"
showToken TkWhile         = "while"
showToken TkPlus          = "+"
showToken TkDash          = "-"
showToken TkMult          = "*"
showToken TkFloatDiv      = "/"
showToken TkModulo        = "%"
showToken TkExponent      = "^"
showToken TkLength        = "#"
showToken TkBitwiseAnd    = "&"
showToken TkTilde         = "~"
showToken TkBitwiseOr     = "|"
showToken TkLShift        = "<<"
showToken TkRShift        = ">>"
showToken TkFloorDiv      = "//"
showToken TkEq            = "=="
showToken TkNeq           = "~="
showToken TkLeq           = "<="
showToken TkGeq           = ">="
showToken TkLt            = "<"
showToken TkGt            = ">"
showToken TkAssign        = "="
showToken TkLParen        = "("
showToken TkRParen        = ")"
showToken TkLBrace        = "{"
showToken TkRBrace        = "}"
showToken TkLBracket      = "["
showToken TkRBracket      = "]"
showToken TkLabel         = "::"
showToken TkSemi          = ";"
showToken TkColon         = ":"
showToken TkComma         = ","
showToken TkDot           = "."
showToken TkConcat        = ".."
showToken TkVararg        = "..."
showToken TkQuote         = "'"
showToken TkDoubleQuote   = "\""
showToken (TkIdent s)     = s
showToken (TkStringLit s) = s
showToken (TkIntLit s)    = s
showToken (TkFloatLit s)  = s
