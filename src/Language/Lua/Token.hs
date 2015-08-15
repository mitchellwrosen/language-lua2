module Language.Lua.Token where

import Data.Data
import GHC.Generics (Generic)

data Token
    = TkAnd              -- ^ and
    | TkBreak            -- ^ break
    | TkDo               -- ^ do
    | TkElse             -- ^ else
    | TkElseif           -- ^ elseif
    | TkEnd              -- ^ end
    | TkFalse            -- ^ false
    | TkFor              -- ^ for
    | TkFunction         -- ^ function
    | TkGoto             -- ^ goto
    | TkIf               -- ^ if
    | TkIn               -- ^ in
    | TkLocal            -- ^ local
    | TkNil              -- ^ nil
    | TkNot              -- ^ not
    | TkOr               -- ^ or
    | TkRepeat           -- ^ repeat
    | TkReturn           -- ^ return
    | TkThen             -- ^ then
    | TkTrue             -- ^ true
    | TkUntil            -- ^ until
    | TkWhile            -- ^ while
    | TkPlus             -- ^ +
    | TkDash             -- ^ -
    | TkStar             -- ^ *
    | TkFslash           -- ^ /
    | TkPct              -- ^ %
    | TkCarrot           -- ^ ^
    | TkHash             -- ^ #
    | TkAmp              -- ^ &
    | TkTilde            -- ^ ~
    | TkPipe             -- ^ |
    | TkLShift           -- ^ <<
    | TkRShift           -- ^ >>
    | TkDoubleFslash     -- ^ //
    | TkDoubleEq         -- ^ ==
    | TkNeq              -- ^ ~=
    | TkLeq              -- ^ <=
    | TkGeq              -- ^ >=
    | TkLt               -- ^ <
    | TkGt               -- ^ >
    | TkEq               -- ^ =
    | TkLParen           -- ^ (
    | TkRParen           -- ^ )
    | TkLBrace           -- ^ {
    | TkRBrace           -- ^ }
    | TkLBracket         -- ^ [
    | TkRBracket         -- ^ ]
    | TkDoubleColon      -- ^ ::
    | TkSemi             -- ^ ;
    | TkColon            -- ^ :
    | TkComma            -- ^ ,
    | TkDot              -- ^ .
    | TkDoubleDot        -- ^ ..
    | TkTripleDot        -- ^ ...
    | TkUnderscore       -- ^ _
    | TkQuote            -- ^ '
    | TkDoubleQuote      -- ^ "
    | TkIdent String
    | TkStringLit String
    | TkIntLit String
    | TkFloatLit String
    deriving (Data, Eq, Generic, Show, Typeable)
