module Language.Lua.Pretty
    ( -- * Re-exports
      module Text.PrettyPrint.Leijen
    ) where

import Text.PrettyPrint.Leijen (Doc, Pretty(..), SimpleDoc(..), displayIO, displayS, renderPretty, renderCompact)
