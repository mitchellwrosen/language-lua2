-- | Lua pretty-printing re-exports. All AST nodes are instances of 'Pretty';
-- this module just re-exports types and top-level rendering functions from
-- <https://hackage.haskell.org/package/wl-pprint wl-pprint>.

module Language.Lua.Pretty
  ( -- * <https://hackage.haskell.org/package/wl-pprint wl-pprint> re-exports
    Doc
  , Pretty(..)
  , SimpleDoc(..)
  , renderPretty
  , renderCompact
  , displayS
  , displayIO
  ) where

import Text.PrettyPrint.Leijen
