{- |
   Module      : Text.Pandoc.Lua.Marshaling.AnyValue
   Copyright   : © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Helper type to work with raw Lua stack indices instead of unmarshaled
values.

TODO: Most of this module should be abstracted, factored out, and go
into HsLua.
-}
module Text.Pandoc.Lua.Marshaling.AnyValue (AnyValue (..)) where

import HsLua.Core (StackIndex)
import HsLua.Class.Peekable (Peekable (peek))

-- | Dummy type to allow values of arbitrary Lua type. This just wraps
-- stack indices, using it requires extra care.
newtype AnyValue = AnyValue StackIndex

instance Peekable AnyValue where
  peek = return . AnyValue
