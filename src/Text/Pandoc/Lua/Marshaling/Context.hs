{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Context
   Copyright   : © 2012-2021 John MacFarlane
                 © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for doctemplates Context and its components.
-}
module Text.Pandoc.Lua.Marshaling.Context () where

import Data.Text (Text, pack)
import Foreign.Lua (Peekable, Pushable)
import Foreign.Lua.Module.DocLayout ()
import Text.DocTemplates (Context(..), Val(..))
import Text.DocLayout (literal)
import qualified Foreign.Lua as Lua

instance Pushable (Context Text) where
  push (Context m) = Lua.push m

instance Pushable (Val Text) where
  push NullVal = Lua.push ()
  push (BoolVal b) = Lua.push b
  push (MapVal ctx) = Lua.push ctx
  push (ListVal xs) = Lua.push xs
  push (SimpleVal d) = Lua.push d

instance Peekable (Context Text) where
  peek idx = Context <$> Lua.peek idx

instance Peekable (Val Text) where
  peek idx = Lua.ltype idx >>= \case
    Lua.TypeNil      -> return NullVal
    Lua.TypeNumber   -> SimpleVal . literal <$> (Lua.peek idx :: Lua.Lua Text)
    Lua.TypeString   -> SimpleVal . literal <$> (Lua.peek idx :: Lua.Lua Text)
    Lua.TypeUserdata -> SimpleVal <$> Lua.peek idx
    Lua.TypeBoolean  -> do
                          b <- Lua.peek idx
                          let val = if b then "true" else "false"
                          return . SimpleVal . literal $ pack val
    Lua.TypeTable    -> do
                          len <- Lua.rawlen idx
                          if len <= 0
                            then MapVal <$> Lua.peek idx
                            else ListVal <$> Lua.peek idx
    _ -> Lua.throwMessage "not a Val"
