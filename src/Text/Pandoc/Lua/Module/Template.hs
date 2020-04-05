{- |
   Module      : Text.Pandoc.Lua.Module.Template
   Copyright   : © 2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Lua functions to access pandoc's templating system.
-}
module Text.Pandoc.Lua.Module.Template
  ( pushModule
  ) where

import Data.Text (Text)
import Foreign.Lua (NumResults (..))
import Text.DocLayout (Doc)
import Text.DocTemplates (Context, applyTemplate)
import Text.Pandoc.Lua.Marshaling.Context ()
import Text.Pandoc.Lua.PandocLua (PandocLua, addFunction, liftPandocLua)

import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Templates as Templates

-- | Push the "pandoc.template" module to the lua stack.
pushModule :: PandocLua NumResults
pushModule = do
  liftPandocLua Lua.newtable
  addFunction "apply_template"       apply_template
  addFunction "get_default_template" get_default_template
  addFunction "get_template"         get_template
  return 1

-- | Retrieve text for a template.
get_template :: FilePath -> PandocLua Text
get_template = Templates.getTemplate

-- | Get default template for the specified writer.
get_default_template :: Text -> PandocLua Text
get_default_template = Templates.getDefaultTemplate

-- | Compile a template and apply it to a context.
apply_template :: FilePath
               -> Text
               -> Context Text
               -> PandocLua NumResults
apply_template partialsSearchPath template context = do
  result <- Lua.liftIO $ applyTemplate partialsSearchPath template context
  liftPandocLua $ case result of
    Left e -> Lua.raiseError e
    Right doc -> do
      Lua.push (doc :: Doc Text)
      return (NumResults 1)
