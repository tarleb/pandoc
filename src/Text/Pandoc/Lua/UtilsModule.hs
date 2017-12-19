{-
Copyright © 2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{- |
   Module      : Text.Pandoc.Lua.PandocModule
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.UtilsModule
  ( pushUtilsModule
  ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Foreign.Lua (FromLuaStack, ToLuaStack, Lua, NumResults)
import System.Exit (ExitCode (..))
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (addFunction, addValue)
import Text.Pandoc.Process (pipeProcess)

import qualified Data.ByteString.Lazy as BSL
import qualified Foreign.Lua as Lua

-- | Push the "pandoc.utils" module to the lua stack.
pushUtilsModule :: Lua NumResults
pushUtilsModule = do
  Lua.newtable
  addFunction "sha1" sha1HashFn
  addFunction "pipe" pipeFn
  return 1

-- | Calculate the hash of the given contents.
sha1HashFn :: BSL.ByteString
           -> Lua String
sha1HashFn contents = return $ showDigest (sha1 contents)

-- | Pipes input through a command.
pipeFn :: String
       -> [String]
       -> BSL.ByteString
       -> Lua NumResults
pipeFn command args input = do
  (ec, output) <- liftIO $ pipeProcess Nothing command args input
  case ec of
    ExitSuccess -> do
      Lua.push output
      return 1
    ExitFailure n -> do
      Lua.push (PipeError command n output)
      fromIntegral <$> Lua.lerror

data PipeError = PipeError
  { pipeErrorCommand :: String
  , pipeErrorCode :: Int
  , pipeErrorOutput :: BSL.ByteString
  }

instance FromLuaStack PipeError where
  peek idx =
    PipeError
    <$> (Lua.getfield idx "command"    *> Lua.peek (-1) <* Lua.pop 1)
    <*> (Lua.getfield idx "error_code" *> Lua.peek (-1) <* Lua.pop 1)
    <*> (Lua.getfield idx "output"     *> Lua.peek (-1) <* Lua.pop 1)

instance ToLuaStack PipeError where
  push pipeErr = do
    Lua.newtable
    addValue "command" (pipeErrorCommand pipeErr)
    addValue "error_code" (pipeErrorCode pipeErr)
    addValue "output" (pipeErrorOutput pipeErr)
    pushPipeErrorMetaTable
    Lua.setmetatable (-2)
      where
        pushPipeErrorMetaTable :: Lua ()
        pushPipeErrorMetaTable = do
          v <- Lua.newmetatable "pandoc pipe error"
          when v $ addFunction "__tostring" pipeErrorMessage

        pipeErrorMessage :: PipeError -> Lua BSL.ByteString
        pipeErrorMessage (PipeError cmd errorCode output) = return $ mconcat
          [ pack "Error running "
          , pack cmd
          , pack " (error code "
          , pack $ show errorCode
          , pack "): "
          , if output == mempty then pack "<no output>" else output
          ]
