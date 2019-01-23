{- |
   Module      : Main
   Copyright   : © 2020-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Pandoc Lua interpreter.
-}
module Main where

import Control.Monad (forM_, when)
import Foreign.Lua (NumArgs, NumResults, Lua, Status)
import System.Environment (getArgs)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (runLua)

import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.UTF8 as UTF8

main :: IO ()
main = do
  args <- getArgs
  luaResult <- runIOorExplode . runLua $
    forM_ args $ \fp -> do
      result <- dofileWithTraceback fp
      when (result /= Lua.OK)
        Lua.throwTopMessage
  handleError luaResult

-- | Like @'Lua.pcall'@, but uses a predefined error handler which adds
-- a traceback on error.
pcallWithTraceback :: NumArgs -> NumResults -> Lua Status
pcallWithTraceback nargs nresults = do
  let traceback' :: Lua NumResults
      traceback' = do
        l <- Lua.state
        msg <- Lua.tostring' (Lua.nthFromBottom 1)
        Lua.traceback l (Just (UTF8.toString msg)) 2
        return 1
  tracebackIdx <- Lua.absindex (Lua.nthFromTop (Lua.fromNumArgs nargs + 1))
  Lua.pushHaskellFunction traceback'
  Lua.insert tracebackIdx
  result <- Lua.pcall nargs nresults (Just tracebackIdx)
  Lua.remove tracebackIdx
  return result

-- | Run the given string as a Lua program, while also adding a
-- traceback to the error message if an error occurs.
dofileWithTraceback :: FilePath -> Lua Status
dofileWithTraceback fp = do
  loadRes <- Lua.loadfile fp
  case loadRes of
    Lua.OK -> pcallWithTraceback 0 Lua.multret
    _ -> return loadRes
