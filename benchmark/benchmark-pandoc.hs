{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2012-2018 John MacFarlane <jgm@berkeley.edu>

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
import Prelude
import Text.Pandoc
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Format (KnownFormat (..), getDefaultExtensions)
import qualified Text.Pandoc.Format as Format
import Control.Monad.Except (throwError)
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Criterion.Main
import Criterion.Types (Config(..))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

readerBench :: Pandoc
            -> KnownFormat
            -> Maybe Benchmark
readerBench doc format =
  case res of
       Right (readerFun, inp) ->
          Just $ bench (Format.name format ++ " reader")
               $ nf (\i -> either (error . show) id $ runPure (readerFun i))
                 inp
       Left _ -> Nothing
  where res = runPure $
          case (knownFormatReader format, knownFormatWriter format) of
            (Just (TextReader r),
             Just (TextWriter w)) -> do
               let exts = Format.getDefaultExtensions format
               inp <- w def{ writerWrapText = WrapAuto
                           , writerExtensions = exts } doc
               return (r def{ readerExtensions = exts }, inp)
            _ -> throwError $ PandocSomeError
                 $ "could not get text reader and writer for " ++
                   Format.name format

writerBench :: Pandoc
            -> KnownFormat
            -> Maybe Benchmark
writerBench doc f =
  case res of
       Right writerFun ->
          Just $ bench (Format.name f ++ " writer")
               $ nf (\d -> either (error . show) id $
                            runPure (writerFun d)) doc
       Left _ -> Nothing
  where res = runPure $
          case knownFormatWriter f of
            Just (TextWriter w) ->
              return $ w def{ writerExtensions = getDefaultExtensions f }
            _ -> throwError $ PandocSomeError
                 $ "could not get text reader and writer for " ++
                   Format.name f

main :: IO ()
main = do
  args <- filter (\x -> take 1 x /= "-") <$> getArgs
  print args
  let formats = Set.fromList $ mapMaybe Format.formatFromName args
  let matchedReaders = if "readers" `elem` args
                       then formats `Set.intersection` textReaderFormats
                       else textReaderFormats
  let matchedWriters = if "writers" `elem` args
                       then formats `Set.intersection` textWriterFormats
                       else textWriterFormats
  inp <- UTF8.toText <$> B.readFile "test/testsuite.txt"
  let opts = def
  let doc = either (error . show) id $ runPure $ readMarkdown opts inp
  let readerBs = mapMaybe (readerBench doc) . Set.toList
                 $ Set.filter (/= Format.Haddock)
                 (matchedReaders `Set.intersection` matchedWriters)
                 -- we need the corresponding writer to generate
                 -- input for the reader
  let writerBs = mapMaybe (writerBench doc) $ Set.toList matchedWriters
  defaultMainWith defaultConfig{ timeLimit = 6.0 }
    (writerBs ++ readerBs)

textReaderFormats :: Set.Set KnownFormat
textReaderFormats = (`Set.filter` Format.allKnownFormats) $ \fmt ->
  case knownFormatReader fmt :: Maybe (Reader PandocPure) of
       Just (TextReader _) -> True
       _                   -> False

textWriterFormats :: Set.Set KnownFormat
textWriterFormats = (`Set.filter` Format.allKnownFormats) $ \fmt ->
  case knownFormatWriter fmt :: Maybe (Writer PandocPure) of
       Just (TextWriter _) -> True
       _                   -> False
