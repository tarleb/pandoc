{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.App
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Does a pandoc conversion based on command-line options.
-}
module Text.Pandoc.App.OutputSettings
  ( OutputSettings (..)
  , optToOutputSettings
  ) where
import Prelude
import Control.Monad
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans
import Data.Char (toLower)
import Data.List (find, isPrefixOf)
import Data.Maybe (fromMaybe)
import Skylighting (defaultSyntaxMap)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (stdout)
import Text.Pandoc hiding (DZSlides)
import Text.Pandoc.App.Opt (Opt (..))
import Text.Pandoc.App.CommandLineOptions (engines)
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Format
import qualified Text.Pandoc.UTF8 as UTF8

-- | Settings specifying how document output should be produced.
data OutputSettings = OutputSettings
  { outputFormat :: IOFormat
  , outputWriterOptions :: WriterOptions
  , outputPdfProgram :: Maybe String
  }

readUtf8File :: PandocMonad m => FilePath -> m String
readUtf8File = fmap UTF8.toString . readFileStrict

-- | Get output settings from command line options.
optToOutputSettings :: Opt -> PandocIO OutputSettings
optToOutputSettings opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)

  when (optDumpArgs opts) . liftIO $ do
    UTF8.hPutStrLn stdout outputFile
    mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
    exitSuccess

  epubMetadata <- case optEpubMetadata opts of
                         Nothing -> return Nothing
                         Just fp -> Just <$> readUtf8File fp

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"
  (Flavored ioFormat writerExts, maybePdfProg) <-
    if pdfOutput
       then pdfWriterAndProg (optWriter opts) (optPdfEngine opts)
       else (,) <$> (case optWriter opts of
                       Just f  -> writerFormat f
                       Nothing -> return .
                                  fromMaybe (withDefaultExtensions HTML5) $
                                  flavoredFormatFromFilePath outputFile)
                <*> pure Nothing

  let standalone = optStandalone opts || not (isTextFormat ioFormat) || pdfOutput

  let addStringAsVariable varname s vars = return $ (varname, s) : vars

  let addSyntaxMap existingmap f = do
        res <- liftIO (parseSyntaxDefinition f)
        case res of
              Left errstr -> throwError $ PandocSyntaxMapError errstr
              Right syn   -> return $ addSyntaxDefinition syn existingmap

  syntaxMap <- foldM addSyntaxMap defaultSyntaxMap
                     (optSyntaxDefinitions opts)

  -- note: this reverses the list constructed in option parsing,
  -- which in turn was reversed from the command-line order,
  -- so we end up with the correct order in the variable list:
  let withList _ [] vars     = return vars
      withList f (x:xs) vars = f x vars >>= withList f xs

  let addContentsAsVariable varname fp vars = do
        s <- UTF8.toString . fst <$> fetchItem fp
        return $ (varname, s) : vars

  variables <-
    withList (addStringAsVariable "sourcefile")
             (reverse $ optInputFiles opts)
             (("outputfile", fromMaybe "-" (optOutputFile opts))
              : optVariables opts)
             -- we reverse this list because, unlike
             -- the other option lists here, it is
             -- not reversed when parsed from CLI arguments.
             -- See withList, above.
    >>=
    withList (addContentsAsVariable "include-before")
             (optIncludeBeforeBody opts)
    >>=
    withList (addContentsAsVariable "include-after")
             (optIncludeAfterBody opts)
    >>=
    withList (addContentsAsVariable "header-includes")
             (optIncludeInHeader opts)
    >>=
    withList (addStringAsVariable "css") (optCss opts)
    >>=
    maybe return (addStringAsVariable "title-prefix")
                 (optTitlePrefix opts)
    >>=
    maybe return (addStringAsVariable "epub-cover-image")
                 (optEpubCoverImage opts)
    >>=
    (\vars ->  if ioFormat == IOFormat DZSlides
                  then do
                      dztempl <- UTF8.toString <$> readDataFile
                                   ("dzslides" </> "template.html")
                      let dzline = "<!-- {{{{ dzslides core"
                      let dzcore = unlines
                                 $ dropWhile (not . (dzline `isPrefixOf`))
                                 $ lines dztempl
                      return $ ("dzslides-core", dzcore) : vars
                  else return vars)

  templ <- case optTemplate opts of
                  _ | not standalone -> return Nothing
                  Nothing -> case unIOFormat ioFormat of
                               Nothing -> return Nothing
                               Just f  -> Just <$> getDefaultTemplate f
                  Just tp -> do
                    -- strip off extensions
                    let tp' = case takeExtension tp of
                                   "" -> tp <.> ioFormatName ioFormat
                                   _  -> tp
                    Just . UTF8.toString <$>
                          ((fst <$> fetchItem tp') `catchError`
                           (\e ->
                               case e of
                                    PandocResourceNotFound _ ->
                                       readDataFile ("templates" </> tp')
                                    _ -> throwError e))

  case lookup "lang" (optMetadata opts) of
         Just l  -> case parseBCP47 l of
                         Left _   -> return ()
                         Right l' -> setTranslations l'
         Nothing -> setTranslations $ Lang "en" "" "US" []

  let writerOpts = def {
          writerTemplate         = templ
        , writerVariables        = variables
        , writerTabStop          = optTabStop opts
        , writerTableOfContents  = optTableOfContents opts
        , writerHTMLMathMethod   = optHTMLMathMethod opts
        , writerIncremental      = optIncremental opts
        , writerCiteMethod       = optCiteMethod opts
        , writerNumberSections   = optNumberSections opts
        , writerNumberOffset     = optNumberOffset opts
        , writerSectionDivs      = optSectionDivs opts
        , writerExtensions       = writerExts
        , writerReferenceLinks   = optReferenceLinks opts
        , writerReferenceLocation = optReferenceLocation opts
        , writerDpi              = optDpi opts
        , writerWrapText         = optWrapText opts
        , writerColumns          = optColumns opts
        , writerEmailObfuscation = optEmailObfuscation opts
        , writerIdentifierPrefix = optIdentifierPrefix opts
        , writerHtmlQTags        = optHtmlQTags opts
        , writerTopLevelDivision = optTopLevelDivision opts
        , writerListings         = optListings opts
        , writerSlideLevel       = optSlideLevel opts
        , writerHighlightStyle   = optHighlightStyle opts
        , writerSetextHeaders    = optSetextHeaders opts
        , writerEpubSubdirectory = optEpubSubdirectory opts
        , writerEpubMetadata     = epubMetadata
        , writerEpubFonts        = optEpubFonts opts
        , writerEpubChapterLevel = optEpubChapterLevel opts
        , writerTOCDepth         = optTOCDepth opts
        , writerReferenceDoc     = optReferenceDoc opts
        , writerSyntaxMap        = syntaxMap
        , writerPreferAscii      = optAscii opts
        }
  return $ OutputSettings
    { outputFormat = ioFormat
    , outputWriterOptions = writerOpts
    , outputPdfProgram = maybePdfProg
    }

pdfIsNoWriterErrorMsg :: String
pdfIsNoWriterErrorMsg =
  "To create a pdf using pandoc, use " ++
  "-t latex|beamer|context|ms|html5" ++
  "\nand specify an output file with " ++
  ".pdf extension (-o filename.pdf)."

pdfWriterAndProg :: PandocMonad m
                 => Maybe String              -- ^ user-specified writer format
                 -> Maybe String              -- ^ user-specified pdf-engine
                 -> m (Flavored IOFormat, Maybe String)
                                              -- ^ m (writerName, maybePdfEngineProg)
pdfWriterAndProg mWriter mEngine = do
  let panErr msg = throwError $ PandocAppError msg
  mFormat <- maybe (return Nothing) (fmap Just . writerFormat) mWriter
  case go mFormat mEngine of
      Right (writ, prog) -> return (writ, Just prog)
      Left err           -> panErr err
    where
      go Nothing Nothing        = Right (withDefaultExtensions LaTeX, "pdflatex")
      go (Just writer') Nothing = (writer',) <$> engineForWriter writer'
      go Nothing (Just engine)  = (,engine) <$> writerForEngine (takeBaseName engine)
      go (Just writer') (Just engine) =
           case find (== (unflavor writer', takeBaseName engine)) engines of
                Just _  -> Right (writer', engine)
                Nothing -> Left $ "pdf-engine " ++ engine ++
                           " is not compatible with output format " ++
                           ioFormatName (unflavor writer')

      writerForEngine eng = case [ioF | (ioF,e) <- engines, e == eng] of
                              IOFormat f : _ -> Right $
                                                      withDefaultExtensions f
                              _  -> Left $ "pdf-engine " ++ eng ++ " not known"

      engineForWriter (Flavored w _) = case [e |  (f,e) <- engines, f == w] of
                                eng : _ -> Right eng
                                []      -> Left $
                                   "cannot produce pdf output from " ++
                                   ioFormatName w

writerFormat :: PandocMonad m => String -> m (Flavored IOFormat)
writerFormat writerName =
  case parseFlavoredFormat writerName of
    Left e -> throwError . PandocAppError $
              if writerName == "pdf"
              then e ++ "\n" ++ pdfIsNoWriterErrorMsg
              else e
    Right (f, warnings) -> f <$ mapM_ report warnings

isTextFormat :: IOFormat -> Bool
isTextFormat f = case ioWriter f of
  Right TextWriter{} -> True
  _                  -> False
