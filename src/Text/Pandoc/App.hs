{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Text.Pandoc.App (
            convertWithOpts
          , Opt(..)
          , LineEnding(..)
          , Filter(..)
          , defaultOpts
          , parseOptions
          , options
          , applyFilters
          ) where
import Prelude
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Encoding.Error as TSE
import Network.URI (URI (..), parseURI)
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitSuccess)
import System.FilePath
import System.IO (nativeNewline, stdout)
import qualified System.IO as IO (Newline (..))
import Text.Pandoc hiding (DZSlides)
import Text.Pandoc.App.Opt (Opt (..), LineEnding (..), defaultOpts)
import Text.Pandoc.App.CommandLineOptions (parseOptions, options)
import Text.Pandoc.App.OutputSettings (OutputSettings (..), optToOutputSettings)
import Text.Pandoc.BCP47 (Lang (..), parseBCP47)
import Text.Pandoc.Builder (setMeta, deleteMeta)
import Text.Pandoc.Filter (Filter (JSONFilter, LuaFilter), applyFilters)
import Text.Pandoc.Format hiding (Native)
import Text.Pandoc.PDF (makePDF)
import Text.Pandoc.Readers.Markdown (yamlToMeta)
import Text.Pandoc.SelfContained (makeDataURI, makeSelfContained)
import Text.Pandoc.Shared (eastAsianLineBreakFilter, stripEmptyParagraphs,
         headerShift, isURI, tabFilter, uriPathToPath)
import qualified Text.Pandoc.UTF8 as UTF8
#ifndef _WINDOWS
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif


convertWithOpts :: Opt -> IO ()
convertWithOpts opts = do
  let outputFile = fromMaybe "-" (optOutputFile opts)
  let filters = optFilters opts
  let verbosity = optVerbosity opts

  when (optDumpArgs opts) $
    do UTF8.hPutStrLn stdout outputFile
       mapM_ (UTF8.hPutStrLn stdout) (optInputFiles opts)
       exitSuccess

  let isPandocCiteproc (JSONFilter f) = takeBaseName f == "pandoc-citeproc"
      isPandocCiteproc _              = False
  -- --bibliography implies -F pandoc-citeproc for backwards compatibility:
  let needsCiteproc = isJust (lookup "bibliography" (optMetadata opts)) &&
                      optCiteMethod opts `notElem` [Natbib, Biblatex] &&
                      all (not . isPandocCiteproc) filters
  let filters' = if needsCiteproc then JSONFilter "pandoc-citeproc" : filters
                                  else filters

  let sources = case optInputFiles opts of
                     []  -> ["-"]
                     xs | optIgnoreArgs opts -> ["-"]
                        | otherwise  -> xs

  datadir <- case optDataDir opts of
                  Nothing   -> E.catch
                                 (Just <$> getAppUserDataDirectory "pandoc")
                                 (\e -> let _ = (e :: E.SomeException)
                                        in  return Nothing)
                  Just _    -> return $ optDataDir opts

  let pdfOutput = map toLower (takeExtension outputFile) == ".pdf"

  let runIO' :: PandocIO a -> IO a
      runIO' f = do
        (res, reports) <- runIOorExplode $ do
                             setTrace (optTrace opts)
                             setVerbosity verbosity
                             x <- f
                             rs <- getLog
                             return (x, rs)
        case optLogFile opts of
             Nothing      -> return ()
             Just logfile -> B.writeFile logfile (encodeLogMessages reports)
        let isWarning msg = messageVerbosity msg == WARNING
        when (optFailIfWarnings opts && any isWarning reports) $
            E.throwIO PandocFailOnWarningError
        return res

  let eol = case optEol opts of
                 CRLF   -> IO.CRLF
                 LF     -> IO.LF
                 Native -> nativeNewline

  runIO' $ do
    setUserDataDir datadir
    setInputFiles (optInputFiles opts)
    setOutputFile (optOutputFile opts)

    -- TODO: we have to get the input and the output into the state for
    -- the sake of the text2tags reader.
    Flavored readerFormat readerExts <-
      case optReader opts of
        Nothing         -> case flavoredFormatFromFilePaths sources of
          Just f              -> return f
          Nothing             -> return . withDefaultExtensions $
                                 if any isURI sources then HTML5 else Markdown
        Just readerName -> case parseFlavoredFormat readerName of
          Right (f, warnings) -> f <$ mapM_ report warnings
          Left e              -> throwError . PandocAppError $
                                 case readerName of
                                   "pdf" -> e ++
                                            "\nPandoc can convert to PDF, " ++
                                            "but not from PDF."
                                   "doc" -> e ++
                                            "\nPandoc can convert from " ++
                                            "DOCX, but not from DOC.\n" ++
                                            "Try using Word to save your DOC " ++
                                            "file as DOCX, and convert that " ++
                                            "with pandoc."
                                   _     -> e

    reader <- case ioReader readerFormat of
                Left e -> liftIO $ E.throwIO (PandocAppError e)
                Right x -> return x

    let convertTabs = tabFilter (if optPreserveTabs opts ||
                                    readerFormat == IOFormat Txt2tags ||
                                    readerFormat == IOFormat Man
                                 then 0
                                 else optTabStop opts)

        readSources :: [FilePath] -> PandocIO Text
        readSources srcs = convertTabs . T.intercalate (T.pack "\n") <$>
                              mapM readSource srcs

    outputSettings <- optToOutputSettings opts
    let format = outputFormat outputSettings
    let writerOptions = outputWriterOptions outputSettings

    writer <- case ioWriter format of
                Left e -> liftIO $ E.throwIO (PandocAppError e)
                Right x -> return x
    let standalone = optStandalone opts || not (producesTextOutput format) || pdfOutput

    -- We don't want to send output to the terminal if the user
    -- does 'pandoc -t docx input.txt'; though we allow them to
    -- force this with '-o -'.  On posix systems, we detect
    -- when stdout is being piped and allow output to stdout
    -- in that case, but on Windows we can't.
#ifdef _WINDOWS
    let istty = True
#else
    istty <- liftIO $ queryTerminal stdOutput
#endif
    when (not (producesTextOutput format) && istty && isNothing ( optOutputFile opts)) $
      liftIO $ E.throwIO $ PandocAppError $
              "Cannot write " ++ ioFormatName format ++ " output to terminal.\n" ++
              "Specify an output file using the -o option, or " ++
              "use '-o -' to force output to stdout."


    abbrevs <- Set.fromList . filter (not . null) . lines <$>
               case optAbbreviations opts of
                    Nothing -> UTF8.toString <$> readDataFile "abbreviations"
                    Just f  -> UTF8.toString <$> readFileStrict f

    metadata <- if format == IOFormat JATS &&
                   isNothing (lookup "csl" (optMetadata opts)) &&
                   isNothing (lookup "citation-style" (optMetadata opts))
                   then do
                     jatsCSL <- readDataFile "jats.csl"
                     let jatsEncoded = makeDataURI
                                         ("application/xml", jatsCSL)
                     return $ ("csl", jatsEncoded) : optMetadata opts
                   else return $ optMetadata opts
    metadataFromFile <-
      case optMetadataFile opts of
        Nothing   -> return mempty
        Just file -> readFileLazy file >>= yamlToMeta

    case lookup "lang" (optMetadata opts) of
           Just l  -> case parseBCP47 l of
                           Left _   -> return ()
                           Right l' -> setTranslations l'
           Nothing -> setTranslations $ Lang "en" "" "US" []

    let readerOpts = def{
            readerStandalone = standalone
          , readerColumns = optColumns opts
          , readerTabStop = optTabStop opts
          , readerIndentedCodeClasses = optIndentedCodeClasses opts
          , readerDefaultImageExtension =
             optDefaultImageExtension opts
          , readerTrackChanges = optTrackChanges opts
          , readerAbbreviations = abbrevs
          , readerExtensions = readerExts
          , readerStripComments = optStripComments opts
          }

    let transforms = (case optBaseHeaderLevel opts of
                          x | x > 1     -> (headerShift (x - 1) :)
                            | otherwise -> id) .
                     (if optStripEmptyParagraphs opts
                         then (stripEmptyParagraphs :)
                         else id) .
                     (if extensionEnabled Ext_east_asian_line_breaks
                            readerExts &&
                         not (extensionEnabled Ext_east_asian_line_breaks
                              (writerExtensions writerOptions) &&
                              writerWrapText writerOptions == WrapPreserve)
                         then (eastAsianLineBreakFilter :)
                         else id) $
                     []

    let sourceToDoc :: [FilePath] -> PandocIO Pandoc
        sourceToDoc sources' =
           case reader of
                TextReader r
                  | optFileScope opts || readerFormat == IOFormat JSON ->
                      mconcat <$> mapM (readSource >=> r readerOpts) sources
                  | otherwise ->
                      readSources sources' >>= r readerOpts
                ByteStringReader r ->
                  mconcat <$> mapM (readFile' >=> r readerOpts) sources


    setResourcePath (optResourcePath opts)
    mapM_ (uncurry setRequestHeader) (optRequestHeaders opts)

    doc <- sourceToDoc sources >>=
              (   (if isJust (optExtractMedia opts)
                      then fillMediaBag
                      else return)
              >=> return . addNonPresentMetadata metadataFromFile
              >=> return . addMetadata metadata
              >=> applyTransforms transforms
              >=> applyFilters readerOpts filters' [ioFormatName format]
              >=> maybe return extractMedia (optExtractMedia opts)
              )

    case writer of
      ByteStringWriter f -> f writerOptions doc >>= writeFnBinary outputFile
      TextWriter f -> case outputPdfProgram outputSettings of
        Just pdfProg -> do
                res <- makePDF pdfProg (optPdfEngineArgs opts) f
                        writerOptions doc
                case res of
                     Right pdf -> writeFnBinary outputFile pdf
                     Left err' -> liftIO $
                       E.throwIO $ PandocPDFError $
                                     TL.unpack (TE.decodeUtf8With TE.lenientDecode err')

        Nothing -> do
                let htmlFormat = format `elem` map IOFormat
                                 [HTML4,HTML5,S5,Slidy,Slideous,DZSlides,RevealJS]
                    addNl = if standalone
                               then id
                               else (<> T.singleton '\n')
                output <- addNl <$> f writerOptions doc
                writerFn eol outputFile =<<
                  if optSelfContained opts && htmlFormat
                     -- TODO not maximally efficient; change type
                     -- of makeSelfContained so it works w/ Text
                     then T.pack <$> makeSelfContained (T.unpack output)
                     else return output

type Transform = Pandoc -> Pandoc

addNonPresentMetadata :: Text.Pandoc.Meta -> Pandoc -> Pandoc
addNonPresentMetadata newmeta (Pandoc meta bs) = Pandoc (meta <> newmeta) bs

addMetadata :: [(String, String)] -> Pandoc -> Pandoc
addMetadata kvs pdc = foldr addMeta (removeMetaKeys kvs pdc) kvs

addMeta :: (String, String) -> Pandoc -> Pandoc
addMeta (k, v) (Pandoc meta bs) = Pandoc meta' bs
  where meta' = case lookupMeta k meta of
                      Nothing -> setMeta k v' meta
                      Just (MetaList xs) ->
                                 setMeta k (MetaList (xs ++ [v'])) meta
                      Just x  -> setMeta k (MetaList [x, v']) meta
        v' = readMetaValue v

removeMetaKeys :: [(String,String)] -> Pandoc -> Pandoc
removeMetaKeys kvs pdc = foldr (deleteMeta . fst) pdc kvs

readMetaValue :: String -> MetaValue
readMetaValue s
  | s == "true"  = MetaBool True
  | s == "True"  = MetaBool True
  | s == "TRUE"  = MetaBool True
  | s == "false" = MetaBool False
  | s == "False" = MetaBool False
  | s == "FALSE" = MetaBool False
  | otherwise    = MetaString s

-- Transformations of a Pandoc document post-parsing:

applyTransforms :: Monad m => [Transform] -> Pandoc -> m Pandoc
applyTransforms transforms d = return $ foldr ($) d transforms

readSource :: FilePath -> PandocIO Text
readSource src = case parseURI src of
                      Just u | uriScheme u `elem` ["http:","https:"] ->
                                 readURI src
                             | uriScheme u == "file:" -> liftIO $
                                 readTextFile (uriPathToPath $ uriPath u)
                      _       -> liftIO $ readTextFile src
  where readTextFile :: FilePath -> IO Text
        readTextFile fp = do
          bs <- if src == "-"
                   then BS.getContents
                   else BS.readFile fp
          E.catch (return $! UTF8.toText bs)
             (\e -> case e of
                         TSE.DecodeError _ (Just w) ->
                           case BS.elemIndex w bs of
                             Just offset -> E.throwIO $
                                  PandocUTF8DecodingError fp offset w
                             _ -> E.throwIO $ PandocUTF8DecodingError fp 0 w
                         _ -> E.throwIO $ PandocAppError (show e))

readURI :: FilePath -> PandocIO Text
readURI src = UTF8.toText . fst <$> openURL src

readFile' :: MonadIO m => FilePath -> m B.ByteString
readFile' "-" = liftIO B.getContents
readFile' f   = liftIO $ B.readFile f

writeFnBinary :: MonadIO m => FilePath -> B.ByteString -> m ()
writeFnBinary "-" = liftIO . B.putStr
writeFnBinary f   = liftIO . B.writeFile (UTF8.encodePath f)

writerFn :: MonadIO m => IO.Newline -> FilePath -> Text -> m ()
-- TODO this implementation isn't maximally efficient:
writerFn eol "-" = liftIO . UTF8.putStrWith eol . T.unpack
writerFn eol f   = liftIO . UTF8.writeFileWith eol f . T.unpack

producesTextOutput :: IOFormat -> Bool
producesTextOutput f = case ioWriter f of
  Right (TextWriter _) -> True
  _                    -> False
