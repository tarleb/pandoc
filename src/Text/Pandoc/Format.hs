{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>
              2018 Albert Krewinkel <albert+pandoc@zeitkraut.de>

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
module Text.Pandoc.Format
  ( module Text.Pandoc.Format.Extensions
  , module Text.Pandoc.Format.KnownFormat
  , Flavored (..)
  , unflavor
  , parseFlavoredFormat
  , flavoredFormatFromFilePath
  , flavoredFormatFromFilePaths
  , IOFormat (..)
  , unIOFormat
  , withDefaultExtensions
  , getDefaultExtensions
  , ioFormatName
  )
where

import Prelude
import Control.Monad (mplus)
import Data.Char (toLower)
import Data.List (intercalate, isSuffixOf)
import System.FilePath (takeExtension, takeFileName)
import Text.Pandoc.Logging (LogMessage (Deprecated))
import Text.Pandoc.Format.Extensions
import Text.Pandoc.Format.KnownFormat
import Text.Parsec.Error (Message (Message), errorMessages)

-- | Full description of a format
data Flavored f = Flavored f Extensions
  deriving (Show, Read, Eq)

unflavor :: Flavored f -> f
unflavor (Flavored f _) = f

data IOFormat = IOFormat KnownFormat
              | CustomFormat FilePath
  deriving (Show, Read, Eq)

unIOFormat :: IOFormat -> Maybe KnownFormat
unIOFormat (IOFormat f) = Just f
unIOFormat _ = Nothing

ioFormatName :: IOFormat -> String
ioFormatName (CustomFormat fp) = map toLower (takeFileName fp)
ioFormatName (IOFormat f) = name f

parseFlavoredFormat :: String -> Either String (Flavored IOFormat, [LogMessage])
parseFlavoredFormat s =
  if ".lua" `isSuffixOf` s
  then return (Flavored (CustomFormat s) mempty, [])
  else case parseFormatSpec s of
    Left e  -> Left $ intercalate "\n" [m | Message m <- errorMessages e]
    Right (formatName, setExts) -> do
        format <- maybe (Left ("Unknown format: " ++ formatName))
                        Right
                        (formatFromName formatName)
        let exts = getDefaultExtensions format
        let msgs = [ Deprecated "markdown_github" "Use gfm instead."
                   | formatName == "markdown_github" ]
        return (Flavored (IOFormat format) (setExts exts), msgs)

withDefaultExtensions :: KnownFormat -> Flavored IOFormat
withDefaultExtensions f = Flavored (IOFormat f) (getDefaultExtensions f)

-- | Default extensions from format.
getDefaultExtensions :: KnownFormat -> Extensions
getDefaultExtensions = \case
  Beamer            -> getDefaultExtensions LaTeX
  CommonMark        -> extensionsFromList
                       [ Ext_raw_html
                       ]
  ConTeXt           -> extensionsFromList
                       [ Ext_auto_identifiers
                       , Ext_smart
                       ]
  EPUB2             -> getDefaultExtensions EPUB3
  EPUB3             -> extensionsFromList
                       [ Ext_epub_html_exts
                       , Ext_native_divs
                       , Ext_native_spans
                       , Ext_raw_html
                       ]
  Ipynb             -> extensionsFromList
                       [ Ext_all_symbols_escapable
                       , Ext_pipe_tables
                       , Ext_raw_html
                       , Ext_fenced_code_blocks
                       , Ext_auto_identifiers
                       , Ext_gfm_auto_identifiers
                       , Ext_backtick_code_blocks
                       , Ext_autolink_bare_uris
                       , Ext_space_in_atx_header
                       , Ext_intraword_underscores
                       , Ext_strikeout
                       , Ext_task_lists
                       , Ext_lists_without_preceding_blankline
                       , Ext_shortcut_reference_links
                       , Ext_tex_math_dollars
                       ]
  GFM               -> githubMarkdownExtensions
  HTML4             -> getDefaultExtensions HTML5
  HTML5             -> extensionsFromList
                       [ Ext_auto_identifiers
                       , Ext_line_blocks
                       , Ext_native_divs
                       , Ext_native_spans
                       ]
  LaTeX             -> extensionsFromList
                       [ Ext_auto_identifiers
                       , Ext_latex_macros
                       , Ext_smart
                       ]
  Markdown          -> pandocExtensions
  Markdown_GitHub   -> githubMarkdownExtensions
  Markdown_MMD      -> multimarkdownExtensions
  Markdown_PHPExtra -> phpMarkdownExtraExtensions
  Markdown_strict   -> strictExtensions
  Muse              -> extensionsFromList
                       [ Ext_amuse
                       , Ext_auto_identifiers
                       ]
  OPML              -> pandocExtensions -- affects notes
  Org               -> extensionsFromList
                       [ Ext_auto_identifiers
                       , Ext_citations
                       ]
  Plain             -> plainExtensions
  Textile           -> extensionsFromList
                       [ Ext_auto_identifiers
                       , Ext_old_dashes
                       , Ext_raw_html
                       , Ext_smart
                       ]
  _                 -> extensionsFromList
                       [ Ext_auto_identifiers
                       ]

-- | Determine format based on file extensions
flavoredFormatFromFilePaths :: [FilePath] -> Maybe (Flavored IOFormat)
flavoredFormatFromFilePaths =
  foldr (mplus . flavoredFormatFromFilePath) Nothing

-- | Determine format based on file extension
flavoredFormatFromFilePath :: FilePath -> Maybe (Flavored IOFormat)
flavoredFormatFromFilePath fp =
  let
    defaultExts f = return $
      Flavored (IOFormat f) (getDefaultExtensions f)
    modifiedExts f extsMod = return $
      Flavored (IOFormat f) (extsMod $ getDefaultExtensions f)
  in case takeExtension (map toLower fp) of
      ".adoc"     -> defaultExts AsciiDoc
      ".asciidoc" -> defaultExts AsciiDoc
      ".context"  -> defaultExts ConTeXt
      ".ctx"      -> defaultExts ConTeXt
      ".db"       -> defaultExts DocBook5
      ".docx"     -> defaultExts Docx
      ".dokuwiki" -> defaultExts DokuWiki
      ".epub"     -> defaultExts EPUB3
      ".fb2"      -> defaultExts FB2
      ".htm"      -> defaultExts HTML5
      ".html"     -> defaultExts HTML5
      ".icml"     -> defaultExts ICML
      ".json"     -> defaultExts JSON
      ".latex"    -> defaultExts LaTeX
      ".lhs"      -> modifiedExts Markdown (enableExtension Ext_literate_haskell)
      ".ltx"      -> defaultExts LaTeX
      ".markdown" -> defaultExts Markdown
      ".md"       -> defaultExts Markdown
      ".ms"       -> defaultExts Roff
      ".muse"     -> defaultExts Muse
      ".native"   -> defaultExts Native
      ".odt"      -> defaultExts OpenDocument
      ".opml"     -> defaultExts OPML
      ".org"      -> defaultExts Org
      ".pptx"     -> defaultExts Pptx
      ".roff"     -> defaultExts Roff
      ".rst"      -> defaultExts RST
      ".rtf"      -> defaultExts RTF
      ".s5"       -> defaultExts S5
      ".t2t"      -> defaultExts Txt2tags
      ".tei"      -> defaultExts TEI
      ".tei.xml"  -> defaultExts TEI
      ".tex"      -> defaultExts LaTeX
      ".texi"     -> defaultExts Texinfo
      ".texinfo"  -> defaultExts Texinfo
      ".text"     -> defaultExts Markdown
      ".textile"  -> defaultExts Textile
      ".txt"      -> defaultExts Markdown
      ".wiki"     -> defaultExts MediaWiki
      ".xhtml"    -> defaultExts HTML5
      ['.',y]     | y `elem` ['1'..'9'] -> defaultExts Man
      _           -> Nothing
