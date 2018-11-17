{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports all writers functions.
-}
module Text.Pandoc.Writers
  (
    -- * Writers: converting /from/ Pandoc format
      Writer(..)
    , knownFormatWriter
    -- , writers
    , writeAsciiDoc
    , writeBeamer
    , writeCommonMark
    , writeConTeXt
    , writeCustom
    , writeDZSlides
    , writeDocbook4
    , writeDocbook5
    , writeDocx
    , writeDokuWiki
    , writeEPUB2
    , writeEPUB3
    , writeFB2
    , writeIpynb
    , writeHaddock
    , writeHtml4
    , writeHtml4String
    , writeHtml5
    , writeHtml5String
    , writeICML
    , writeJATS
    , writeJSON
    , writeLaTeX
    , writeMan
    , writeMarkdown
    , writeMediaWiki
    , writeMs
    , writeMuse
    , writeNative
    , writeODT
    , writeOPML
    , writeOpenDocument
    , writeOrg
    , writePlain
    , writePowerpoint
    , writeRST
    , writeRTF
    , writeRevealJs
    , writeS5
    , writeSlideous
    , writeSlidy
    , writeTEI
    , writeTexinfo
    , writeTextile
    , writeZimWiki
    , ioWriter
    ) where

import Prelude
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Text.Pandoc.Class
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Format
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Writers.AsciiDoc
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.ConTeXt
import Text.Pandoc.Writers.Custom
import Text.Pandoc.Writers.Docbook
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Writers.DokuWiki
import Text.Pandoc.Writers.EPUB
import Text.Pandoc.Writers.FB2
import Text.Pandoc.Writers.Ipynb
import Text.Pandoc.Writers.Haddock
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.ICML
import Text.Pandoc.Writers.JATS
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.Man
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.MediaWiki
import Text.Pandoc.Writers.Ms
import Text.Pandoc.Writers.Muse
import Text.Pandoc.Writers.Native
import Text.Pandoc.Writers.ODT
import Text.Pandoc.Writers.OpenDocument
import Text.Pandoc.Writers.OPML
import Text.Pandoc.Writers.Org
import Text.Pandoc.Writers.Powerpoint
import Text.Pandoc.Writers.RST
import Text.Pandoc.Writers.RTF
import Text.Pandoc.Writers.TEI
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Writers.Textile
import Text.Pandoc.Writers.ZimWiki

data Writer m = TextWriter (WriterOptions -> Pandoc -> m Text)
              | ByteStringWriter (WriterOptions -> Pandoc -> m BL.ByteString)

-- | Get a writer for the given format.
knownFormatWriter :: PandocMonad m => KnownFormat -> Maybe (Writer m)
knownFormatWriter f = case f of
  AsciiDoc          -> Just $ TextWriter writeAsciiDoc
  Beamer            -> Just $ TextWriter writeBeamer
  CommonMark        -> Just $ TextWriter writeCommonMark
  ConTeXt           -> Just $ TextWriter writeConTeXt
  DZSlides          -> Just $ TextWriter writeDZSlides
  DocBook4          -> Just $ TextWriter writeDocbook4
  DocBook5          -> Just $ TextWriter writeDocbook5
  Docx              -> Just $ ByteStringWriter writeDocx
  DokuWiki          -> Just $ TextWriter writeDokuWiki
  EPUB2             -> Just $ ByteStringWriter writeEPUB2
  EPUB3             -> Just $ ByteStringWriter writeEPUB3
  FB2               -> Just $ TextWriter writeFB2
  GFM               -> Just $ TextWriter writeCommonMark
  HTML4             -> Just $ TextWriter writeHtml4String
  HTML5             -> Just $ TextWriter writeHtml5String
  Haddock           -> Just $ TextWriter writeHaddock
  ICML              -> Just $ TextWriter writeICML
  Ipynb             -> Just $ TextWriter writeIpynb
  JATS              -> Just $ TextWriter writeJATS
  JSON              -> Just $ TextWriter writeJSON
  LaTeX             -> Just $ TextWriter writeLaTeX
  MS                -> Just $ TextWriter writeMs
  Man               -> Just $ TextWriter writeMan
  Markdown          -> Just $ TextWriter writeMarkdown
  Markdown_GitHub   -> Just $ TextWriter writeMarkdown
  Markdown_MMD      -> Just $ TextWriter writeMarkdown
  Markdown_PHPExtra -> Just $ TextWriter writeMarkdown
  Markdown_strict   -> Just $ TextWriter writeMarkdown
  MediaWiki         -> Just $ TextWriter writeMediaWiki
  Muse              -> Just $ TextWriter writeMuse
  Native            -> Just $ TextWriter writeNative
  ODT               -> Just $ ByteStringWriter writeODT
  OPML              -> Just $ TextWriter writeOPML
  OpenDocument      -> Just $ TextWriter writeOpenDocument
  Org               -> Just $ TextWriter writeOrg
  Plain             -> Just $ TextWriter writePlain
  Pptx              -> Just $ ByteStringWriter writePowerpoint
  RST               -> Just $ TextWriter writeRST
  RTF               -> Just $ TextWriter writeRTF
  RevealJS          -> Just $ TextWriter writeRevealJs
  S5                -> Just $ TextWriter writeS5
  Slideous          -> Just $ TextWriter writeSlideous
  Slidy             -> Just $ TextWriter writeSlidy
  TEI               -> Just $ TextWriter writeTEI
  Texinfo           -> Just $ TextWriter writeTexinfo
  Textile           -> Just $ TextWriter writeTextile
  ZimWiki           -> Just $ TextWriter writeZimWiki
  _                 -> Nothing

-- | Retrieve writer for the chosen format
ioWriter :: IOFormat -> Either String (Writer PandocIO)
ioWriter (CustomFormat fp)  = return $ TextWriter (\o d -> writeCustom fp o d)
ioWriter (IOFormat f) = case knownFormatWriter f of
  Nothing -> Left ("Unknown writer: " ++ show f)
  Just r  -> Right r

writeJSON :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJSON _ = return . UTF8.toText . BL.toStrict . encode
