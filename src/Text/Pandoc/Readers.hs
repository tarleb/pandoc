{-# LANGUAGE MonoLocalBinds      #-}
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
   Module      : Text.Pandoc.Readers
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports the readers.

Note:  all of the readers assume that the input text has @'\n'@
line endings.  So if you get your input text from a web form,
you should remove @'\r'@ characters using @filter (/='\r')@.
-}
module Text.Pandoc.Readers
  (
    -- * Readers: converting /to/ Pandoc format
    Reader (..)
  , readDocx
  , readOdt
  , readMarkdown
  , readCommonMark
  , readCreole
  , readDokuWiki
  , readMediaWiki
  , readVimwiki
  , readRST
  , readOrg
  , readLaTeX
  , readHtml
  , readJATS
  , readTextile
  , readDocBook
  , readOPML
  , readHaddock
  , readNative
  , readJSON
  , readTWiki
  , readTikiWiki
  , readTxt2Tags
  , readEPUB
  , readMuse
  , readFB2
  , readIpynb
  -- * Miscellaneous
  , knownFormatReader
  , ioReader
  ) where

import Prelude
import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Format
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.CommonMark
import Text.Pandoc.Readers.Creole
import Text.Pandoc.Readers.DocBook
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Readers.DokuWiki
import Text.Pandoc.Readers.EPUB
import Text.Pandoc.Readers.FB2
import Text.Pandoc.Readers.Ipynb
import Text.Pandoc.Readers.Haddock
import Text.Pandoc.Readers.HTML (readHtml)
import Text.Pandoc.Readers.JATS (readJATS)
import Text.Pandoc.Readers.LaTeX
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.MediaWiki
import Text.Pandoc.Readers.Muse
import Text.Pandoc.Readers.Native
import Text.Pandoc.Readers.Odt
import Text.Pandoc.Readers.OPML
import Text.Pandoc.Readers.Org
import Text.Pandoc.Readers.RST
import Text.Pandoc.Readers.Textile
import Text.Pandoc.Readers.TikiWiki
import Text.Pandoc.Readers.TWiki
import Text.Pandoc.Readers.Txt2Tags
import Text.Pandoc.Readers.Vimwiki
import Text.Pandoc.Readers.Man
import qualified Text.Pandoc.UTF8 as UTF8

data Reader m = TextReader (ReaderOptions -> Text -> m Pandoc)
              | ByteStringReader (ReaderOptions -> BL.ByteString -> m Pandoc)

-- | Association list of formats and readers.
knownFormatReader :: PandocMonad m => KnownFormat -> Maybe (Reader m)
knownFormatReader f = case f of
  CommonMark        -> Just $ TextReader readCommonMark
  Creole            -> Just $ TextReader readCreole
  DocBook4          -> Just $ TextReader readDocBook
  DocBook5          -> Just $ TextReader readDocBook
  Docx              -> Just $ ByteStringReader readDocx
  DokuWiki          -> Just $ TextReader readDokuWiki
  EPUB2             -> Just $ ByteStringReader readEPUB
  EPUB3             -> Just $ ByteStringReader readEPUB
  FB2               -> Just $ TextReader readFB2
  GFM               -> Just $ TextReader readCommonMark
  HTML4             -> Just $ TextReader readHtml
  HTML5             -> Just $ TextReader readHtml
  Haddock           -> Just $ TextReader readHaddock
  Ipynb             -> Just $ TextReader readIpynb
  JATS              -> Just $ TextReader readJATS
  JSON              -> Just $ TextReader readJSON
  LaTeX             -> Just $ TextReader readLaTeX
  Man               -> Just $ TextReader readMan
  Markdown          -> Just $ TextReader readMarkdown
  Markdown_GitHub   -> Just $ TextReader readMarkdown
  Markdown_MMD      -> Just $ TextReader readMarkdown
  Markdown_PHPExtra -> Just $ TextReader readMarkdown
  Markdown_strict   -> Just $ TextReader readMarkdown
  MediaWiki         -> Just $ TextReader readMediaWiki
  Muse              -> Just $ TextReader readMuse
  Native            -> Just $ TextReader readNative
  ODT               -> Just $ ByteStringReader readOdt
  OPML              -> Just $ TextReader readOPML
  Org               -> Just $ TextReader readOrg
  RST               -> Just $ TextReader readRST
  TWiki             -> Just $ TextReader readTWiki
  Textile           -> Just $ TextReader readTextile
  TikiWiki          -> Just $ TextReader readTikiWiki
  Txt2tags          -> Just $ TextReader readTxt2Tags
  Vimwiki           -> Just $ TextReader readVimwiki
  _                 -> Nothing

-- | Retrieve reader, extensions based on format.
ioReader :: IOFormat -> Either String (Reader PandocIO)
ioReader (CustomFormat _) = Left "Cannot use Lua to read"
ioReader (IOFormat f) = case knownFormatReader f of
  Nothing -> Left $ "Unknown reader: " ++ show f
  Just r -> Right r

-- | Read pandoc document from JSON format.
readJSON :: PandocMonad m
         => ReaderOptions -> Text -> m Pandoc
readJSON _ t =
  case eitherDecode' . BL.fromStrict . UTF8.fromText $ t of
       Right doc -> return doc
       Left _    -> throwError $ PandocParseError "JSON parse error"
