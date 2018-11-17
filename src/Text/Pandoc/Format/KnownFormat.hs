{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
#ifdef DERIVE_JSON_VIA_TH
{-# LANGUAGE TemplateHaskell    #-}
#endif
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
module Text.Pandoc.Format.KnownFormat
  ( KnownFormat (..)
  , allKnownFormats
  , name
  , namedFormat
  , formatFromName
  ) where

import Prelude
import Data.Char (toLower)
import Data.Data (Data)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Formats of which pandoc is aware.
data KnownFormat =
    AsciiDoc
  | Beamer
  | CommonMark
  | ConTeXt
  | Creole
  | DZSlides
  | DocBook4
  | DocBook5
  | Docx
  | DokuWiki
  | EPUB2
  | EPUB3
  | FB2
  | GFM
  | HTML4
  | HTML5
  | Haddock
  | ICML
  | Ipynb
  | JATS
  | JSON
  | LaTeX
  | Man
  | Markdown
  | Markdown_GitHub
  | Markdown_MMD
  | Markdown_PHPExtra
  | Markdown_strict
  | MediaWiki
  | MS
  | Muse
  | Native
  | ODT
  | OOXML
  | OPML
  | OpenDocument
  | Org
  | Plain
  | Pptx
  | RST
  | RTF
  | RevealJS
  | Roff
  | S5
  | Slideous
  | Slidy
  | TEI
  | TWiki
  | Texinfo
  | Textile
  | TikiWiki
  | Txt2tags
  | Vimwiki
  | ZimWiki
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

name :: KnownFormat -> String
name Txt2tags = "t2t"
name f = map toLower (show f)

-- | List of all formats of which pandoc is aware.
allKnownFormats :: Set KnownFormat
allKnownFormats = Set.fromAscList [minBound .. maxBound]

namedFormat :: Map String KnownFormat
namedFormat = Map.fromList $
  [ ("docbook", DocBook5)
  , ("epub", EPUB3)
  , ("html", HTML5)
  , ("t2t", Txt2tags)
  ] ++
  map (\f -> (map toLower (show f), f)) (Set.toList allKnownFormats)

-- | Get a format from a string identifier.
formatFromName :: String -> Maybe KnownFormat
formatFromName = flip Map.lookup namedFormat
