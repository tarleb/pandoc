{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.WriterOptions
   Copyright   : © 2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for ReaderOptions and its components.
-}
module Text.Pandoc.Lua.Marshaling.WriterOptions
  ( pushWriterOptions
  ) where

import Data.Char (toLower)
import Data.Data (showConstr, toConstr)
import Data.Text (Text)
import Foreign.Lua (Lua, Optional (..), Pushable)
import Skylighting (Syntax)
import Text.DocTemplates (Template)
import Text.Pandoc.Lua.Marshaling.CommonState ()
import Text.Pandoc.Lua.Marshaling.Context ()
import Text.Pandoc.Lua.Marshaling.ReaderOptions ()
import Text.Pandoc.Highlighting (Style)
import Text.Pandoc.Options

import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

--
-- Writer Options
--
instance Pushable (Template Text) where
  push = Lua.pushAny

instance Pushable Syntax where
  push = Lua.pushAny

instance Pushable ObfuscationMethod where
  push = (Lua.push :: Text -> Lua ()) . \case
    NoObfuscation         -> "none"
    ReferenceObfuscation  -> "references"
    JavascriptObfuscation -> "javascript"

instance Pushable CiteMethod where
  push = Lua.push . map toLower . showConstr . toConstr

instance Pushable Style where
  push = Lua.pushAny

instance Pushable HTMLMathMethod where
  push = Lua.push . show

instance Pushable ReferenceLocation where
  push = Lua.push . show

instance Pushable TopLevelDivision where
  push = Lua.push . show

instance Pushable WrapOption where
  push = Lua.push . map toLower . drop 4 . showConstr . toConstr

instance Pushable WriterOptions where
  push = pushWriterOptions

pushWriterOptions :: WriterOptions -> Lua ()
pushWriterOptions opts = do
  Lua.newtable
  let addField' name f = LuaUtil.addField name (f opts)
  addField' "cite_method"        writerCiteMethod
  addField' "columns"            writerColumns
  addField' "dpi"                writerDpi
  addField' "email_obfuscation"  writerEmailObfuscation
  addField' "epub_chapter_level" writerEpubChapterLevel
  addField' "epub_fonts"         writerEpubFonts
  addField' "epub_metadata"      (Optional . writerEpubMetadata)
  addField' "epub_subdirectory"  writerEpubSubdirectory
  addField' "extensions"         writerExtensions
  addField' "highlight_style"    (Optional . writerHighlightStyle)
  addField' "html_math_method"   writerHTMLMathMethod
  addField' "html_q_tags"        writerHtmlQTags
  addField' "identifier_prefix"  writerIdentifierPrefix
  addField' "incremental"        writerIncremental
  addField' "listings"           writerListings
  addField' "number_offset"      writerNumberOffset
  addField' "number_section"     writerNumberSections
  addField' "prefer_ascii"       writerPreferAscii
  addField' "reference_doc"      (Optional . writerReferenceDoc)
  addField' "reference_links"    writerReferenceLinks
  addField' "reference_location" writerReferenceLocation
  addField' "section_divs"       writerSectionDivs
  addField' "setext_headers"     writerSetextHeaders
  addField' "slide_level"        (Optional . writerSlideLevel)
  addField' "syntax_map"         writerSyntaxMap
  addField' "tab_stop"           writerTabStop
  addField' "table_of_contents"  writerTableOfContents
  addField' "template"           (Optional . writerTemplate)
  addField' "toc_depth"          writerTOCDepth
  addField' "top_level_division" writerTopLevelDivision
  addField' "variables"          writerVariables
  addField' "wraptext"           writerWrapText
