{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2009-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Templates
   Copyright   : Copyright (C) 2009-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A simple templating system with variable substitution and conditionals.

-}

module Text.Pandoc.Templates ( module Text.DocTemplates
                             , renderTemplate'
                             , getDefaultTemplate
                             ) where

import Prelude
import Control.Monad.Except (throwError)
import Data.Aeson (ToJSON (..))
import qualified Data.Text as T
import System.FilePath ((<.>), (</>))
import Text.DocTemplates (Template, TemplateTarget, applyTemplate,
                          compileTemplate, renderTemplate, varListToJSON)
import Text.Pandoc.Class (PandocMonad, readDataFile)
import Text.Pandoc.Error
import Text.Pandoc.Format
import qualified Text.Pandoc.UTF8 as UTF8

-- | Get default template for the specified writer.
getDefaultTemplate :: PandocMonad m
                   => KnownFormat           -- ^ Name of writer
                   -> m String
getDefaultTemplate format =
  case name format of
       "native"  -> return ""
       "json"    -> return ""
       "docx"    -> return ""
       "fb2"     -> return ""
       "pptx"    -> return ""
       "ipynb"   -> return ""
       "odt"     -> getDefaultTemplate OpenDocument
       "html"    -> getDefaultTemplate HTML5
       "docbook" -> getDefaultTemplate DocBook5
       "epub"    -> getDefaultTemplate EPUB3
       "beamer"  -> getDefaultTemplate LaTeX
       "markdown_strict"   -> getDefaultTemplate Markdown
       "multimarkdown"     -> getDefaultTemplate Markdown
       "markdown_github"   -> getDefaultTemplate Markdown
       "markdown_mmd"      -> getDefaultTemplate Markdown
       "markdown_phpextra" -> getDefaultTemplate Markdown
       "gfm"               -> getDefaultTemplate CommonMark
       _        -> let fname = "templates" </> "default" <.> name format
                   in  UTF8.toString <$> readDataFile fname

-- | Like 'applyTemplate', but runs in PandocMonad and
-- raises an error if compilation fails.
renderTemplate' :: (PandocMonad m, ToJSON a, TemplateTarget b)
                => String -> a -> m b
renderTemplate' template context =
  case applyTemplate (T.pack template) context of
       Left e  -> throwError (PandocTemplateError e)
       Right r -> return r
