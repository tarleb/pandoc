{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
module Text.Pandoc.Format.Base
  ( Format (..)
  ) where

import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Formats that pandoc knows how to parse.
data TextFormat
  = Asciidoc AsciidocVariant
  | BibTeX BibTeXVariant
  | CSV
  | Creole
  | CslJson
  | DocBook DocBookVersion
  | DokuWiki
  | EndNoteXML
  | FB2
  | HTML HTMLVersion
  | Haddock
  | ICML
  | Ipynb
  | JATS JATSTagSet
  | JSON
  | Jira
  | Markdown MDVariant
  | MediaWiki
  | Muse
  | Native
  | OpenDocument
  | OOXML
  | OPML
  | Org
  | RIS
  | ReStructuredText
  | Roff RoffMacroPackage
  | TeX TeXType
  | Texinfo
  | TSV
  | TWiki
  | Textile
  | TikiWiki
  | T2T       -- txt2tags
  | Vimwiki
  | XWiki
  | ZimWiki
  deriving (Eq, Data, Generic, Ord, Read, Show, Typeable)

data AsciidocVariant = AsciidocClassic | AsciiDoctor
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data BibTeXVariant = PlainBibTeX | BibLaTeX
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data HTMLVersion = HTML4 | HTML5
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data EPUBVersion = EPUB2 | EPUB3
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data DocBookVersion = DocBook4 | DocBook5
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data TeXType = LaTeX | ConTeXt
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data RoffMacroPackage = Ms | Man
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

-- | JATS tag set variant.
data JATSTagSet
  = TagSetArchiving         -- ^ Archiving and Interchange Tag Set
  | TagSetPublishing        -- ^ Journal Publishing Tag Set
  | TagSetArticleAuthoring  -- ^ Article Authoring Tag Set
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

-- | Named Markdown variants.
data MDVariant =
    CommonMark
  | CommonMark_X
  | GFM
  | MD_GitHub
  | MD_MMD
  | MD_PHPExtra
  | MD_strict
  | MD_default
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

-- | Varieties of HTML slide shows.
data HTMLSlideVariant =
    S5
  | Slidy
  | Slideous
  | DZSlides
  | RevealJS
  deriving (Bounded, Enum, Eq, Data, Generic, Ord, Read, Show, Typeable)

data ContainerFormat
  = DOCX
  | PPTX
  | ODT
  | EPUB EPUBVersion
  deriving (Eq, Data, Generic, Ord, Read, Show, Typeable)

data SlideFormat = HTMLSlides HTMLSlideVariant | Beamer
  deriving (Eq, Data, Generic, Ord, Read, Show, Typeable)

data Format
  = PlainTextMarkup TextFormat
  | Container ContainerFormat
  | Slides SlideFormat
  | PDF
  deriving (Eq, Data, Generic, Ord, Read, Show, Typeable)

baseFormat :: Format -> Maybe TextFormat
baseFormat = \case
  PlainTextMarkup ptm -> Just ptm
  Container cf -> Just $ case cf of
                           DOCX -> OOXML
                           PPTX -> OOXML
                           ODT  -> OpenDocument
                           EPUB EPUB2 -> HTML HTML4
                           EPUB EPUB3 -> HTML HTML5
  Slides Beamer -> Just $ TeX LaTeX
  Slides (HTMLSlides slideVariant) -> Just . HTML $ case slideVariant of
                                                      DZSlides -> HTML5
                                                      RevealJS -> HTML5
                                                      S5       -> HTML4
                                                      Slideous -> HTML4
                                                      Slidy    -> HTML4
  PDF -> Nothing
