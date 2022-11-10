{- |
   Module      : Text.Pandoc.Metadata.BaseInfo
   Copyright   : Copyright (C) 2020-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Article metadata information.
-}
module Text.Pandoc.Metadata (BaseInfo (..)) where
import Text.Pandoc.Definition (Block, Inline)
import Data.Text (Text)

data BaseInfo = BaseInfo
  { abstract :: [Block]
  , authors :: [AuthorMetadata]
  , date :: Date
  , description :: Text
  , institute :: [[Inline]] -- ^ produced by reader, not used in writer
  , keywords :: [Text]
  , subtitle :: [Inline]
  , title :: [Inline]

  , article :: Maybe ArticleInfo
  , journal :: Maybe JournalInfo
  , copyright :: Maybe CopyrightInfo
  }

data JournalInfo = JournalInfo
  { journalTitle :: [Inline]
  , abbrevTitle :: Maybe Text
  , ppubIssn :: Maybe Text
  , epubIssn :: Maybe Text
  , publisherName :: Text
  , publisherLocation :: Text
  }

data AuthorMetadata
  = AuthorMetadataName Text
  | AuthorMetadataInfo AuthorInfo

data AuthorInfo = AuthorInfo
  { surname :: Text
  , givenNames :: Text
  , email :: Maybe Text
  , orcid :: Maybe Text
  , corId :: Int -- ^ corresponding author ID
  , affId :: Int -- ^ author affiliation ID
  }

data ArticleInfo = ArticleInfo
  { publisherId :: Maybe Text
  , doi :: Maybe Text
  , pmid :: Maybe Text
  , pmcid :: Maybe Text
  , artAccessId :: Maybe Text -- ^ what's this?
  , heading :: [Inline]
  , categories :: [[Inline]]
  , authorNotes :: Maybe AuthorNotes
  , volume :: Maybe Text
  , issue :: Maybe Text
  , fpage :: Maybe Text -- ^ first page
  , lpage :: Maybe Text -- ^ last page
  }

data AuthorNotes = AuthorNotes
  { corrsp :: [CorrespondenceInfo] -- ^ ID and email
  , conflict :: [Block]  -- ^ conflicts of interest declarations
  , con :: [Block]       -- ^ contributed-by information
  }

data CorrespondenceInfo = CorrespondenceInfo
  { id :: Int
  , corrEmail :: Text  -- ^ named just @email@ in template
  }

data CopyrightInfo = CopyrightInfo
  { statement :: Maybe [Inline]
  , copyrightYear :: Maybe Text  -- ^ named just @year@ in template
  , holder :: Maybe Text
  }

data Date = Date
  { iso8601 :: Text
  , day     :: Text
  , month   :: Text
  , year    :: Text
  }
