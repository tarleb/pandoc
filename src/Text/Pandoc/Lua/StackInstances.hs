{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Copyright © 2012-2018 John MacFarlane <jgm@berkeley.edu>
            2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Lua.StackInstances
   Copyright   : © 2012-2018 John MacFarlane
                 © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

StackValue instances for pandoc types.
-}
module Text.Pandoc.Lua.StackInstances () where

import Prelude
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Data (showConstr, toConstr)
import Data.Text (Text, unpack)
import Foreign.Lua (Lua, Peekable, Pushable, StackIndex)
import Foreign.Lua.Types.Peekable (reportValueOnFailure)
import Foreign.Lua.Userdata ( ensureUserdataMetatable, pushAnyWithMetatable
                            , toAnyWithName, metatableName)
import Foreign.Ptr (Ptr)
import Foreign.StablePtr (StablePtr, deRefStablePtr, newStablePtr)
import Text.Pandoc.Class (CommonState (..))
import Text.Pandoc.Definition
import Text.Pandoc.Extensions (Extensions)
import Text.Pandoc.Logging (LogMessage, showLogMessage)
import Text.Pandoc.Lua.Util (defineHowTo, pushViaConstructor)
import Text.Pandoc.Options (ReaderOptions (..), TrackChanges)
import Text.Pandoc.Shared (Element (Blk, Sec))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Types.Peekable as Lua (reportValueOnFailure)
import qualified Foreign.Storable as Storable
import qualified Text.Pandoc.Lua.Util as LuaUtil

instance Pushable Pandoc where
  push (Pandoc meta blocks) =
    pushViaConstructor "Pandoc" blocks meta

instance Peekable Pandoc where
  peek idx = defineHowTo "get Pandoc value" $ do
    blocks <- LuaUtil.rawField idx "blocks"
    meta   <- LuaUtil.rawField idx "meta"
    return $ Pandoc meta blocks

instance Pushable Meta where
  push (Meta mmap) =
    pushViaConstructor "Meta" mmap
instance Peekable Meta where
  peek idx = defineHowTo "get Meta value" $
    Meta <$> Lua.peek idx

instance Pushable MetaValue where
  push = pushMetaValue
instance Peekable MetaValue where
  peek = peekMetaValue

instance Pushable Block where
  push = pushBlock

instance Peekable Block where
  peek = peekBlock

-- Inline
instance Pushable Inline where
  push = pushInline

instance Peekable Inline where
  peek = peekInline

-- Citation
instance Pushable Citation where
  push (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstructor "Citation" cid mode prefix suffix noteNum hash

instance Peekable Citation where
  peek idx = do
    id' <- LuaUtil.rawField idx "id"
    prefix <- LuaUtil.rawField idx "prefix"
    suffix <- LuaUtil.rawField idx "suffix"
    mode <- LuaUtil.rawField idx "mode"
    num <- LuaUtil.rawField idx "note_num"
    hash <- LuaUtil.rawField idx "hash"
    return $ Citation id' prefix suffix mode num hash

instance Pushable Alignment where
  push = Lua.push . show
instance Peekable Alignment where
  peek = Lua.peekRead

instance Pushable CitationMode where
  push = Lua.push . show
instance Peekable CitationMode where
  peek = Lua.peekRead

instance Pushable Format where
  push (Format f) = Lua.push f
instance Peekable Format where
  peek idx = Format <$> Lua.peek idx

instance Pushable ListNumberDelim where
  push = Lua.push . show
instance Peekable ListNumberDelim where
  peek = Lua.peekRead

instance Pushable ListNumberStyle where
  push = Lua.push . show
instance Peekable ListNumberStyle where
  peek = Lua.peekRead

instance Pushable MathType where
  push = Lua.push . show
instance Peekable MathType where
  peek = Lua.peekRead

instance Pushable QuoteType where
  push = Lua.push . show
instance Peekable QuoteType where
  peek = Lua.peekRead

-- | Push an meta value element to the top of the lua stack.
pushMetaValue :: MetaValue -> Lua ()
pushMetaValue = \case
  MetaBlocks blcks  -> pushViaConstructor "MetaBlocks" blcks
  MetaBool bool     -> Lua.push bool
  MetaInlines inlns -> pushViaConstructor "MetaInlines" inlns
  MetaList metalist -> pushViaConstructor "MetaList" metalist
  MetaMap metamap   -> pushViaConstructor "MetaMap" metamap
  MetaString str    -> Lua.push str

-- | Interpret the value at the given stack index as meta value.
peekMetaValue :: StackIndex -> Lua MetaValue
peekMetaValue idx = defineHowTo "get MetaValue" $ do
  -- Get the contents of an AST element.
  let elementContent :: Peekable a => Lua a
      elementContent = Lua.peek idx
  luatype <- Lua.ltype idx
  case luatype of
    Lua.TypeBoolean -> MetaBool <$> Lua.peek idx
    Lua.TypeString  -> MetaString <$> Lua.peek idx
    Lua.TypeTable   -> do
      tag <- Lua.try $ LuaUtil.getTag idx
      case tag of
        Right "MetaBlocks"  -> MetaBlocks  <$> elementContent
        Right "MetaBool"    -> MetaBool    <$> elementContent
        Right "MetaMap"     -> MetaMap     <$> elementContent
        Right "MetaInlines" -> MetaInlines <$> elementContent
        Right "MetaList"    -> MetaList    <$> elementContent
        Right "MetaString"  -> MetaString  <$> elementContent
        Right t             -> Lua.throwException ("Unknown meta tag: " <> t)
        Left _ -> do
          -- no meta value tag given, try to guess.
          len <- Lua.rawlen idx
          if len <= 0
            then MetaMap <$> Lua.peek idx
            else  (MetaInlines <$> Lua.peek idx)
                  <|> (MetaBlocks <$> Lua.peek idx)
                  <|> (MetaList <$> Lua.peek idx)
    _        -> Lua.throwException "could not get meta value"

-- | Push an block element to the top of the lua stack.
pushBlock :: Block -> Lua ()
pushBlock = \case
  BlockQuote blcks         -> pushViaConstructor "BlockQuote" blcks
  BulletList items         -> pushViaConstructor "BulletList" items
  CodeBlock attr code      -> pushViaConstructor "CodeBlock" code (LuaAttr attr)
  DefinitionList items     -> pushViaConstructor "DefinitionList" items
  Div attr blcks           -> pushViaConstructor "Div" blcks (LuaAttr attr)
  Header lvl attr inlns    -> pushViaConstructor "Header" lvl inlns (LuaAttr attr)
  HorizontalRule           -> pushViaConstructor "HorizontalRule"
  LineBlock blcks          -> pushViaConstructor "LineBlock" blcks
  OrderedList lstAttr list -> pushViaConstructor "OrderedList" list
                                                 (LuaListAttributes lstAttr)
  Null                     -> pushViaConstructor "Null"
  Para blcks               -> pushViaConstructor "Para" blcks
  Plain blcks              -> pushViaConstructor "Plain" blcks
  RawBlock f cs            -> pushViaConstructor "RawBlock" f cs
  Table capt aligns widths headers rows ->
    pushViaConstructor "Table" capt aligns widths headers rows

-- | Return the value at the given index as block if possible.
peekBlock :: StackIndex -> Lua Block
peekBlock idx = defineHowTo "get Block value" $ do
  tag <- LuaUtil.getTag idx
  case tag of
      "BlockQuote"     -> BlockQuote <$> elementContent
      "BulletList"     -> BulletList <$> elementContent
      "CodeBlock"      -> withAttr CodeBlock <$> elementContent
      "DefinitionList" -> DefinitionList <$> elementContent
      "Div"            -> withAttr Div <$> elementContent
      "Header"         -> (\(lvl, LuaAttr attr, lst) -> Header lvl attr lst)
                          <$> elementContent
      "HorizontalRule" -> return HorizontalRule
      "LineBlock"      -> LineBlock <$> elementContent
      "OrderedList"    -> (\(LuaListAttributes lstAttr, lst) ->
                             OrderedList lstAttr lst)
                          <$> elementContent
      "Null"           -> return Null
      "Para"           -> Para <$> elementContent
      "Plain"          -> Plain <$> elementContent
      "RawBlock"       -> uncurry RawBlock <$> elementContent
      "Table"          -> (\(capt, aligns, widths, headers, body) ->
                                  Table capt aligns widths headers body)
                          <$> elementContent
      _ -> Lua.throwException ("Unknown block type: " <> tag)
 where
   -- Get the contents of an AST element.
   elementContent :: Peekable a => Lua a
   elementContent = LuaUtil.rawField idx "c"

-- | Push an inline element to the top of the lua stack.
pushInline :: Inline -> Lua ()
pushInline = pushAnyWithMetatable $
  ensureUserdataMetatable "pandoc Inline" pushInlineMetatable

-- | Return the value at the given index as inline if possible.
peekInline :: StackIndex -> Lua Inline
peekInline = Lua.reportValueOnFailure "Inline" (\idx -> toAnyWithName idx "pandoc Inline")

------------------------------------------------------------------------

pushInlineMetatable :: Lua ()
pushInlineMetatable = do
  Lua.pushHaskellFunction inlineIndexFn
  Lua.setfield (Lua.nthFromTop 2) "__index"

  Lua.pushHaskellFunction inlineNewindex
  Lua.setfield (Lua.nthFromTop 2) "__newindex"


inlineIndexFn :: Inline
              -> Text
              -> Lua Lua.NumResults
inlineIndexFn inln accessor =
  case inln of
    Str s -> case accessor of
               "text" -> 1 <$ Lua.push s
               _ -> unknownAccessor accessor
    _ ->
      if accessor `elem` ["t", "tag"]
      then returnValue . showConstr $ toConstr inln
      else case inln of
        Code attr cs -> case accessor of
          "attr" -> returnValue attr
          "text" -> returnValue cs
          _ -> unknownAccessor accessor
        Cite citations inlns -> case accessor of
          "content" -> returnValue inlns
          "citations" -> returnValue citations
          _ -> unknownAccessor accessor
        Emph inlns -> content inlns
        Image attr inlns (src, title) -> case accessor of
          "attr" -> returnValue attr
          "caption" -> returnValue inlns
          "src" -> returnValue src
          "title" -> returnValue title
          _ -> unknownAccessor accessor
        LineBreak -> unknownAccessor accessor
        Link attr inlns (title, target) -> case accessor of
          "attr" -> returnValue attr
          "content" -> returnValue inlns
          "target" -> returnValue target
          "title" -> returnValue title
          _ -> unknownAccessor accessor
        Math mathType math -> case accessor of
          "mathtype" -> returnValue mathType
          "text" -> returnValue math
          _ -> unknownAccessor accessor
        Note inlns -> case accessor of
          "content" -> returnValue inlns
          _ -> unknownAccessor accessor
        Quoted quoteType inlns -> case accessor of
          "content" -> returnValue inlns
          "quotetype" -> returnValue quoteType
          _ -> unknownAccessor accessor
        RawInline format cs  -> case accessor of
          "format" -> returnValue format
          "text" -> returnValue cs
          _ -> unknownAccessor accessor
        SmallCaps inlns -> content inlns
        SoftBreak -> unknownAccessor accessor
        Space -> unknownAccessor accessor
        Span attr inlns -> case accessor of
          "content" -> returnValue inlns
          "attr" -> returnValue attr
          _ -> unknownAccessor accessor
        Str cs -> case accessor of
          "text" -> returnValue cs
          _ -> unknownAccessor accessor
        Strikeout inlns -> content inlns
        Strong inlns -> content inlns
        Superscript inlns -> content inlns
        Subscript inlns -> content inlns
     where
      returnValue x = 1 <$ Lua.push x

      content inlns = if accessor == "content"
        then returnValue inlns
        else unknownAccessor accessor

inlineNewindex :: Ptr (StablePtr Inline)
               -> Text
               -> AnyValue
               -> Lua Lua.NumResults
inlineNewindex ptr accessor anyval = do
  oldInline <- Lua.liftIO $ deRefStablePtr =<< Storable.peek ptr
  updatedInline <- case oldInline of
    Code attr cs -> case accessor of
      "attr" -> flip Code cs <$> attrValue
      "text" -> Code attr <$> stringValue
      _ -> unknownAccessor accessor
    Cite citations inlns -> case accessor of
      "content" -> Cite citations <$> inlineValue
      "citations" -> flip Cite inlns <$> fromAnyValue anyval
      _ -> unknownAccessor accessor
    Emph _ -> content Emph
    Image attr inlns (src, title) -> case accessor of
      "attr" -> (\x -> Link x inlns (src, title)) <$> attrValue
      "caption" -> (\x -> Link attr x (src, title)) <$> inlineValue
      "src" -> (\x -> Link attr inlns (x, title)) <$> stringValue
      "title" -> (\x -> Link attr inlns (src, x)) <$> stringValue
      _ -> unknownAccessor accessor
    LineBreak -> unknownAccessor accessor
    Link attr inlns (title, target) -> case accessor of
      "attr" -> (\x -> Link x inlns (title, target)) <$> attrValue
      "content" -> (\x -> Link attr x (title, target)) <$> inlineValue
      "target" -> (\x -> Link attr inlns (title, x)) <$> stringValue
      "title" -> (\x -> Link attr inlns (x, target)) <$> stringValue
      _ -> unknownAccessor accessor
    Math mathType math -> case accessor of
      "attr" -> flip Math math <$> fromAnyValue anyval
      "text" -> Math mathType <$> fromAnyValue anyval
      _ -> unknownAccessor accessor
    Note _ -> case accessor of
      "content" -> Note <$> fromAnyValue anyval
      _ -> unknownAccessor accessor
    Quoted quoteType inlns -> case accessor of
      "content" -> Quoted quoteType <$> inlineValue
      "quotetype" -> flip Quoted inlns <$> fromAnyValue anyval
      _ -> unknownAccessor accessor
    RawInline format cs  -> case accessor of
      "format" -> flip RawInline cs . Format <$> stringValue
      "text" -> RawInline format <$> stringValue
      _ -> unknownAccessor accessor
    SmallCaps _ -> content SmallCaps
    SoftBreak -> unknownAccessor accessor
    Space -> unknownAccessor accessor
    Span attr inlns -> case accessor of
      "content" -> Span attr <$> inlineValue
      "attr" -> flip Span inlns <$> attrValue
      _ -> unknownAccessor accessor
    Str _ -> case accessor of
      "text" -> Str <$> stringValue
      _ -> unknownAccessor accessor
    Strikeout _ -> content Strikeout
    Strong _ -> content Strong
    Superscript _ -> content Superscript
    Subscript _ -> content Subscript
  Lua.liftIO $ Storable.poke ptr =<< newStablePtr updatedInline
  return 0
 where
  attrValue = fromAnyValue anyval
  stringValue = fromAnyValue anyval
  inlineValue = fromAnyValue anyval

  content constr = if accessor == "content"
    then constr <$> inlineValue
    else unknownAccessor accessor

unknownAccessor :: Text -> Lua a
unknownAccessor accessor = Lua.throwException (unpack ("Unknown accessor: " <> accessor))

fromAnyValue :: Peekable a => AnyValue -> Lua a
fromAnyValue (AnyValue idx) = Lua.peek idx

------------------------------------------------------------------------

withAttr :: (Attr -> a -> b) -> (LuaAttr, a) -> b
withAttr f (LuaAttr attributes, x) = f attributes x

-- | Wrapper for Attr
newtype LuaAttr = LuaAttr Attr

instance Pushable LuaAttr where
  push (LuaAttr (id', classes, kv)) =
    pushViaConstructor "Attr" id' classes kv

instance Peekable LuaAttr where
  peek idx = defineHowTo "get Attr value" (LuaAttr <$> Lua.peek idx)

-- | Wrapper for ListAttributes
newtype LuaListAttributes = LuaListAttributes  ListAttributes

instance Pushable LuaListAttributes where
  push (LuaListAttributes (start, style, delimiter)) =
    pushViaConstructor "ListAttributes" start style delimiter

instance Peekable LuaListAttributes where
  peek = defineHowTo "get ListAttributes value" .
         fmap LuaListAttributes . Lua.peek

--
-- Hierarchical elements
--
instance Pushable Element where
  push (Blk blk) = Lua.push blk
  push sec = pushAnyWithMetatable pushElementMetatable sec
   where
    pushElementMetatable = ensureUserdataMetatable (metatableName sec) $
                           LuaUtil.addFunction "__index" indexElement

instance Peekable Element where
  peek idx = Lua.ltype idx >>= \case
    Lua.TypeUserdata -> Lua.peekAny idx
    _                -> Blk <$> Lua.peek idx

indexElement :: Element -> String -> Lua Lua.NumResults
indexElement = \case
  (Blk _) -> const (1 <$ Lua.pushnil) -- this shouldn't happen
  (Sec lvl num attr label contents) -> fmap (return 1) . \case
    "level"     -> Lua.push lvl
    "numbering" -> Lua.push num
    "attr"      -> Lua.push (LuaAttr attr)
    "label"     -> Lua.push label
    "contents"  -> Lua.push contents
    "tag"       -> Lua.push ("Sec" :: Text)
    "t"         -> Lua.push ("Sec" :: Text)
    _           -> Lua.pushnil


--
-- Reader Options
--
instance Pushable Extensions where
  push exts = Lua.push (show exts)

instance Pushable TrackChanges where
  push = Lua.push . showConstr . toConstr

instance Pushable ReaderOptions where
  push ro = do
    let ReaderOptions
          (extensions            :: Extensions)
          (standalone            :: Bool)
          (columns               :: Int)
          (tabStop               :: Int)
          (indentedCodeClasses   :: [String])
          (abbreviations         :: Set.Set String)
          (defaultImageExtension :: String)
          (trackChanges          :: TrackChanges)
          (stripComments         :: Bool)
          = ro
    Lua.newtable
    LuaUtil.addField "extensions" extensions
    LuaUtil.addField "standalone" standalone
    LuaUtil.addField "columns" columns
    LuaUtil.addField "tab_stop" tabStop
    LuaUtil.addField "indented_code_classes" indentedCodeClasses
    LuaUtil.addField "abbreviations" abbreviations
    LuaUtil.addField "default_image_extension" defaultImageExtension
    LuaUtil.addField "track_changes" trackChanges
    LuaUtil.addField "strip_comments" stripComments

    -- add metatable
    let indexReaderOptions :: AnyValue -> AnyValue -> Lua Lua.NumResults
        indexReaderOptions _tbl (AnyValue key) = do
          Lua.ltype key >>= \case
            Lua.TypeString -> (Lua.peek key :: Lua Text) >>= \case
              "defaultImageExtension" -> Lua.push defaultImageExtension
              "indentedCodeClasses" -> Lua.push indentedCodeClasses
              "stripComments" -> Lua.push stripComments
              "tabStop" -> Lua.push tabStop
              "trackChanges" -> Lua.push trackChanges
              _ -> Lua.pushnil
            _ -> Lua.pushnil
          return 1
    Lua.newtable
    LuaUtil.addFunction "__index" indexReaderOptions
    Lua.setmetatable (Lua.nthFromTop 2)

-- | Dummy type to allow values of arbitrary Lua type.
newtype AnyValue = AnyValue StackIndex

--
-- TODO: Much of the following should be abstracted, factored out
-- and go into HsLua.
--

instance Peekable AnyValue where
  peek = return . AnyValue

-- | Name used by Lua for the @CommonState@ type.
commonStateTypeName :: String
commonStateTypeName = "Pandoc CommonState"

instance Peekable CommonState where
  peek idx = reportValueOnFailure commonStateTypeName
             (`toAnyWithName` commonStateTypeName) idx

instance Pushable CommonState where
  push st = pushAnyWithMetatable pushCommonStateMetatable st
   where
    pushCommonStateMetatable = ensureUserdataMetatable commonStateTypeName $ do
      LuaUtil.addFunction "__index" indexCommonState
      LuaUtil.addFunction "__pairs" pairsCommonState

indexCommonState :: CommonState -> AnyValue -> Lua Lua.NumResults
indexCommonState st (AnyValue idx) = Lua.ltype idx >>= \case
  Lua.TypeString -> 1 <$ (Lua.peek idx >>= pushField)
  _ -> 1 <$ Lua.pushnil
 where
  pushField :: String -> Lua ()
  pushField name = case lookup name commonStateFields of
    Just pushValue -> pushValue st
    Nothing -> Lua.pushnil

pairsCommonState :: CommonState -> Lua Lua.NumResults
pairsCommonState st = do
  Lua.pushHaskellFunction nextFn
  Lua.pushnil
  Lua.pushnil
  return 3
 where
  nextFn :: AnyValue -> AnyValue -> Lua Lua.NumResults
  nextFn _ (AnyValue idx) =
    Lua.ltype idx >>= \case
      Lua.TypeNil -> case commonStateFields of
        []  -> 2 <$ (Lua.pushnil *> Lua.pushnil)
        (key, pushValue):_ -> 2 <$ (Lua.push key *> pushValue st)
      Lua.TypeString -> do
        key <- Lua.peek idx
        case tail $ dropWhile ((/= key) . fst) commonStateFields of
          []                     -> 2 <$ (Lua.pushnil *> Lua.pushnil)
          (nextKey, pushValue):_ -> 2 <$ (Lua.push nextKey *> pushValue st)
      _ -> 2 <$ (Lua.pushnil *> Lua.pushnil)

commonStateFields :: [(String, CommonState -> Lua ())]
commonStateFields =
  [ ("input_files", Lua.push . stInputFiles)
  , ("output_file", Lua.push . Lua.Optional . stOutputFile)
  , ("log", Lua.push . stLog)
  , ("request_headers", Lua.push . Map.fromList . stRequestHeaders)
  , ("resource_path", Lua.push . stResourcePath)
  , ("source_url", Lua.push . Lua.Optional . stSourceURL)
  , ("user_data_dir", Lua.push . Lua.Optional . stUserDataDir)
  , ("trace", Lua.push . stTrace)
  , ("verbosity", Lua.push . show . stVerbosity)
  ]

-- | Name used by Lua for the @CommonState@ type.
logMessageTypeName :: String
logMessageTypeName = "Pandoc LogMessage"

instance Peekable LogMessage where
  peek idx = reportValueOnFailure logMessageTypeName
             (`toAnyWithName` logMessageTypeName) idx

instance Pushable LogMessage where
  push msg = pushAnyWithMetatable pushLogMessageMetatable msg
   where
    pushLogMessageMetatable = ensureUserdataMetatable logMessageTypeName $
      LuaUtil.addFunction "__tostring" tostringLogMessage

tostringLogMessage :: LogMessage -> Lua String
tostringLogMessage = return . showLogMessage
