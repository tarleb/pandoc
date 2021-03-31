{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.AST
   Copyright   : © 2012-2021 John MacFarlane
                 © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling/unmarshaling instances for document AST elements.
-}
module Text.Pandoc.Lua.Marshaling.AST
  ( peekAttr
  , peekBlock
  , peekCitation
  , peekInline
  , peekListAttributes
  , peekMeta
  , peekMetaValue
  , peekPandoc

  , pushAttr
  , pushBlock
  , pushInline
  , pushListAttributes
  , pushMetaValue
  , pushPandoc
  ) where

import Control.Applicative ((<|>))
import Control.Monad ((<$!>), (>=>))
import HsLua
import HsLua.Marshalling.Peek (toPeeker)
import HsLua.Class.Peekable (PeekError)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Util ( defineHowTo, pushViaConstr', pushViaConstructor)
import Text.Pandoc.Lua.Marshaling.CommonState ()

import qualified HsLua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

instance Pushable Pandoc where
  push = pushPandoc

pushPandoc :: LuaError e => Pusher e Pandoc
pushPandoc (Pandoc meta blocks) =
  pushViaConstr' "Pandoc" [pushList pushBlock blocks, push meta]

peekPandoc :: PeekError e => Peeker e Pandoc
peekPandoc idx = retrieving "Pandoc value" $! do
  meta <- peekFieldRaw peekMeta "meta" idx
  blks <- peekFieldRaw (toPeeker peek) "blocks" idx
  return $ Pandoc <$!> meta <*>  blks

instance Peekable Pandoc where
  peek = peekPandoc >=> force

instance Pushable Meta where
  push (Meta mmap) =
    pushViaConstr' "Meta" [push mmap]
instance Peekable Meta where
  peek = peekMeta >=> force

peekMeta :: PeekError e => Peeker e Meta
peekMeta idx = retrieving "Meta" $
  fmap Meta <$!> toPeeker Lua.peek idx

instance Pushable MetaValue where
  push = pushMetaValue

instance Peekable MetaValue where
  peek = peekMetaValue >=> force

instance Pushable Block where
  push = pushBlock

instance Peekable Block where
  peek = peekBlock >=> force

-- Inline
instance Pushable Inline where
  push = pushInline

instance Peekable Inline where
  peek = peekInline >=> force

-- Citation
instance Pushable Citation where
  push (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstr' "Citation"
    [ push cid, push mode, push prefix, push suffix, push noteNum, push hash
    ]

instance Peekable Citation where
  peek = peekCitation >=> force

peekCitation :: PeekError e => Peeker e Citation
-- peekCitation = toPeeker peek
peekCitation idx = retrieving "Citation" $ do
  idx' <- absindex idx
  runLuaPeek $ Citation
    <$!> LuaPeek (peekFieldRaw peekText "id" idx')
    <*>  LuaPeek (peekFieldRaw (peekList peekInline) "prefix" idx')
    <*>  LuaPeek (peekFieldRaw (peekList peekInline) "suffix" idx')
    <*>  LuaPeek (peekFieldRaw peekRead "mode" idx')
    <*>  LuaPeek (peekFieldRaw peekIntegral "note_num" idx')
    <*>  LuaPeek (peekFieldRaw peekIntegral "hash" idx')


instance Pushable Alignment where
  push = Lua.pushString . show
instance Peekable Alignment where
  peek = Lua.peekRead >=> force

instance Pushable CitationMode where
  push = Lua.push . show
instance Peekable CitationMode where
  peek = Lua.peekRead >=> force

instance Pushable Format where
  push (Format f) = Lua.push f
instance Peekable Format where
  peek = peekFormat >=> force

peekFormat :: LuaError e => Peeker e Format
peekFormat idx = fmap Format <$!> peekText idx

instance Pushable ListNumberDelim where
  push = Lua.push . show

instance Pushable ListNumberStyle where
  push = Lua.push . show
instance Peekable ListNumberStyle where
  peek = Lua.peekRead >=> force

instance Pushable MathType where
  push = Lua.push . show
instance Peekable MathType where
  peek = Lua.peekRead >=> force

instance Pushable QuoteType where
  push = Lua.push . show
instance Peekable QuoteType where
  peek = Lua.peekRead >=> force

-- | Push an meta value element to the top of the lua stack.
pushMetaValue :: LuaError e => MetaValue -> LuaE e ()
pushMetaValue = \case
  MetaBlocks blcks  -> pushViaConstr' "MetaBlocks" [pushList pushBlock blcks]
  MetaBool bool     -> Lua.push bool
  MetaInlines inlns -> pushViaConstr' "MetaInlines"
                       [pushList pushInline inlns]
  MetaList metalist -> pushViaConstr' "MetaList"
                       [pushList pushMetaValue metalist]
  MetaMap metamap   -> pushViaConstr' "MetaMap"
                       [pushMap pushText pushMetaValue metamap]
  MetaString str    -> Lua.push str

-- | Interpret the value at the given stack index as meta value.
peekMetaValue :: forall e. PeekError e => Peeker e MetaValue
peekMetaValue = toPeeker $ \idx -> defineHowTo "get MetaValue" $ do
  -- Get the contents of an AST element.
  let elementContent :: Peekable a => LuaE e a
      elementContent = Lua.peek idx
  luatype <- Lua.ltype idx
  case luatype of
    Lua.TypeBoolean -> MetaBool <$!> Lua.peek idx
    Lua.TypeString  -> MetaString <$!> Lua.peek idx
    Lua.TypeTable   -> do
      tag <- try $ LuaUtil.getTag idx
      case tag of
        Right "MetaBlocks"  -> MetaBlocks  <$!> elementContent
        Right "MetaBool"    -> MetaBool    <$!> elementContent
        Right "MetaMap"     -> MetaMap     <$!> elementContent
        Right "MetaInlines" -> MetaInlines <$!> elementContent
        Right "MetaList"    -> MetaList    <$!> elementContent
        Right "MetaString"  -> MetaString  <$!> elementContent
        Right t             -> Lua.failLua ("Unknown meta tag: " <> t)
        Left _ -> do
          -- no meta value tag given, try to guess.
          len <- Lua.rawlen idx
          if len <= 0
            then MetaMap <$!> Lua.peek idx
            else  (MetaInlines <$!> Lua.peek idx)
                  <|> (MetaBlocks <$!> Lua.peek idx)
                  <|> (MetaList <$!> Lua.peek idx)
    _        -> Lua.failLua "could not get meta value"

-- | Push a block element to the top of the Lua stack.
pushBlock :: forall e. LuaError e => Block -> LuaE e ()
pushBlock = \case
  BlockQuote blcks         -> pushViaConstructor @e "BlockQuote" blcks
  BulletList items         -> pushViaConstructor @e "BulletList" items
  CodeBlock attr code      -> pushViaConstr' @e "CodeBlock"
                              [ push code, pushAttr attr ]
  DefinitionList items     -> pushViaConstructor @e "DefinitionList" items
  Div attr blcks           -> pushViaConstr' @e "Div"
                              [push blcks, pushAttr attr]
  Header lvl attr inlns    -> pushViaConstr' @e "Header"
                              [push lvl, push inlns, pushAttr attr]
  HorizontalRule           -> pushViaConstructor @e "HorizontalRule"
  LineBlock blcks          -> pushViaConstructor @e "LineBlock" blcks
  OrderedList lstAttr list -> pushViaConstr' @e "OrderedList"
                              [ push list, pushListAttributes @e lstAttr ]
  Null                     -> pushViaConstructor @e "Null"
  Para blcks               -> pushViaConstructor @e "Para" blcks
  Plain blcks              -> pushViaConstructor @e "Plain" blcks
  RawBlock f cs            -> pushViaConstructor @e "RawBlock" f cs
  Table attr blkCapt specs thead tbody tfoot ->
    pushViaConstr' @e "Table"
    [ pushCaption blkCapt, push specs, push thead, push tbody
    , push tfoot, pushAttr attr]

-- | Return the value at the given index as block if possible.
peekBlock :: forall e. PeekError e => Peeker e Block
peekBlock = retrieving "Block" . \idx -> do
  -- Get the contents of an AST element.
  let mkBlock :: (a -> Block) -> Peeker e a -> LuaE e (Result Block)
      mkBlock f p = fmap f <$!> peekFieldRaw p "c" idx
  tag <- LuaUtil.getTag idx
  case tag of
      "BlockQuote"     -> mkBlock BlockQuote peekBlocks
      "BulletList"     -> mkBlock BulletList (peekList peekBlocks)
      "CodeBlock"      -> mkBlock (uncurry CodeBlock)
                                  (peekPair peekAttr peekText)
      "DefinitionList" -> mkBlock DefinitionList
                          (peekList (peekPair peekInlines (peekList peekBlocks)))
      "Div"            -> mkBlock (uncurry Div) (peekPair peekAttr peekBlocks)
      "Header"         -> mkBlock (\(lvl, attr, lst) -> Header lvl attr lst)
                          (peekTriple peekIntegral peekAttr peekInlines)
      "HorizontalRule" -> return (pure HorizontalRule)
      "LineBlock"      -> mkBlock LineBlock (peekList peekInlines)
      "OrderedList"    -> mkBlock (uncurry OrderedList)
                          (peekPair peekListAttributes (peekList peekBlocks))
      "Null"           -> return (pure Null)
      "Para"           -> mkBlock Para peekInlines
      "Plain"          -> mkBlock Plain peekInlines
      "RawBlock"       -> mkBlock (uncurry RawBlock)
                                  (peekPair peekFormat peekText)
      "Table"          -> mkBlock id
                          (retrieving "Table" . (absindex >=> (\idx' -> do
                              attr  <- rawgeti idx' 1 *> peekAttr top
                              capt  <- rawgeti idx' 2 *> peekCaption top
                              cs    <- rawgeti idx' 3 *> (toPeeker peek) top
                              thead <- rawgeti idx' 4 *> peekTableHead top
                              tbods <- rawgeti idx' 5 *> peekList peekTableBody top
                              tfoot <- rawgeti idx' 6 *> (toPeeker peek) top
                              pop 6
                              return $! Table
                                <$!> attr <*> capt  <*> cs
                                <*> thead <*> tbods <*> tfoot)))
      _ -> Lua.failLua ("Unknown block type: " <> tag)

peekBlocks :: PeekError e => Peeker e [Block]
peekBlocks = peekList peekBlock

peekInlines :: PeekError e => Peeker e [Inline]
peekInlines = peekList peekInline

-- | Push Caption element
pushCaption :: LuaError e => Caption -> LuaE e ()
pushCaption (Caption shortCaption longCaption) = do
  Lua.newtable
  LuaUtil.addField "short" (Lua.Optional shortCaption)
  LuaUtil.addField "long" longCaption

-- | Peek Caption element
peekCaption :: PeekError e => Peeker e Caption
peekCaption = retrieving "Caption" . \idx -> do
  short <- peekFieldRaw (optional peekInlines) "short" idx
  long <- peekFieldRaw peekBlocks "long" idx
  return $! Caption <$!> short <*> long

instance Peekable ColWidth where
  peek = peekColWidth >=> force

peekColWidth :: LuaError e => Peeker e ColWidth
peekColWidth = retrieving "ColWidth" . \idx -> do
  width <- optional peekRealFloat idx
  return $! maybe ColWidthDefault ColWidth <$!> width

instance Pushable ColWidth where
  push = \case
    (ColWidth w)    -> Lua.push w
    ColWidthDefault -> Lua.pushnil

instance Pushable Row where
  push (Row attr cells) = Lua.push (attr, cells)

instance Peekable Row where
  peek = peekRow >=> force

peekRow :: PeekError e => Peeker e Row
peekRow = fmap ((uncurry Row) <$!>)
  . retrieving "Row"
  . peekPair peekAttr (peekList peekCell)

instance Pushable TableBody where
  push (TableBody attr (RowHeadColumns rowHeadColumns) head' body) = do
    Lua.newtable
    LuaUtil.addField "attr" attr
    LuaUtil.addField "row_head_columns" rowHeadColumns
    LuaUtil.addField "head" head'
    LuaUtil.addField "body" body

peekTableBody :: PeekError e => Peeker e TableBody
peekTableBody = retrieving "TableBody" . \idx -> do
  attr  <- peekFieldRaw peekAttr "attr" idx
  rwhds <- peekFieldRaw (fmap (fmap RowHeadColumns) . peekIntegral) "row_head_columns" idx
  head' <- peekFieldRaw (peekList peekRow) "head" idx
  body  <- peekFieldRaw (peekList peekRow) "body" idx
  return $! (TableBody <$!> attr <*> rwhds <*> head' <*> body)

instance Pushable TableHead where
  push (TableHead attr rows) = Lua.push (attr, rows)

peekTableHead :: PeekError e => Peeker e TableHead
peekTableHead = (((uncurry TableHead) <$!>) <$!>)
  . retrieving "TableHead"
  . peekPair peekAttr (peekList peekRow)

instance Pushable TableFoot where
  push (TableFoot attr cells) = Lua.push (attr, cells)

instance Peekable TableFoot where
  peek = fmap (uncurry TableFoot) . Lua.peek

instance Pushable Cell where
  push = pushCell

instance Peekable Cell where
  peek = peekCell >=> force

pushCell :: LuaError e => Cell -> LuaE e ()
pushCell (Cell attr align (RowSpan rowSpan) (ColSpan colSpan) contents) = do
  Lua.newtable
  LuaUtil.addField "attr" attr
  LuaUtil.addField "alignment" align
  LuaUtil.addField "row_span" rowSpan
  LuaUtil.addField "col_span" colSpan
  LuaUtil.addField "contents" contents

peekCell :: PeekError e => Peeker e Cell
peekCell = retrieving "Cell" . \idx -> do
  attr <- peekFieldRaw peekAttr "attr" idx
  algn <- peekFieldRaw peekRead "alignment" idx
  rs   <- (RowSpan <$!>) <$!> peekFieldRaw peekIntegral "row_span" idx
  cs   <- (ColSpan <$!>) <$!> peekFieldRaw peekIntegral "col_span" idx
  blks <- peekFieldRaw peekBlocks "contents" idx
  return $! Cell <$!> attr <*> algn <*> rs <*> cs <*> blks

-- | Push an inline element to the top of the lua stack.
pushInline :: forall e. LuaError e => Inline -> LuaE e ()
pushInline = \case
  Cite citations lst       -> pushViaConstructor @e "Cite" lst citations
  Code attr lst            -> pushViaConstr' @e "Code"
                              [push lst, pushAttr attr]
  Emph inlns               -> pushViaConstructor @e "Emph" inlns
  Underline inlns          -> pushViaConstructor @e "Underline" inlns
  Image attr alt (src,tit) -> pushViaConstr' @e "Image"
                              [push alt, push src, push tit, pushAttr attr]
  LineBreak                -> pushViaConstructor @e "LineBreak"
  Link attr lst (src,tit)  -> pushViaConstr' @e "Link"
                              [push lst, push src, push tit, pushAttr attr]
  Note blcks               -> pushViaConstructor @e "Note" blcks
  Math mty str             -> pushViaConstructor @e "Math" mty str
  Quoted qt inlns          -> pushViaConstructor @e "Quoted" qt inlns
  RawInline f cs           -> pushViaConstructor @e "RawInline" f cs
  SmallCaps inlns          -> pushViaConstructor @e "SmallCaps" inlns
  SoftBreak                -> pushViaConstructor @e "SoftBreak"
  Space                    -> pushViaConstructor @e "Space"
  Span attr inlns          -> pushViaConstr' @e "Span"
                              [push inlns, pushAttr attr]
  Str str                  -> pushViaConstructor @e "Str" str
  Strikeout inlns          -> pushViaConstructor @e "Strikeout" inlns
  Strong inlns             -> pushViaConstructor @e "Strong" inlns
  Subscript inlns          -> pushViaConstructor @e "Subscript" inlns
  Superscript inlns        -> pushViaConstructor @e "Superscript" inlns

-- | Return the value at the given index as inline if possible.
peekInline :: forall e. PeekError e => Peeker e Inline
peekInline = retrieving "Inline" . \idx -> do
  -- Get the contents of an AST element.
  let mkBlock :: (a -> Inline) -> Peeker e a -> LuaE e (Result Inline)
      mkBlock f p = fmap f <$!> peekFieldRaw p "c" idx
  tag <- LuaUtil.getTag idx
  case tag of
    "Cite"       -> mkBlock (uncurry Cite) (peekPair (toPeeker peek) peekInlines)
    "Code"       -> mkBlock (uncurry Code) (peekPair peekAttr peekText)
    "Emph"       -> mkBlock Emph peekInlines
    "Underline"  -> mkBlock Underline peekInlines
    "Image"      -> mkBlock (\(attr, lst, tgt) -> Image attr lst tgt)
                    (peekTriple peekAttr peekInlines (toPeeker peek))
    "Link"       -> mkBlock (\(attr, lst, tgt) -> Link attr lst tgt)
                    (peekTriple peekAttr peekInlines (toPeeker peek))
    "LineBreak"  -> return (pure LineBreak)
    "Note"       -> mkBlock Note peekBlocks
    "Math"       -> mkBlock (uncurry Math) (peekPair peekRead peekText)
    "Quoted"     -> mkBlock (uncurry Quoted) (peekPair peekRead peekInlines)
    "RawInline"  -> mkBlock (uncurry RawInline) (peekPair peekFormat peekText)
    "SmallCaps"  -> mkBlock SmallCaps peekInlines
    "SoftBreak"  -> return (pure SoftBreak)
    "Space"      -> return (pure Space)
    "Span"       -> mkBlock (uncurry Span) (peekPair peekAttr peekInlines)
    "Str"        -> mkBlock Str peekText
    "Strikeout"  -> mkBlock Strikeout peekInlines
    "Strong"     -> mkBlock Strong peekInlines
    "Subscript"  -> mkBlock Subscript peekInlines
    "Superscript"-> mkBlock Superscript peekInlines
    _ -> Lua.failLua ("Unknown inline type: " <> tag)

pushAttr :: forall e. LuaError e => Attr -> LuaE e ()
pushAttr (id', classes, kv) = pushViaConstr' @e "Attr"
  [ pushText id'
  , pushList pushText classes
  , pushList (pushPair pushText pushText) kv
  ]

instance Peekable LuaAttr where
  peek = fmap LuaAttr . (peekAttr >=> force)

peekAttr :: LuaError e => Peeker e Attr
peekAttr = retrieving "Attr" . peekTriple
  peekText
  (peekList peekText)
  (peekList (peekPair peekText peekText))

pushListAttributes :: forall e. LuaError e => ListAttributes -> LuaE e ()
pushListAttributes (start, style, delimiter) =
    pushViaConstr' "ListAttributes"
    [ push start, push style, push delimiter ]

peekListAttributes :: LuaError e => Peeker e ListAttributes
peekListAttributes = retrieving "ListAttributes" . peekTriple
  peekIntegral
  peekRead
  peekRead
