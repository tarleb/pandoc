{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Writers.Custom
   Copyright   : Copyright (C) 2012-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to custom markup using
a lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Control.Arrow ((***))
import Control.Exception
import Control.Monad (when)
import Data.List (intersperse)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Typeable
import Foreign.Lua (Lua, Pushable)
import Text.DocLayout (Doc, render)
import Text.Pandoc.Class.PandocIO (PandocIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lua (Global (..), LuaException (LuaException),
                        runLua, setGlobals)
import Text.Pandoc.Lua.Util (addField, dofileWithTraceback)
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Writers.Shared

import qualified Foreign.Lua as Lua

attrToMap :: Attr -> M.Map Text Text
attrToMap (id',classes,keyvals) = M.fromList
    $ ("id", id')
    : ("class", T.unwords classes)
    : keyvals

-- | Checks if the global variable @PANDOC_USE_DOCLAYOUT@ is set
-- and non-nil. Only then should Doc types be pushed, Lua strings
-- otherwise.
useDocLayout :: Lua Bool
useDocLayout = do
  Lua.pushglobaltable
  Lua.pushstring "PANDOC_USE_DOCLAYOUT"
  Lua.rawget (Lua.nthFromTop 2)
  result <- Lua.toboolean Lua.stackTop
  Lua.pop 2
  return result

pushDoc :: Doc Text -> Lua ()
pushDoc d = do
  useDoc  <- useDocLayout
  if useDoc
    then Lua.push (d :: Doc Text)
    else Lua.push (render Nothing d :: Text)

newtype Stringify a = Stringify a

instance Pushable (Stringify Format) where
  push (Stringify (Format f)) = Lua.push (T.toLower f)

instance Pushable (Stringify [Inline]) where
  push (Stringify ils) = pushDoc =<< inlineListToCustom ils

instance Pushable (Stringify [Block]) where
  push (Stringify blks) = pushDoc =<< blockListToCustom blks

instance Pushable (Stringify MetaValue) where
  push (Stringify (MetaMap m))       = Lua.push (fmap Stringify m)
  push (Stringify (MetaList xs))     = Lua.push (map Stringify xs)
  push (Stringify (MetaBool x))      = Lua.push x
  push (Stringify (MetaString s))    = Lua.push s
  push (Stringify (MetaInlines ils)) = Lua.push (Stringify ils)
  push (Stringify (MetaBlocks bs))   = Lua.push (Stringify bs)

instance Pushable (Stringify Citation) where
  push (Stringify cit) = do
    Lua.createtable 6 0
    addField "citationId" $ citationId cit
    addField "citationPrefix" . Stringify $ citationPrefix cit
    addField "citationSuffix" . Stringify $ citationSuffix cit
    addField "citationMode" $ show (citationMode cit)
    addField "citationNoteNum" $ citationNoteNum cit
    addField "citationHash" $ citationHash cit

-- | Key-value pair, pushed as a table with @a@ as the only key and @v@ as the
-- associated value.
newtype KeyValue a b = KeyValue (a, b)

instance (Pushable a, Pushable b) => Pushable (KeyValue a b) where
  push (KeyValue (k, v)) = do
    Lua.newtable
    Lua.push k
    Lua.push v
    Lua.rawset (Lua.nthFromTop 3)

data PandocLuaException = PandocLuaException Text
    deriving (Show, Typeable)

instance Exception PandocLuaException

-- | Convert Pandoc to custom markup.
writeCustom :: FilePath -> WriterOptions -> Pandoc -> PandocIO Text
writeCustom luaFile opts doc@(Pandoc meta _) = do
  let globals = [ PANDOC_DOCUMENT doc
                , PANDOC_SCRIPT_FILE luaFile
                ]
  res <- runLua $ do
    setGlobals globals
    stat <- dofileWithTraceback luaFile
    -- check for error in lua script (later we'll change the return type
    -- to handle this more gracefully):
    when (stat /= Lua.OK) $
      Lua.tostring' (-1) >>= throw . PandocLuaException . UTF8.toText
    body    <- docToCustom opts doc
    context <- metaToContext opts
                             blockListToCustom
                             inlineListToCustom
                             meta
    return (body, context)
  let (body, context) = case res of
        Left (LuaException msg) -> throw (PandocLuaException msg)
        Right x -> x
  return $
    let colwidth = if writerWrapText opts == WrapAuto
                   then Just $ writerColumns opts
                   else Nothing
    in render colwidth $ case writerTemplate opts  of
                           Nothing  -> body
                           Just tpl -> renderTemplate tpl $
                                       setField "body" body context

docToCustom :: WriterOptions -> Pandoc -> Lua (Doc Text)
docToCustom opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom blocks
  useDoc <- useDocLayout
  let metamap' = fmap Stringify metamap
  let variables = writerVariables opts
  if useDoc
    then Lua.callFunc "Doc"  body                 metamap' variables
    else Lua.callFunc "Doc" (render Nothing body) metamap' variables

-- | Convert Pandoc block element to Custom.
blockToCustom :: Block         -- ^ Block element
              -> Lua (Doc Text)

blockToCustom Null = return mempty

blockToCustom (Plain inlines) = Lua.callFunc "Plain" (Stringify inlines)

blockToCustom (Para [Image attr txt (src,tit)]) =
  Lua.callFunc "CaptionedImage" src tit (Stringify txt) (attrToMap attr)

blockToCustom (Para inlines) = Lua.callFunc "Para" (Stringify inlines)

blockToCustom (LineBlock linesList) =
  Lua.callFunc "LineBlock" (map Stringify linesList)

blockToCustom (RawBlock format str) =
  Lua.callFunc "RawBlock" (Stringify format) str

blockToCustom HorizontalRule = Lua.callFunc "HorizontalRule"

blockToCustom (Header level attr inlines) =
  Lua.callFunc "Header" level (Stringify inlines) (attrToMap attr)

blockToCustom (CodeBlock attr str) =
  Lua.callFunc "CodeBlock" str (attrToMap attr)

blockToCustom (BlockQuote blocks) =
  Lua.callFunc "BlockQuote" (Stringify blocks)

blockToCustom (Table capt aligns widths headers rows) =
  let aligns' = map show aligns
      capt' = Stringify capt
      headers' = map Stringify headers
      rows' = map (map Stringify) rows
  in Lua.callFunc "Table" capt' aligns' widths headers' rows'

blockToCustom (BulletList items) =
  Lua.callFunc "BulletList" (map Stringify items)

blockToCustom (OrderedList (num,sty,delim) items) =
  Lua.callFunc "OrderedList" (map Stringify items) num (show sty) (show delim)

blockToCustom (DefinitionList items) =
  Lua.callFunc "DefinitionList"
               (map (KeyValue . (Stringify *** map Stringify)) items)

blockToCustom (Div attr items) =
  Lua.callFunc "Div" (Stringify items) (attrToMap attr)

-- | Convert list of Pandoc block elements to Custom.
blockListToCustom :: [Block]       -- ^ List of block elements
                  -> Lua (Doc Text)
blockListToCustom xs = do
  blocksep <- Lua.callFunc "Blocksep"
  bs <- mapM blockToCustom xs
  return $ mconcat $ intersperse blocksep bs

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: [Inline] -> Lua (Doc Text)
inlineListToCustom lst = do
  xs <- mapM inlineToCustom lst
  return $ mconcat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: Inline -> Lua (Doc Text)

inlineToCustom (Str str) = Lua.callFunc "Str" str

inlineToCustom Space = Lua.callFunc "Space"

inlineToCustom SoftBreak = Lua.callFunc "SoftBreak"

inlineToCustom (Emph lst) = Lua.callFunc "Emph" (Stringify lst)

inlineToCustom (Strong lst) = Lua.callFunc "Strong" (Stringify lst)

inlineToCustom (Strikeout lst) = Lua.callFunc "Strikeout" (Stringify lst)

inlineToCustom (Superscript lst) = Lua.callFunc "Superscript" (Stringify lst)

inlineToCustom (Subscript lst) = Lua.callFunc "Subscript" (Stringify lst)

inlineToCustom (SmallCaps lst) = Lua.callFunc "SmallCaps" (Stringify lst)

inlineToCustom (Quoted SingleQuote lst) = Lua.callFunc "SingleQuoted" (Stringify lst)

inlineToCustom (Quoted DoubleQuote lst) = Lua.callFunc "DoubleQuoted" (Stringify lst)

inlineToCustom (Cite cs lst) = Lua.callFunc "Cite" (Stringify lst) (map Stringify cs)

inlineToCustom (Code attr str) =
  Lua.callFunc "Code" str (attrToMap attr)

inlineToCustom (Math DisplayMath str) =
  Lua.callFunc "DisplayMath" str

inlineToCustom (Math InlineMath str) =
  Lua.callFunc "InlineMath" str

inlineToCustom (RawInline format str) =
  Lua.callFunc "RawInline" (Stringify format) str

inlineToCustom LineBreak = Lua.callFunc "LineBreak"

inlineToCustom (Link attr txt (src,tit)) =
  Lua.callFunc "Link" (Stringify txt) src tit (attrToMap attr)

inlineToCustom (Image attr alt (src,tit)) =
  Lua.callFunc "Image" (Stringify alt) src tit (attrToMap attr)

inlineToCustom (Note contents) = Lua.callFunc "Note" (Stringify contents)

inlineToCustom (Span attr items) =
  Lua.callFunc "Span" (Stringify items) (attrToMap attr)
