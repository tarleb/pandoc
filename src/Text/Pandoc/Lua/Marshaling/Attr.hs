{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{- |
Module      : Text.Pandoc.Lua.Marshaling.Attr
Copyright   : © 2012-2021 John MacFarlane
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above

Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Marshaling/unmarshaling instances for document AST elements.
-}
module Text.Pandoc.Lua.Marshaling.Attr
  ( peekAttr
  , pushAttr
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HsLua
import HsLua.Marshalling.Peekers (peekIndexRaw)
import HsLua.Packaging.Operation (Operation (Index, Newindex))
import HsLua.Packaging.UDType
import Safe (atMay)
import Text.Pandoc.Definition (Attr)

import qualified Data.Text as T

typeAttr :: LuaError e => UDType e Attr
typeAttr = deftype "Attr" []
  [ property "identifier" "element identifier"
      (pushText, \(ident,_,_) -> ident)
      (peekText, \(_,cls,kv) -> (,cls,kv))
  , property "classes" "element classes"
      (pushList pushText, \(_,classes,_) -> classes)
      (peekList peekText, \(ident,_,kv) -> (ident,,kv))
  , property "attributes" "various element attributes"
      (pushAttribs, \(_,_,attribs) -> attribs)
      (peekAttribs, \(ident,cls,_) -> (ident,cls,))
  ]

pushAttr :: LuaError e => Pusher e Attr
pushAttr = pushUD typeAttr

peekAttribs :: LuaError e => Peeker e [(Text,Text)]
peekAttribs idx =
  peekList (peekPair peekText peekText) idx <|>
  peekUD typeAttributeList idx

pushAttribs :: LuaError e => Pusher e [(Text, Text)]
pushAttribs = pushUD typeAttributeList

typeAttributeList :: LuaError e => UDType e [(Text, Text)]
typeAttributeList = deftype "AttributeList"
  [ operation Index $ defun ""
    ### liftPure2 lookupKey
    <#> udparam typeAttributeList "t" "attributes list"
    <#> parameter peekKey "string|integer" "key" "lookup key"
    =#> functionResult (maybe pushnil pushText) "string" "attribute value"

  , operation Newindex $ defun ""
    ### setKey
    <#> udparam typeAttributeList "t" "attributes list"
    <#> parameter peekKey "string|integer" "key" "lookup key"
    <#> optionalParameter peekText "string|nil" "value" "new value"
    =#> []
  ]
  []

data Key = StringKey Text | IntKey Int

peekKey :: LuaError e => Peeker e (Maybe Key)
peekKey idx = liftLua (ltype idx) >>= \case
  TypeNumber -> Just . IntKey <$!> peekIntegral idx
  TypeString -> Just . StringKey <$!> peekText idx
  _          -> return Nothing

lookupKey :: [(Text,Text)] -> Maybe Key -> Maybe Text
lookupKey !kvs = \case
  Just (StringKey str) -> lookup str kvs
  Just (IntKey n)      -> snd <$!> atMay kvs n
  Nothing              -> Nothing

setKey :: forall e. LuaError e
       => [(Text, Text)] -> Maybe Key -> Maybe Text
       -> LuaE e ()
setKey kvs mbKey mbValue = case mbKey of
  Just (StringKey str) -> case break ((== str) . fst) kvs of
                            (prefix, _:suffix) -> setNew $ case mbValue of
                              Nothing -> prefix ++ suffix
                              Just value -> prefix ++ (str, value):suffix
                            _  -> case mbValue of
                              Nothing -> return ()
                              Just value -> setNew (kvs ++ [(str, value)])
  _  -> failLua "invalid attribute key"
  where
    setNew :: [(Text, Text)] -> LuaE e ()
    setNew new =
      putuserdata (nthBottom 1) (udName @e typeAttributeList) new >>= \case
        True -> return ()
        False -> failLua "failed to modify attributes list"

peekAttr :: LuaError e => Peeker e Attr
peekAttr idx = retrieving "Attr" $ liftLua (ltype idx) >>= \case
  TypeString -> (,[],[]) <$!> peekText idx -- treat string as ID
  TypeUserdata -> peekUD typeAttr idx
  TypeTable -> do
    len' <- liftLua $ rawlen idx
    let peekClasses = peekList peekText
    if len' > 0
      then do
        ident <- peekIndexRaw 1 peekText idx
        classes <- fromMaybe [] <$!> optional (peekIndexRaw 2 peekClasses idx)
        attribs <- fromMaybe [] <$!> optional (peekIndexRaw 3 peekAttribs idx)
        return $ ident `seq` classes `seq` attribs `seq`
          (ident, classes, attribs)
      else retrieving "HTML-like attributes" $ do
        kvs <- peekKeyValuePairs peekText peekText idx
        let ident = fromMaybe "" $ lookup "id" kvs
        let classes = maybe [] T.words $ lookup "class" kvs
        let attribs = filter ((`notElem` ["id", "class"]) . fst) kvs
        return $ ident `seq` classes `seq` attribs `seq`
          (ident, classes, attribs)
  x -> liftLua . failLua $ "Cannot get Attr from " ++ show x
