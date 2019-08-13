module P6Output.JsonV1 (encode) where

import Pre hiding (list)

import qualified Data.HashMap.Lazy as M

import Data.Aeson.Encoding (pair, pairs, encodingToLazyByteString, Encoding, Series, int, text, list, null_)

import P1Lexing.Types (Range(..), Position(..), range, Token(..))
import P2LanguageDefinition.Types (SDName(..), Name(..), TypeName(..))
import P4Parsing.Types (Node(..), NodeInternals(..))

{-
# top-value:
{ "version": 1
, "files": { filename : node } }

# node:
{ "type": "node"
, "name": string
, "contents": map[string](internal | node | token)
, "range": range }

# range:
{ "file": string
, "start": pos
, "end": pos }
| null

# pos:
{ "line": int
, "col": int }

# internal:
{ "type": "internal"
, "contents": map[string](internal | node | token) }

# token:
{ "type": "lit"
, "val": string
, "range": range }
|
{ "type": "tok"
, "tokt": string
, "val": string
, "range": range }

-}

encode :: HashMap Text (Node l TypeName) -> LByteString
encode files = pairs (pair "version" (int 1) <> foldMap encodeFile (M.toList files))
  & encodingToLazyByteString

encodeFile :: (Text, Node l TypeName) -> Series
encodeFile (path, node) = pair path $ encodeNode node

encodeNode :: Node l TypeName -> Encoding
encodeNode Node{n_name, n_contents, n_range} = pair "type" (text "node")
  <> pair "name" (text $ coerce n_name)
  <> pair "range" (encodeRange n_range)
  <> pair "contents" (encodeMap n_contents)
  & pairs

encodeRange :: Range -> Encoding
encodeRange (Range path start end) = pair "file" (text path)
  <> pair "start" (encodePos start)
  <> pair "end" (encodePos end)
  & pairs
  where
    encodePos (Position line col) = pair "line" (int line) <> pair "col" (int col) & pairs
encodeRange Nowhere = null_

encodeMap :: HashMap SDName (Seq (NodeInternals l TypeName (Node l TypeName))) -> Encoding
encodeMap = M.toList
  >>> foldMap (coerce *** foldable encodeInternal >>> uncurry pair)
  >>> mappend (pair "type" (text "internal"))
  >>> pairs

encodeInternal :: NodeInternals l TypeName (Node l TypeName) -> Encoding
encodeInternal (NodeLeaf node) = encodeNode node
encodeInternal (TokenLeaf tok) = pair "range" (encodeRange $ range tok)
  <> case tok of
       LitTok _ _ t -> pair "type" (text "lit") <> pair "val" (text t)
       OtherTok _ _ tyn t -> pair "type" (text "tok") <> pair "val" (text t) <> pair "tokt" (text $ coerce tyn)
  & pairs
encodeInternal (Struct m) = pair "type" (text "internal") <> pair "contents" (encodeMap m)
  & pairs

foldable :: Foldable f => (a -> Encoding) -> f a -> Encoding
foldable f = toList >>> list f
