--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

{-# LANGUAGE OverloadedStrings #-}

module Thrift.Protocol.PrettyJSON
    ( module Thrift.Protocol
    , PrettyJSONProtocol(..)
    ) where

import Data.ByteString.Lazy.Builder as B
import Data.Int
import Data.List
import Data.Monoid
import Data.Text.Lazy.Encoding
import qualified Data.HashMap.Strict as Map
import qualified Data.Text.Lazy as LT

import Thrift.Protocol
import Thrift.Protocol.JSONUtils
import Thrift.Protocol.SimpleJSON
import Thrift.Types

-- | The Pretty JSON Protocol data uses the standard 'TSimpleJSONProtocol'
-- for parsing and serializes as pretty printed JSON.
-- Data is encoded as a JSON 'ByteString'
data PrettyJSONProtocol t = PrettyJSONProtocol Int t
                      -- ^ Construct a 'JSONProtocol' with an indent width
                      -- and a 'Transport'

instance Protocol PrettyJSONProtocol where
    getTransport (PrettyJSONProtocol _ t) = t
    writeMessage (PrettyJSONProtocol _ t)  = writeMessage (SimpleJSONProtocol t)
    readMessage (PrettyJSONProtocol _ t) = readMessage (SimpleJSONProtocol t)
    serializeVal (PrettyJSONProtocol indent _) =
      toLazyByteString . buildJSONValue indent 0
    deserializeVal (PrettyJSONProtocol _ t) =
      deserializeVal (SimpleJSONProtocol t)
    readVal (PrettyJSONProtocol _ t) = readVal (SimpleJSONProtocol t)

-- Writing Functions

indented :: Int -> Builder
indented i = mconcat $ "\n" : replicate i " "

buildJSONValue :: Int -> Int -> ThriftVal -> Builder
buildJSONValue i l (TStruct fields) = "{" <>
  indented (l + i) <> buildJSONStruct i (l + i) fields <>
  indented l <> "}"
buildJSONValue i l (TMap _ _ entries) = "{" <>
  indented (l + i) <> buildJSONMap i (l + i) entries <>
  indented l <> "}"
buildJSONValue i l (TList _ entries) = "[" <>
  indented (l + i) <> buildJSONList i (l + i) entries <>
  indented l <> "]"
buildJSONValue i l (TSet _ entries) = "[" <>
  indented (l + i) <> buildJSONList i (l + i) entries <>
  indented l <> "]"
buildJSONValue _ _ (TBool b) = if b then "true" else "false"
buildJSONValue _ _ (TByte b) = int8Dec b
buildJSONValue _ _ (TI16 i) = int16Dec i
buildJSONValue _ _ (TI32 i) = int32Dec i
buildJSONValue _ _ (TI64 i) = int64Dec i
buildJSONValue _ _ (TDouble d) = doubleDec d
buildJSONValue _ _ (TString s) = "\"" <> escape s <> "\""

buildJSONStruct
  :: Int -> Int -> Map.HashMap Int16 (LT.Text, ThriftVal) -> Builder
buildJSONStruct i l = mconcat . intersperse ("," <> indented l) .  Map.elems .
  Map.map (\(str,val) ->
    "\"" <> B.lazyByteString (encodeUtf8 str) <> "\": " <>
    buildJSONValue i l val)

buildJSONMap :: Int -> Int -> [(ThriftVal, ThriftVal)] -> Builder
buildJSONMap i l = mconcat . intersperse ("," <> indented l) . map buildKV
  where
    buildKV (key@(TString _), val) =
      buildJSONValue i l key <> ": " <> buildJSONValue i l val
    buildKV (key, val) =
      "\"" <> buildJSONValue i l key <> "\": " <> buildJSONValue i l val
buildJSONList :: Int -> Int -> [ThriftVal] -> Builder
buildJSONList i l =
  mconcat . intersperse ("," <> indented l) . map (buildJSONValue i l)
