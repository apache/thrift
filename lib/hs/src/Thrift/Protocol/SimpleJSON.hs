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

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Thrift.Protocol.SimpleJSON
    ( module Thrift.Protocol
    , SimpleJSONProtocol(..)
    ) where

import Control.Applicative
import Control.Exception
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as PC
import Data.Attoparsec.ByteString.Lazy as LP
import Data.ByteString.Lazy.Builder as B
import Data.Functor
import Data.Int
import Data.List
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text.Lazy.Encoding
import qualified Data.HashMap.Strict as Map

import Thrift.Protocol
import Thrift.Protocol.JSONUtils
import Thrift.Transport
import Thrift.Types

import qualified Data.Text.Lazy as LT

-- | The Simple JSON Protocol data uses the standard 'TSimpleJSONProtocol'.
-- Data is encoded as a JSON 'ByteString'
data SimpleJSONProtocol t = SimpleJSONProtocol t
                      -- ^ Construct a 'JSONProtocol' with a 'Transport'

version :: Int32
version = 1

instance Protocol SimpleJSONProtocol where
    getTransport (SimpleJSONProtocol t) = t

    writeMessage (SimpleJSONProtocol t) (s, ty, sq) =
      bracket writeMessageBegin writeMessageEnd . const
      where
        writeMessageBegin = tWrite t $ toLazyByteString $
          "[" <> int32Dec version <>
          ",\"" <> escape (encodeUtf8 s) <> "\"" <>
          "," <> intDec (fromEnum ty) <>
          "," <> int32Dec sq <>
          ","
        writeMessageEnd _ = tWrite t "]"
    readMessage p = bracket readMessageBegin readMessageEnd
      where
        readMessageBegin = runParser p $ skipSpace *> do
          _ver :: Int32 <- lexeme (PC.char8 '[') *> lexeme (signed decimal)
          bs <- lexeme (PC.char8 ',') *> lexeme escapedString
          case decodeUtf8' bs of
            Left _ -> fail "readMessage: invalid text encoding"
            Right str -> do
              ty <- toEnum <$> (lexeme (PC.char8 ',') *>
                                lexeme (signed decimal))
              seqNum <- lexeme (PC.char8 ',') *> signed decimal
              return (str, ty, seqNum)
        readMessageEnd _ = runParser p $ skipSpace *> PC.char8 ']'

    serializeVal _ = toLazyByteString . buildJSONValue
    deserializeVal _ ty bs =
      case LP.eitherResult $ LP.parse (parseJSONValue ty) bs of
        Left s -> error s
        Right val -> val

    readVal p ty = runParser p $ skipSpace *> parseJSONValue ty


-- Writing Functions

buildJSONValue :: ThriftVal -> Builder
buildJSONValue (TStruct fields) = "{" <> buildJSONStruct fields <> "}"
buildJSONValue (TMap _ _ entries) = "{" <> buildJSONMap entries <> "}"
buildJSONValue (TList _ entries) = "[" <> buildJSONList entries <> "]"
buildJSONValue (TSet _ entries) = "[" <> buildJSONList entries <> "]"
buildJSONValue (TBool b) = if b then "true" else "false"
buildJSONValue (TByte b) = int8Dec b
buildJSONValue (TI16 i) = int16Dec i
buildJSONValue (TI32 i) = int32Dec i
buildJSONValue (TI64 i) = int64Dec i
buildJSONValue (TDouble d) = doubleDec d
buildJSONValue (TString s) = "\"" <> escape s <> "\""

buildJSONStruct :: Map.HashMap Int16 (LT.Text, ThriftVal) -> Builder
buildJSONStruct = mconcat . intersperse "," . Map.elems . Map.map (\(str,val) ->
  "\"" <> B.lazyByteString (encodeUtf8 str) <> "\":" <> buildJSONValue val)

buildJSONMap :: [(ThriftVal, ThriftVal)] -> Builder
buildJSONMap = mconcat . intersperse "," . map buildKV
  where
    buildKV (key@(TString _), val) =
      buildJSONValue key <> ":" <> buildJSONValue val
    buildKV (key, val) =
      "\"" <> buildJSONValue key <> "\":" <> buildJSONValue val
buildJSONList :: [ThriftVal] -> Builder
buildJSONList = mconcat . intersperse "," . map buildJSONValue


-- Reading Functions

parseJSONValue :: ThriftType -> Parser ThriftVal
parseJSONValue (T_STRUCT tmap) =
  TStruct <$> (lexeme (PC.char8 '{') *> parseJSONStruct tmap <* PC.char8 '}')
parseJSONValue (T_MAP kt vt) =
  TMap kt vt <$> between '{' '}' (parseJSONMap kt vt)
parseJSONValue (T_LIST ty) =
  TList ty <$> between '[' ']' (parseJSONList ty)
parseJSONValue (T_SET ty) =
  TSet ty <$> between '[' ']' (parseJSONList ty)
parseJSONValue T_BOOL =
  (TBool True <$ string "true") <|> (TBool False <$ string "false")
parseJSONValue T_BYTE = TByte <$> signed decimal
parseJSONValue T_I16 = TI16 <$> signed decimal
parseJSONValue T_I32 = TI32 <$> signed decimal
parseJSONValue T_I64 = TI64 <$> signed decimal
parseJSONValue T_DOUBLE = TDouble <$> double
parseJSONValue T_STRING = TString <$> escapedString
parseJSONValue T_STOP = fail "parseJSONValue: cannot parse type T_STOP"
parseJSONValue T_VOID = fail "parseJSONValue: cannot parse type T_VOID"

parseAnyValue :: Parser ()
parseAnyValue = choice $
                skipBetween '{' '}' :
                skipBetween '[' ']' :
                map (void . parseJSONValue)
                  [ T_BOOL
                  , T_I16
                  , T_I32
                  , T_I64
                  , T_DOUBLE
                  , T_STRING
                  ]
  where
    skipBetween :: Char -> Char -> Parser ()
    skipBetween a b = between a b $ void (PC.satisfy (\c -> c /= a && c /= b))
                                          <|> skipBetween a b

parseJSONStruct :: TypeMap -> Parser (Map.HashMap Int16 (LT.Text, ThriftVal))
parseJSONStruct tmap = Map.fromList . catMaybes <$> parseField
                       `sepBy` lexeme (PC.char8 ',')
  where
    parseField = do
      bs <- lexeme escapedString <* lexeme (PC.char8 ':')
      case decodeUtf8' bs of
        Left _ -> fail "parseJSONStruct: invalid key encoding"
        Right str -> case Map.lookup str tmap of
          Just (fid, ftype) -> do
            val <- lexeme (parseJSONValue ftype)
            return $ Just (fid, (str, val))
          Nothing -> lexeme parseAnyValue *> return Nothing

parseJSONMap :: ThriftType -> ThriftType -> Parser [(ThriftVal, ThriftVal)]
parseJSONMap kt@T_STRING vt =
  ((,) <$> lexeme (parseJSONValue kt) <*>
   (lexeme (PC.char8 ':') *> lexeme (parseJSONValue vt))) `sepBy`
  lexeme (PC.char8 ',')
parseJSONMap kt vt =
  ((,) <$> lexeme (PC.char8 '"' *> parseJSONValue kt <* PC.char8 '"') <*>
   (lexeme (PC.char8 ':') *> lexeme (parseJSONValue vt))) `sepBy`
  lexeme (PC.char8 ',')

parseJSONList :: ThriftType -> Parser [ThriftVal]
parseJSONList ty = lexeme (parseJSONValue ty) `sepBy` lexeme (PC.char8 ',')
