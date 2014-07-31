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

module Thrift.Protocol.JSON
    ( module Thrift.Protocol
    , JSONProtocol(..)
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as PC
import Data.Attoparsec.ByteString.Lazy as LP
import Data.ByteString.Lazy.Builder as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Int
import Data.List
import Data.Monoid
import Data.Text.Lazy.Encoding
import Data.Word
import qualified Data.HashMap.Strict as Map

import Thrift.Protocol
import Thrift.Transport
import Thrift.Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT

data JSONProtocol t = JSONProtocol t

instance Protocol JSONProtocol where
    getTransport (JSONProtocol t) = t

    writeMessage (JSONProtocol t) (s, ty, sq) = tWrite t $ toLazyByteString $
      B.char8   '['   <> buildShowable version1 <>
      B.string8 ",\"" <> escape (encodeUtf8 s) <> B.char8 '\"' <>
      B.char8   ',' <> buildShowable (fromEnum ty) <>
      B.char8   ',' <> buildShowable sq <>
      B.char8   ']'
    readMessage p = runParser p $ skipSpace *> do
      _ver :: Int32 <- lexeme (PC.char8 '[') *> lexeme (signed decimal)
      bs <- lexeme (PC.char8 ',') *> lexeme escapedString
      case decodeUtf8' bs of
        Left _ -> fail "readMessage: invalid text encoding"
        Right str -> do
          ty <- toEnum <$> (lexeme (PC.char8 ',') *> lexeme (signed decimal))
          seqNum <- between ',' ']' $ lexeme (signed decimal)
          return (str, ty, seqNum)

    serializeVal _ = toLazyByteString . buildJSONValue
    deserializeVal _ ty bs =
      case LP.eitherResult $ LP.parse (parseJSONValue ty) bs of
        Left s -> error s
        Right val -> val

    readVal p ty = runParser p $ skipSpace *> parseJSONValue ty


-- | Writing Functions

buildJSONValue :: ThriftVal -> Builder
buildJSONValue (TStruct fields) =
  B.char8 '{' <> buildJSONStruct fields <> B.char8 '}'
buildJSONValue (TMap _ _ entries) =
  B.char8 '{' <> buildJSONMap entries <>  B.char8 '}'
buildJSONValue (TList _ entries) =
  B.char8 '[' <> buildJSONList entries <> B.char8 ']'
buildJSONValue (TSet _ entries) =
  B.char8 '[' <> buildJSONList entries <>  B.char8 ']'
buildJSONValue (TBool b) =
  if b then B.string8 "true" else B.string8 "false"
buildJSONValue (TByte b) = buildShowable b
buildJSONValue (TI16 i) = buildShowable i
buildJSONValue (TI32 i) = buildShowable i
buildJSONValue (TI64 i) = buildShowable i
buildJSONValue (TFloat f) = buildShowable f
buildJSONValue (TDouble d) = buildShowable d
buildJSONValue (TString s) = B.char8 '\"' <> escape s <> B.char8 '\"'

buildJSONStruct :: Map.HashMap Int16 (LT.Text, ThriftVal) -> Builder
buildJSONStruct = mconcat . intersperse (B.char8 ',') . Map.elems . Map.map (\(str,val) ->
  B.char8 '\"' <> B.lazyByteString (encodeUtf8 str) <> B.string8 "\":" <> buildJSONValue val)

buildJSONMap :: [(ThriftVal, ThriftVal)] -> Builder
buildJSONMap = mconcat . intersperse (B.char8 ',') . map buildKV
  where
    buildKV (key@(TString _), val) =
      buildJSONValue key <> B.char8 ':' <> buildJSONValue val
    buildKV (key, val) =
      B.char8 '\"' <> buildJSONValue key <> B.string8 "\":" <> buildJSONValue val
buildJSONList :: [ThriftVal] -> Builder
buildJSONList = mconcat . intersperse (B.char8 ',') . map buildJSONValue


-- | Reading Functions

parseJSONValue :: ThriftType -> Parser ThriftVal
parseJSONValue (T_STRUCT tmap) =
  TStruct <$> between '{' '}' (parseJSONStruct tmap)
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
parseJSONValue T_FLOAT = TFloat . realToFrac <$> double
parseJSONValue T_DOUBLE = TDouble <$> double
parseJSONValue T_STRING = TString <$> escapedString
parseJSONValue T_STOP = fail "parseJSONValue: cannot parse type T_STOP"
parseJSONValue T_VOID = fail "parseJSONValue: cannot parse type T_VOID"

parseJSONStruct :: TypeMap -> Parser (Map.HashMap Int16 (LT.Text, ThriftVal))
parseJSONStruct tmap = Map.fromList <$> parseField `sepBy` lexeme (PC.char8 ',')
  where
    parseField = do
      bs <- lexeme escapedString <* lexeme (PC.char8 ':')
      case decodeUtf8' bs of
        Left _ -> fail "parseJSONStruct: invalid key encoding"
        Right str -> case Map.lookup str tmap of
          Just (fid, ftype) -> do
            val <- lexeme (parseJSONValue ftype)
            return (fid, (str, val))
          Nothing -> fail "parseJSONStruct: invalid key"

parseJSONMap :: ThriftType -> ThriftType -> Parser [(ThriftVal, ThriftVal)]
parseJSONMap kt vt =
  ((,) <$> lexeme (PC.char8 '"' *> parseJSONValue kt <* PC.char8 '"') <*>
   (lexeme (PC.char8 ':') *> lexeme (parseJSONValue vt))) `sepBy`
  lexeme (PC.char8 ',')

parseJSONList :: ThriftType -> Parser [ThriftVal]
parseJSONList ty = lexeme (parseJSONValue ty) `sepBy` lexeme (PC.char8 ',')

buildShowable :: (Show a) => a -> Builder
buildShowable = string8 . show

escapedString :: Parser LBS.ByteString
escapedString = PC.char8 '"' *>
                (LBS.pack <$> P.many' (escapedChar <|> notChar8 '"')) <*
                PC.char8 '"'

escapedChar :: Parser Word8
escapedChar = PC.char8 '\\' *> (c2w <$> choice
                                [ '\SOH' <$ P.string "u0001"
                                , '\STX' <$ P.string "u0002"
                                , '\ETX' <$ P.string "u0003"
                                , '\EOT' <$ P.string "u0004"
                                , '\ENQ' <$ P.string "u0005"
                                , '\ACK' <$ P.string "u0006"
                                , '\BEL' <$ P.string "u0007"
                                , '\BS'  <$ P.string "u0008"
                                , '\VT'  <$ P.string "u000b"
                                , '\FF'  <$ P.string "u000c"
                                , '\CR'  <$ P.string "u000d"
                                , '\SO'  <$ P.string "u000e"
                                , '\SI'  <$ P.string "u000f"
                                , '\DLE' <$ P.string "u0010"
                                , '\DC1' <$ P.string "u0011"
                                , '\DC2' <$ P.string "u0012"
                                , '\DC3' <$ P.string "u0013"
                                , '\DC4' <$ P.string "u0014"
                                , '\NAK' <$ P.string "u0015"
                                , '\SYN' <$ P.string "u0016"
                                , '\ETB' <$ P.string "u0017"
                                , '\CAN' <$ P.string "u0018"
                                , '\EM'  <$ P.string "u0019"
                                , '\SUB' <$ P.string "u001a"
                                , '\ESC' <$ P.string "u001b"
                                , '\FS'  <$ P.string "u001c"
                                , '\GS'  <$ P.string "u001d"
                                , '\RS'  <$ P.string "u001e"
                                , '\US'  <$ P.string "u001f"
                                , '\DEL' <$ P.string "u007f"
                                , '\0' <$ PC.char '0'
                                , '\a' <$ PC.char 'a'
                                , '\b' <$ PC.char 'b'
                                , '\f' <$ PC.char 'f'
                                , '\n' <$ PC.char 'n'
                                , '\r' <$ PC.char 'r'
                                , '\t' <$ PC.char 't'
                                , '\v' <$ PC.char 'v'
                                , '\"' <$ PC.char '"'
                                , '\'' <$ PC.char '\''
                                , '\\' <$ PC.char '\\'
                                ])

escape :: LBS.ByteString -> Builder
escape = LBS.foldl' escapeChar mempty
  where
    escapeChar b w = b <> lazyByteString (case w2c w of
      '\0' -> "\\0"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      '\"' -> "\\\""
      '\\' -> "\\\\"
      '\SOH' -> "\\u0001"
      '\STX' -> "\\u0002"
      '\ETX' -> "\\u0003"
      '\EOT' -> "\\u0004"
      '\ENQ' -> "\\u0005"
      '\ACK' -> "\\u0006"
      '\BEL' -> "\\u0007"
      '\VT'  -> "\\u000b"
      '\SO'  -> "\\u000e"
      '\SI'  -> "\\u000f"
      '\DLE' -> "\\u0010"
      '\DC1' -> "\\u0011"
      '\DC2' -> "\\u0012"
      '\DC3' -> "\\u0013"
      '\DC4' -> "\\u0014"
      '\NAK' -> "\\u0015"
      '\SYN' -> "\\u0016"
      '\ETB' -> "\\u0017"
      '\CAN' -> "\\u0018"
      '\EM'  -> "\\u0019"
      '\SUB' -> "\\u001a"
      '\ESC' -> "\\u001b"
      '\FS'  -> "\\u001c"
      '\GS'  -> "\\u001d"
      '\RS'  -> "\\u001e"
      '\US'  -> "\\u001f"
      '\DEL' -> "\\u007f"
      _ -> LBS.singleton w)

lexeme :: Parser a -> Parser a
lexeme = (<* skipSpace)

notChar8 :: Char -> Parser Word8
notChar8 c = P.satisfy (/= c2w c)

between :: Char -> Char -> Parser a -> Parser a
between a b p = lexeme (PC.char8 a) *> lexeme p <* lexeme (PC.char8 b)
