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

module Thrift.Protocol.JSONUtils
       ( escapedString
       , escapedChar
       , escape
       , lexeme
       , notChar8
       , between
       ) where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 as PC
import Data.ByteString.Lazy.Builder as B
import Data.ByteString.Internal (c2w, w2c)
import Data.Monoid
import Data.Word
import qualified Data.ByteString.Lazy as LBS

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
                                , '/'  <$ PC.char '/'
                                ])

escape :: LBS.ByteString -> Builder
escape = LBS.foldl' escapeChar mempty
  where
    escapeChar b w = b <> case w2c w of
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
      _ -> B.word8 w

lexeme :: Parser a -> Parser a
lexeme = (<* skipSpace)

notChar8 :: Char -> Parser Word8
notChar8 c = P.satisfy (/= c2w c)

between :: Char -> Char -> Parser a -> Parser a
between a b p = lexeme (PC.char8 a) *> lexeme p <* lexeme (PC.char8 b)
