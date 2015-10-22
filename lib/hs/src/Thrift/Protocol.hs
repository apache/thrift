{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Thrift.Protocol
    ( Protocol(..)
    , ProtocolExn(..)
    , ProtocolExnType(..)
    , getTypeOf
    , runParser
    , bsToDouble
    ) where

import Control.Exception
import Data.Attoparsec.ByteString
import Data.Bits
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Unsafe
import Data.Functor ((<$>))
import Data.Int
import Data.Monoid (mempty)
import Data.Text.Lazy (Text)
import Data.Typeable (Typeable)
import Data.Word
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, peek, poke)
import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as Map

import Thrift.Types
import Thrift.Transport

-- | The Thrift Protocol type class.  A 'Protocol' defines a way in which a
-- Thrift type should be serialized and deserialized (ie JSON, Binary, etc).
-- All 'Protocol's should satisfy the round trip property:
-- >>> @deserializeVal p t (serializeVal p v) = v@
class Protocol a where
  -- | Get the underlying Transport for this Protocol
  getTransport :: Transport t => a t -> t

  -- | Write a message using this Protocol
  writeMessage :: Transport t
               => a t
               -> (Text, MessageType, Int32) -- ^ The message parameters
               -> IO () -- ^ IO action to perform inside message
               -> IO ()
  -- | Read a message using this Protocol
  readMessage :: Transport t
              => a t
              -> ((Text, MessageType, Int32) -> IO b) -- ^ Read function
              -> IO b

  -- | Serialize a 'ThriftVal' using this protocol without any monadic action.
  -- You should use 'Empty.EmptyTransport' when constructing a 'Protocol' if you
  -- are only planning on using this function
  serializeVal :: Transport t => a t -> ThriftVal -> ByteString
  -- | Deserialize a 'ThriftVal' using this protocol without any monadic action.
  -- You should use 'Empty.EmptyTransport' when constructing a 'Protocol' if you
  -- are only planning on using this function
  deserializeVal :: Transport t => a t -> ThriftType -> ByteString -> ThriftVal

  -- | Write a 'ThriftVal' using the underlying transport for this Protocol
  -- This function has a default definition in terms of `serializeVal`
  writeVal :: Transport t => a t -> ThriftVal -> IO ()
  writeVal p = tWrite (getTransport p) . serializeVal p
  -- | Read a 'ThriftVal' using the underlying Transport
  -- There is no default definition since the input should be incrementally
  -- parsed, not read all at once
  readVal :: Transport t => a t -> ThriftType -> IO ThriftVal

data ProtocolExnType
    = PE_UNKNOWN
    | PE_INVALID_DATA
    | PE_NEGATIVE_SIZE
    | PE_SIZE_LIMIT
    | PE_BAD_VERSION
    | PE_NOT_IMPLEMENTED
    | PE_MISSING_REQUIRED_FIELD
      deriving ( Eq, Show, Typeable )

data ProtocolExn = ProtocolExn ProtocolExnType String
  deriving ( Show, Typeable )
instance Exception ProtocolExn

getTypeOf :: ThriftVal -> ThriftType
getTypeOf v =  case v of
  TStruct{} -> T_STRUCT Map.empty
  TMap{} -> T_MAP T_VOID T_VOID
  TList{} -> T_LIST T_VOID
  TSet{} -> T_SET T_VOID
  TBool{} -> T_BOOL
  TByte{} -> T_BYTE
  TI16{} -> T_I16
  TI32{} -> T_I32
  TI64{} -> T_I64
  TString{} -> T_STRING
  TDouble{} -> T_DOUBLE

-- | Run an attoparsec Parser using the 'tRead' function of the underlying
-- transport
runParser :: (Protocol p, Transport t, Show a) => p t -> Parser a -> IO a
runParser prot p = refill >>= getResult . parse p
  where
    refill = handle handleEOF $ toStrict <$> tReadAll (getTransport prot) 1
    getResult (Done _ a) = return a
    getResult (Partial k) = refill >>= getResult . k
    getResult f = throw $ ProtocolExn PE_INVALID_DATA $ show f

handleEOF :: SomeException -> IO BS.ByteString
handleEOF = const $ return mempty

-- | Converts a 'ByteString' to a floating point number
-- The 'ByteString' is assumed to be encoded in network order (Big Endian)
-- therefore the behavior of this function varies based on whether the local
-- machine is big endian or little endian.
bsToDouble :: BS.ByteString -> Double
bsToDouble bs = unsafeDupablePerformIO $ unsafeUseAsCString bs castBs
  where
#if __BYTE_ORDER == __LITTLE_ENDIAN
    castBs chrPtr = do
      w <- peek (castPtr chrPtr)
      poke (castPtr chrPtr) (byteSwap w)
      peek (castPtr chrPtr)
#else
    castBs = peek . castPtr
#endif

#if __BYTE_ORDER == __LITTLE_ENDIAN
-- | Swap endianness of a 64-bit word
byteSwap :: Word64 -> Word64
byteSwap w = (w `shiftL` 56 .&. 0xFF00000000000000) .|.
             (w `shiftL` 40 .&. 0x00FF000000000000) .|.
             (w `shiftL` 24 .&. 0x0000FF0000000000) .|.
             (w `shiftL` 8  .&. 0x000000FF00000000) .|.
             (w `shiftR` 8  .&. 0x00000000FF000000) .|.
             (w `shiftR` 24 .&. 0x0000000000FF0000) .|.
             (w `shiftR` 40 .&. 0x000000000000FF00) .|.
             (w `shiftR` 56 .&. 0x00000000000000FF)
#endif
