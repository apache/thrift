{-# LANGUAGE DeriveDataTypeable #-}
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
    , skip
    , MessageType(..)
    , ThriftType(..)
    , ProtocolExn(..)
    , ProtocolExnType(..)
    ) where

import Control.Monad ( replicateM_, unless )
import Control.Exception
import Data.Int
import Data.Typeable ( Typeable )
import Data.ByteString.Lazy

import Thrift.Transport


data ThriftType
    = T_STOP
    | T_VOID
    | T_BOOL
    | T_BYTE
    | T_DOUBLE
    | T_I16
    | T_I32
    | T_I64
    | T_STRING
    | T_STRUCT
    | T_MAP
    | T_SET
    | T_LIST
      deriving ( Eq )

instance Enum ThriftType where
    fromEnum T_STOP   = 0
    fromEnum T_VOID   = 1
    fromEnum T_BOOL   = 2
    fromEnum T_BYTE   = 3
    fromEnum T_DOUBLE = 4
    fromEnum T_I16    = 6
    fromEnum T_I32    = 8
    fromEnum T_I64    = 10
    fromEnum T_STRING = 11
    fromEnum T_STRUCT = 12
    fromEnum T_MAP    = 13
    fromEnum T_SET    = 14
    fromEnum T_LIST   = 15

    toEnum 0  = T_STOP
    toEnum 1  = T_VOID
    toEnum 2  = T_BOOL
    toEnum 3  = T_BYTE
    toEnum 4  = T_DOUBLE
    toEnum 6  = T_I16
    toEnum 8  = T_I32
    toEnum 10 = T_I64
    toEnum 11 = T_STRING
    toEnum 12 = T_STRUCT
    toEnum 13 = T_MAP
    toEnum 14 = T_SET
    toEnum 15 = T_LIST
    toEnum t = error $ "Invalid ThriftType " ++ show t

data MessageType
    = M_CALL
    | M_REPLY
    | M_EXCEPTION
      deriving ( Eq )

instance Enum MessageType where
    fromEnum M_CALL      =  1
    fromEnum M_REPLY     =  2
    fromEnum M_EXCEPTION =  3

    toEnum 1 = M_CALL
    toEnum 2 = M_REPLY
    toEnum 3 = M_EXCEPTION
    toEnum t = error $ "Invalid MessageType " ++ show t


class Protocol a where
    getTransport :: Transport t => a t -> t

    writeMessageBegin :: Transport t => a t -> (String, MessageType, Int32) -> IO ()
    writeMessageEnd   :: Transport t => a t -> IO ()

    writeStructBegin :: Transport t => a t -> String -> IO ()
    writeStructEnd   :: Transport t => a t -> IO ()
    writeFieldBegin  :: Transport t => a t -> (String, ThriftType, Int16) -> IO ()
    writeFieldEnd    :: Transport t => a t -> IO ()
    writeFieldStop   :: Transport t => a t -> IO ()
    writeMapBegin    :: Transport t => a t -> (ThriftType, ThriftType, Int32) -> IO ()
    writeMapEnd      :: Transport t => a t -> IO ()
    writeListBegin   :: Transport t => a t -> (ThriftType, Int32) -> IO ()
    writeListEnd     :: Transport t => a t -> IO ()
    writeSetBegin    :: Transport t => a t -> (ThriftType, Int32) -> IO ()
    writeSetEnd      :: Transport t => a t -> IO ()

    writeBool   :: Transport t => a t -> Bool -> IO ()
    writeByte   :: Transport t => a t -> Int8 -> IO ()
    writeI16    :: Transport t => a t -> Int16 -> IO ()
    writeI32    :: Transport t => a t -> Int32 -> IO ()
    writeI64    :: Transport t => a t -> Int64 -> IO ()
    writeDouble :: Transport t => a t -> Double -> IO ()
    writeString :: Transport t => a t -> String -> IO ()
    writeBinary :: Transport t => a t -> ByteString -> IO ()


    readMessageBegin :: Transport t => a t -> IO (String, MessageType, Int32)
    readMessageEnd   :: Transport t => a t -> IO ()

    readStructBegin :: Transport t => a t -> IO String
    readStructEnd   :: Transport t => a t -> IO ()
    readFieldBegin  :: Transport t => a t -> IO (String, ThriftType, Int16)
    readFieldEnd    :: Transport t => a t -> IO ()
    readMapBegin    :: Transport t => a t -> IO (ThriftType, ThriftType, Int32)
    readMapEnd      :: Transport t => a t -> IO ()
    readListBegin   :: Transport t => a t -> IO (ThriftType, Int32)
    readListEnd     :: Transport t => a t -> IO ()
    readSetBegin    :: Transport t => a t -> IO (ThriftType, Int32)
    readSetEnd      :: Transport t => a t -> IO ()

    readBool   :: Transport t => a t -> IO Bool
    readByte   :: Transport t => a t -> IO Int8
    readI16    :: Transport t => a t -> IO Int16
    readI32    :: Transport t => a t -> IO Int32
    readI64    :: Transport t => a t -> IO Int64
    readDouble :: Transport t => a t -> IO Double
    readString :: Transport t => a t -> IO String
    readBinary :: Transport t => a t -> IO ByteString


skip :: (Protocol p, Transport t) => p t -> ThriftType -> IO ()
skip _ T_STOP = return ()
skip _ T_VOID = return ()
skip p T_BOOL = readBool p >> return ()
skip p T_BYTE = readByte p >> return ()
skip p T_I16 = readI16 p >> return ()
skip p T_I32 = readI32 p >> return ()
skip p T_I64 = readI64 p >> return ()
skip p T_DOUBLE = readDouble p >> return ()
skip p T_STRING = readString p >> return ()
skip p T_STRUCT = do _ <- readStructBegin p
                     skipFields p
                     readStructEnd p
skip p T_MAP = do (k, v, s) <- readMapBegin p
                  replicateM_ (fromIntegral s) (skip p k >> skip p v)
                  readMapEnd p
skip p T_SET = do (t, n) <- readSetBegin p
                  replicateM_ (fromIntegral n) (skip p t)
                  readSetEnd p
skip p T_LIST = do (t, n) <- readListBegin p
                   replicateM_ (fromIntegral n) (skip p t)
                   readListEnd p


skipFields :: (Protocol p, Transport t) => p t -> IO ()
skipFields p = do
    (_, t, _) <- readFieldBegin p
    unless (t == T_STOP) (skip p t >> readFieldEnd p >> skipFields p)


data ProtocolExnType
    = PE_UNKNOWN
    | PE_INVALID_DATA
    | PE_NEGATIVE_SIZE
    | PE_SIZE_LIMIT
    | PE_BAD_VERSION
      deriving ( Eq, Show, Typeable )

data ProtocolExn = ProtocolExn ProtocolExnType String
  deriving ( Show, Typeable )
instance Exception ProtocolExn
