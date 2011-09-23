{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
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

module Thrift
    ( module Thrift.Transport
    , module Thrift.Protocol
    , AppExnType(..)
    , AppExn(..)
    , readAppExn
    , writeAppExn
    , ThriftException(..)
    ) where

import Control.Monad ( when )
import Control.Exception

import Data.Typeable ( Typeable )

import Thrift.Transport
import Thrift.Protocol


data ThriftException = ThriftException
  deriving ( Show, Typeable )
instance Exception ThriftException

data AppExnType
    = AE_UNKNOWN
    | AE_UNKNOWN_METHOD
    | AE_INVALID_MESSAGE_TYPE
    | AE_WRONG_METHOD_NAME
    | AE_BAD_SEQUENCE_ID
    | AE_MISSING_RESULT
    | AE_INTERNAL_ERROR
    | AE_PROTOCOL_ERROR
      deriving ( Eq, Show, Typeable )

instance Enum AppExnType where
    toEnum 0 = AE_UNKNOWN
    toEnum 1 = AE_UNKNOWN_METHOD
    toEnum 2 = AE_INVALID_MESSAGE_TYPE
    toEnum 3 = AE_WRONG_METHOD_NAME
    toEnum 4 = AE_BAD_SEQUENCE_ID
    toEnum 5 = AE_MISSING_RESULT
    toEnum 6 = AE_INTERNAL_ERROR
    toEnum 7 = AE_PROTOCOL_ERROR
    toEnum t = error $ "Invalid AppExnType " ++ show t

    fromEnum AE_UNKNOWN = 0
    fromEnum AE_UNKNOWN_METHOD = 1
    fromEnum AE_INVALID_MESSAGE_TYPE = 2
    fromEnum AE_WRONG_METHOD_NAME = 3
    fromEnum AE_BAD_SEQUENCE_ID = 4
    fromEnum AE_MISSING_RESULT = 5
    fromEnum AE_INTERNAL_ERROR = 6
    fromEnum AE_PROTOCOL_ERROR = 7

data AppExn = AppExn { ae_type :: AppExnType, ae_message :: String }
  deriving ( Show, Typeable )
instance Exception AppExn

writeAppExn :: (Protocol p, Transport t) => p t -> AppExn -> IO ()
writeAppExn pt ae = do
    writeStructBegin pt "TApplicationException"

    when (ae_message ae /= "") $ do
        writeFieldBegin pt ("message", T_STRING , 1)
        writeString pt (ae_message ae)
        writeFieldEnd pt

    writeFieldBegin pt ("type", T_I32, 2);
    writeI32 pt (fromIntegral $ fromEnum (ae_type ae))
    writeFieldEnd pt
    writeFieldStop pt
    writeStructEnd pt

readAppExn :: (Protocol p, Transport t) => p t -> IO AppExn
readAppExn pt = do
    _ <- readStructBegin pt
    record <- readAppExnFields pt (AppExn {ae_type = undefined, ae_message = undefined})
    readStructEnd pt
    return record

readAppExnFields :: forall (a :: * -> *) t. (Protocol a, Transport t) => a t -> AppExn -> IO AppExn 
readAppExnFields pt record = do
    (_, ft, tag) <- readFieldBegin pt
    if ft == T_STOP
        then return record
        else case tag of
                 1 -> if ft == T_STRING then
                          do s <- readString pt
                             readAppExnFields pt record{ae_message = s}
                          else do skip pt ft
                                  readAppExnFields pt record
                 2 -> if ft == T_I32 then
                          do i <- readI32 pt
                             readAppExnFields pt record{ae_type = (toEnum $ fromIntegral i)}
                          else do skip pt ft
                                  readAppExnFields pt record
                 _ -> do skip pt ft
                         readFieldEnd pt
                         readAppExnFields pt record

