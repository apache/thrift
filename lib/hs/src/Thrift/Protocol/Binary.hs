{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
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

module Thrift.Protocol.Binary
    ( module Thrift.Protocol
    , BinaryProtocol(..)
    ) where

import Control.Exception ( throw )
import Control.Monad ( liftM )

import qualified Data.Binary
import Data.Bits
import Data.Int
import Data.Text.Lazy.Encoding ( decodeUtf8, encodeUtf8 )

import GHC.Exts
import GHC.Word

import Thrift.Protocol
import Thrift.Transport

import qualified Data.ByteString.Lazy as LBS

version_mask :: Int32
version_mask = 0xffff0000

version_1 :: Int32
version_1    = 0x80010000

data BinaryProtocol a = Transport a => BinaryProtocol a


instance Protocol BinaryProtocol where
    getTransport (BinaryProtocol t) = t

    writeMessageBegin p (n, t, s) = do
        writeI32 p (version_1 .|. (fromIntegral $ fromEnum t))
        writeString p n
        writeI32 p s
    writeMessageEnd _ = return ()

    writeStructBegin _ _ = return ()
    writeStructEnd _ = return ()
    writeFieldBegin p (_, t, i) = writeType p t >> writeI16 p i
    writeFieldEnd _ = return ()
    writeFieldStop p = writeType p T_STOP
    writeMapBegin p (k, v, n) = writeType p k >> writeType p v >> writeI32 p n
    writeMapEnd _ = return ()
    writeListBegin p (t, n) = writeType p t >> writeI32 p n
    writeListEnd _ = return ()
    writeSetBegin p (t, n) = writeType p t >> writeI32 p n
    writeSetEnd _ = return ()

    writeBool p b = tWrite (getTransport p) $ LBS.singleton $ toEnum $ if b then 1 else 0
    writeByte p b = tWrite (getTransport p) $ Data.Binary.encode b
    writeI16 p b = tWrite (getTransport p) $ Data.Binary.encode b
    writeI32 p b = tWrite (getTransport p) $ Data.Binary.encode b
    writeI64 p b = tWrite (getTransport p) $ Data.Binary.encode b
    writeDouble p d = writeI64 p (fromIntegral $ floatBits d)
    writeString p s = writeI32 p (fromIntegral $ LBS.length s') >> tWrite (getTransport p) s'
      where
        s' = encodeUtf8 s
    writeBinary p s = writeI32 p (fromIntegral $ LBS.length s) >> tWrite (getTransport p) s

    readMessageBegin p = do
        ver <- readI32 p
        if (ver .&. version_mask /= version_1)
            then throw $ ProtocolExn PE_BAD_VERSION "Missing version identifier"
            else do
              s <- readString p
              sz <- readI32 p
              return (s, toEnum $ fromIntegral $ ver .&. 0xFF, sz)
    readMessageEnd _ = return ()
    readStructBegin _ = return ""
    readStructEnd _ = return ()
    readFieldBegin p = do
        t <- readType p
        n <- if t /= T_STOP then readI16 p else return 0
        return ("", t, n)
    readFieldEnd _ = return ()
    readMapBegin p = do
        kt <- readType p
        vt <- readType p
        n <- readI32 p
        return (kt, vt, n)
    readMapEnd _ = return ()
    readListBegin p = do
        t <- readType p
        n <- readI32 p
        return (t, n)
    readListEnd _ = return ()
    readSetBegin p = do
        t <- readType p
        n <- readI32 p
        return (t, n)
    readSetEnd _ = return ()

    readBool p = (== 1) `fmap` readByte p

    readByte p = do
        bs <- tReadAll (getTransport p) 1
        return $ Data.Binary.decode bs

    readI16 p = do
        bs <- tReadAll (getTransport p) 2
        return $ Data.Binary.decode bs

    readI32 p = do
        bs <- tReadAll (getTransport p) 4
        return $ Data.Binary.decode bs

    readI64 p = do
        bs <- tReadAll (getTransport p) 8
        return $ Data.Binary.decode bs

    readDouble p = do
        bs <- readI64 p
        return $ floatOfBits $ fromIntegral bs

    readString p = do
        i <- readI32 p
        decodeUtf8 `liftM` tReadAll (getTransport p) (fromIntegral i)

    readBinary p = do
        i <- readI32 p
        tReadAll (getTransport p) (fromIntegral i)


-- | Write a type as a byte
writeType :: (Protocol p, Transport t) => p t -> ThriftType -> IO ()
writeType p t = writeByte p (fromIntegral $ fromEnum t)

-- | Read a byte as though it were a ThriftType
readType :: (Protocol p, Transport t) => p t -> IO ThriftType
readType p = do
    b <- readByte p
    return $ toEnum $ fromIntegral b

floatBits :: Double -> Word64
floatBits (D# d#) = W64# (unsafeCoerce# d#)

floatOfBits :: Word64 -> Double
floatOfBits (W64# b#) = D# (unsafeCoerce# b#)

