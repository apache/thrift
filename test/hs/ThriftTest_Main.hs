{-# LANGUAGE ScopedTypeVariables #-}
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

module Main where


import qualified Control.Exception
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network

import Thrift
import Thrift.Protocol.Binary
import Thrift.Server
import Thrift.Transport.Handle

import qualified ThriftTestUtils

import qualified ThriftTest
import qualified ThriftTest_Client as Client
import qualified ThriftTest_Iface as Iface
import qualified ThriftTest_Types as Types


data TestHandler = TestHandler
instance Iface.ThriftTest_Iface TestHandler where
    testVoid _ = return ()

    testString _ (Just s) = do
        ThriftTestUtils.serverLog s
        return s

    testString _ Nothing = do
        error $ "Unsupported testString form"

    testByte _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testByte _ Nothing = do
        error $ "Unsupported testByte form"

    testI32 _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testI32 _ Nothing = do
        error $ "Unsupported testI32 form"

    testI64 _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testI64 _ Nothing = do
        error $ "Unsupported testI64 form"

    testDouble _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testDouble _ Nothing = do
        error $ "Unsupported testDouble form"

    testStruct _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testStruct _ Nothing = do
        error $ "Unsupported testStruct form"

    testNest _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testNest _ Nothing = do
        error $ "Unsupported testNest form"

    testMap _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testMap _ Nothing = do
        error $ "Unsupported testMap form"

    testStringMap _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testStringMap _ Nothing = do
        error $ "Unsupported testMap form"

    testSet _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testSet _ Nothing = do
        error $ "Unsupported testSet form"

    testList _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testList _ Nothing = do
        error $ "Unsupported testList form"

    testEnum _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testEnum _ Nothing = do
        error $ "Unsupported testEnum form"

    testTypedef _ (Just x) = do
        ThriftTestUtils.serverLog $ show x
        return x

    testTypedef _ Nothing = do
        error $ "Unsupported testTypedef form"

    testMapMap _ (Just _) = do
        return (Map.fromList [(1, Map.fromList [(2, 2)])])

    testMapMap _ Nothing = do
        error $ "Unsupported testMapMap form"

    testInsanity _ (Just x) = do
        return (Map.fromList [(1, Map.fromList [(Types.ONE, x)])])

    testInsanity _ Nothing = do
        error $ "Unsupported testInsanity form"

    testMulti _ _ _ _ _ _ _ = do
        return (Types.Xtruct Nothing Nothing Nothing Nothing)

    testException _ _ = do
        Control.Exception.throw (Types.Xception (Just 1) (Just "bya"))

    testMultiException _ _ _ = do
        Control.Exception.throw (Types.Xception (Just 1) (Just "xyz"))

    testOneway _ (Just i) = do
        ThriftTestUtils.serverLog $ show i

    testOneway _ Nothing = do
        error $ "Unsupported testOneway form"


client :: (String, Network.PortID) -> IO ()
client addr = do
    to <- hOpen addr
    let ps = (BinaryProtocol to, BinaryProtocol to)

    v1 <- Client.testString ps "bya"
    ThriftTestUtils.clientLog v1

    v2 <- Client.testByte ps 8
    ThriftTestUtils.clientLog $ show v2

    v3 <- Client.testByte ps (-8)
    ThriftTestUtils.clientLog $ show v3

    v4 <- Client.testI32 ps 32
    ThriftTestUtils.clientLog $ show v4

    v5 <- Client.testI32 ps (-32)
    ThriftTestUtils.clientLog $ show v5

    v6 <- Client.testI64 ps 64
    ThriftTestUtils.clientLog $ show v6

    v7 <- Client.testI64 ps (-64)
    ThriftTestUtils.clientLog $ show v7

    v8 <- Client.testDouble ps 3.14
    ThriftTestUtils.clientLog $ show v8

    v9 <- Client.testDouble ps (-3.14)
    ThriftTestUtils.clientLog $ show v9

    v10 <- Client.testMap ps (Map.fromList [(1,1),(2,2),(3,3)])
    ThriftTestUtils.clientLog $ show v10

    v11 <- Client.testStringMap ps (Map.fromList [("a","123"),("a b","with spaces "),("same","same"),("0","numeric key")])
    ThriftTestUtils.clientLog $ show v11

    v12 <- Client.testList ps [1,2,3,4,5]
    ThriftTestUtils.clientLog $ show v12

    v13 <- Client.testSet ps (Set.fromList [1,2,3,4,5])
    ThriftTestUtils.clientLog $ show v13

    v14 <- Client.testStruct ps (Types.Xtruct (Just "hi") (Just 4) (Just 5) Nothing)
    ThriftTestUtils.clientLog $ show v14

    (testException ps "bad") `Control.Exception.catch` testExceptionHandler

    (testMultiException ps "ok") `Control.Exception.catch` testMultiExceptionHandler1
    (testMultiException ps "bad") `Control.Exception.catch` testMultiExceptionHandler2 `Control.Exception.catch` testMultiExceptionHandler3

    -- (  (Client.testMultiException ps "e" "e2">> ThriftTestUtils.clientLog "bad") `Control.Exception.catch` 

    tClose to
  where testException ps msg = do
            Client.testException ps "e"
            ThriftTestUtils.clientLog msg
            return ()

        testExceptionHandler (e :: Types.Xception) = do
            ThriftTestUtils.clientLog $ show e

        testMultiException ps msg = do
            _ <- Client.testMultiException ps "e" "e2"
            ThriftTestUtils.clientLog msg
            return ()

        testMultiExceptionHandler1 (e :: Types.Xception) = do
            ThriftTestUtils.clientLog $ show e

        testMultiExceptionHandler2 (e :: Types.Xception2) = do
            ThriftTestUtils.clientLog $ show e

        testMultiExceptionHandler3 (_ :: Control.Exception.SomeException) = do
            ThriftTestUtils.clientLog "ok"


server :: Network.PortNumber -> IO ()
server port = do
    ThriftTestUtils.serverLog "Ready..."
    (runBasicServer TestHandler ThriftTest.process port)
    `Control.Exception.catch`
    (\(TransportExn s _) -> error $ "FAILURE: " ++ s)


main :: IO ()
main = ThriftTestUtils.runTest server client
