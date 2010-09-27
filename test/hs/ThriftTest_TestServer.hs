--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License. You may obtain _ copy of the License at
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

module ThriftTest_TestServer where

import ThriftTest
import ThriftTest_Iface
import Data.Map as Map
import Control.Exception
import ThriftTest_Types

import Thrift
import Thrift.Server


data TestHandler = TestHandler
instance ThriftTest_Iface TestHandler where
    testVoid _ = return ()

    testString _ (Just s) = do
        print s
        return s

    testString _ Nothing = do
        error $ "Unsupported testString form"

    testByte _ (Just x) = do
        print x
        return x

    testByte _ Nothing = do
        error $ "Unsupported testByte form"

    testI32 _ (Just x) = do
        print x
        return x

    testI32 _ Nothing = do
        error $ "Unsupported testI32 form"

    testI64 _ (Just x) = do
        print x
        return x

    testI64 _ Nothing = do
        error $ "Unsupported testI64 form"

    testDouble _ (Just x) = do
        print x
        return x

    testDouble _ Nothing = do
        error $ "Unsupported testDouble form"

    testStruct _ (Just x) = do
        print x
        return x

    testStruct _ Nothing = do
        error $ "Unsupported testStruct form"

    testNest _ (Just x) = do
        print x
        return x

    testNest _ Nothing = do
        error $ "Unsupported testNest form"

    testMap _ (Just x) = do
        print x
        return x

    testMap _ Nothing = do
        error $ "Unsupported testMap form"

    testSet _ (Just x) = do
        print x
        return x

    testSet _ Nothing = do
        error $ "Unsupported testSet form"

    testList _ (Just x) = do
        print x
        return x

    testList _ Nothing = do
        error $ "Unsupported testList form"

    testEnum _ (Just x) = do
        print x
        return x

    testEnum _ Nothing = do
        error $ "Unsupported testEnum form"

    testTypedef _ (Just x) = do
        print x
        return x

    testTypedef _ Nothing = do
        error $ "Unsupported testTypedef form"

    testMapMap _ (Just _) = do
        return (Map.fromList [(1, Map.fromList [(2, 2)])])

    testMapMap _ Nothing = do
        error $ "Unsupported testMapMap form"

    testInsanity _ (Just x) = do
        return (Map.fromList [(1, Map.fromList [(ONE, x)])])

    testInsanity _ Nothing = do
        error $ "Unsupported testInsanity form"

    testMulti _ _ _ _ _ _ _ = do
        return (Xtruct Nothing Nothing Nothing Nothing)

    testException _ _ = do
        throw (Xception (Just 1) (Just "bya"))

    testMultiException _ _ _ = do
        throw (Xception (Just 1) (Just "xyz"))

    testOneway _ (Just i) = do
        print i

    testOneway _ Nothing = do
        error $ "Unsupported testOneway form"


main :: IO ()
main = do putStrLn "Server ready..."
          (runBasicServer TestHandler process 9090)
          `Control.Exception.catch`
          (\(TransportExn s _) -> print s)
