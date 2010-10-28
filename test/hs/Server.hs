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

module Server where

import ThriftTest
import ThriftTest_Iface
import Data.Map as Map
import Control.Exception
import ThriftTest_Types

import Thrift
import Thrift.Server


data TestHandler = TestHandler
instance ThriftTest_Iface TestHandler where
    testVoid a = return ()
    testString a (Just s) = do print s; return s
    testByte a (Just x) = do print x; return x
    testI32 a (Just x) = do print x; return x
    testI64 a (Just x) = do print x; return x
    testDouble a (Just x) = do print x; return x
    testStruct a (Just x) = do print x; return x
    testNest a (Just x) = do print x; return x
    testMap a (Just x) = do print x; return x
    testSet a (Just x) = do print x; return x
    testList a (Just x) = do print x; return x
    testEnum a (Just x) = do print x; return x
    testTypedef a (Just x) = do print x; return x
    testMapMap a (Just x) = return (Map.fromList [(1,Map.fromList [(2,2)])])
    testInsanity a (Just x) = return (Map.fromList [(1,Map.fromList [(ONE,x)])])
    testMulti a a1 a2 a3 a4 a5 a6 = return (Xtruct Nothing Nothing Nothing Nothing)
    testException a c = throw (Xception (Just 1) (Just "bya"))
    testMultiException a c1 c2 = throw (Xception (Just 1) (Just "xyz"))
    testOneway a (Just i) = do print i


main = do (runBasicServer TestHandler process 9090)
          `Control.Exception.catch`
          (\(TransportExn s t) -> print s)
