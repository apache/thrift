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

module Client where
import Thrift
import ThriftTest_Client
import ThriftTest_Types
import TSocket
import TBinaryProtocol
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
t = TSocket "127.0.0.1" 9090 Nothing

main = do to <- topen t
          let p =  TBinaryProtocol to
          let ps = (p,p)
          print =<< testString ps "bya"
          print =<< testByte ps 8
          print =<< testByte ps (-8)
          print =<< testI32 ps 32
          print =<< testI32 ps (-32)
          print =<< testI64 ps 64
          print =<< testI64 ps (-64)
          print =<< testDouble ps 3.14
          print =<< testDouble ps (-3.14)
          print =<< testMap ps (Map.fromList [(1,1),(2,2),(3,3)])
          print =<< testList ps [1,2,3,4,5]
          print =<< testSet ps (Set.fromList [1,2,3,4,5])
          print =<< testStruct ps (Xtruct (Just "hi") (Just 4) (Just 5) Nothing)
          tclose to

