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

module ConstantsDemo_TestClient where


import Network

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle

import Yowza_Client


serverAddress :: (String, PortID)
serverAddress = ("127.0.0.1", PortNumber 9090)

main :: IO ()
main = do
    to <- hOpen serverAddress
    let p =  BinaryProtocol to
    let ps = (p,p)
    blingity ps
    print =<< blangity ps
    tClose to

