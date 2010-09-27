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

module ConstantsDemo_TestServer where


import Control.Exception

import Thrift
import Thrift.Server

import Yowza
import Yowza_Iface


data YowzaHandler = YowzaHandler
instance Yowza_Iface YowzaHandler where
    blingity _ = do
        print $ "Got blingity"
        return ()

    blangity _ = do
        print $ "Got blangity"
        return $ 31


main :: IO ()
main = do putStrLn "Server ready..."
          (runBasicServer YowzaHandler process 9090)
          `Control.Exception.catch`
          (\(TransportExn s _) -> print s)
