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

import qualified Calculator
import qualified Calculator_Client as Client
import qualified SharedService_Client as SClient
import Tutorial_Types
import SharedService_Iface
import Shared_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Transport.Handle
import Thrift.Server

import Data.Maybe
import Text.Printf
import Network

main = do
  transport  <- hOpen ("localhost", PortNumber 9090)
  let binProto = BinaryProtocol transport
  let client = (binProto, binProto)

  Client.ping client
  print "ping()"

  sum <- Client.add client 1 1
  printf "1+1=%d\n" sum


  let work = Work { f_Work_op = Just DIVIDE,
                    f_Work_num1 = Just 1,
                    f_Work_num2 = Just 0,
                    f_Work_comment = Nothing
                  }

  -- TODO - get this one working
  --catch (Client.calculate client 1 work) (\except ->
  --     printf "InvalidOp %s" (show except))


  let work = Work { f_Work_op = Just SUBTRACT,
                    f_Work_num1 = Just 15,
                    f_Work_num2 = Just 10,
                    f_Work_comment = Nothing
                  }

  diff <- Client.calculate client 1 work
  printf "15-10=%d\n" diff

  log <- SClient.getStruct client 1
  printf "Check log: %s\n"  $ fromJust $ f_SharedStruct_value log

  -- Close!
  tClose transport


