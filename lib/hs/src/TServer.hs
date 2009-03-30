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

module TServer(run_basic_server,run_threaded_server) where

import Network
import Thrift
import Control.Exception
import TBinaryProtocol
import TChannelTransport
import Control.Concurrent

proc_loop hand proc ps = do v <-proc hand ps
                            if v then proc_loop hand proc ps
                                else return ()

accept_loop accepter hand sock proc transgen iprotgen oprotgen =
    do (h,hn,_) <- accepter sock
       let t = transgen h
       let ip = iprotgen t
       let op = oprotgen t
       forkIO (handle (\e -> return ()) (proc_loop hand proc (ip,op)))
       accept_loop accepter hand sock proc transgen iprotgen oprotgen

run_threaded_server accepter listener hand proc port transgen iprotgen oprotgen =
    do sock <- listener
       accept_loop accepter hand sock proc transgen iprotgen oprotgen
       return ()


-- A basic threaded binary protocol socket server.
run_basic_server hand proc port = run_threaded_server accept (listenOn (PortNumber port)) hand proc port TChannelTrans TBinaryProtocol TBinaryProtocol
