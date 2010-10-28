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

module Thrift.Transport.Handle
    ( module Thrift.Transport
    , HandleSource(..)
    ) where

import Control.Exception ( throw )
import Control.Monad ( replicateM )

import Network

import System.IO
import System.IO.Error ( isEOFError )

import Thrift.Transport


instance Transport Handle where
    tIsOpen = hIsOpen
    tClose h    = hClose h
    tRead  h n  = replicateM n (hGetChar h) `catch` handleEOF
    tWrite h s  = mapM_ (hPutChar h) s
    tFlush = hFlush


-- | Type class for all types that can open a Handle. This class is used to
-- replace tOpen in the Transport type class.
class HandleSource s where
    hOpen :: s -> IO Handle

instance HandleSource FilePath where
    hOpen s = openFile s ReadWriteMode

instance HandleSource (HostName, PortID) where
    hOpen = uncurry connectTo


handleEOF e = if isEOFError e
    then return []
    else throw $ TransportExn "TChannelTransport: Could not read" TE_UNKNOWN
