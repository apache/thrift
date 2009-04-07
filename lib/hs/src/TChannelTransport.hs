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

module TChannelTransport(TChannelTrans(..)) where

import Thrift
import Control.Exception

import System.IO
import System.IO.Error ( isEOFError )


data TChannelTrans = TChannelTrans (Handle)

instance TTransport TChannelTrans where
    tisOpen a = True
    topen a = return a
    tclose a = return a
    tread a 0 = return []
    tread (TChannelTrans h) i = Prelude.catch
                                 (do c <- hGetChar h
                                     t <- tread (TChannelTrans h) (i-1)
                                     return $ c:t)
                                 (\e -> if isEOFError e then return [] else throwDyn (TransportExn "TChannelTransport: Could not read" TE_UNKNOWN))
    twrite a [] = return ()
    twrite (TChannelTrans h) (c:t) = do hPutChar h c
                                        twrite (TChannelTrans h) t
    tflush (TChannelTrans h) = hFlush h

