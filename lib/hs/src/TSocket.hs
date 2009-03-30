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

module TSocket(TSocket(..)) where
import Thrift
import Data.IORef
import Network
import IO
import Control.Exception
data TSocket = TSocket{host::[Char],port::PortNumber,chan :: Maybe Handle}

instance TTransport TSocket where
    tisOpen a = case chan a of
                  Just _ -> True
                  Nothing -> False
    topen a = do h <- connectTo (host a) (PortNumber (port a))
                 return $ (a{chan = Just h})
    tclose a = case chan a of
                 Just h -> do hClose h
                              return $ a{chan=Nothing}
                 Nothing -> return a
    tread a 0 = return []
    tread a n = case chan a of
                  Just h -> handle (\e -> throwDyn (TransportExn "TSocket: Could not read." TE_UNKNOWN))
                      (do c <- hGetChar h
                          l <- tread a (n-1)
                          return $ c:l)
                  Nothing -> return []
    twrite a s = case chan a of
                   Just h -> hPutStr h s
                   Nothing -> return ()
    tflush a = case chan a of
                 Just h -> hFlush h
                 Nothing -> return ()


