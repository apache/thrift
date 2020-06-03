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

module Thrift.Server
    ( runBasicServer
    , runThreadedServer
    ) where

import Control.Concurrent ( forkIO )
import Control.Exception
import Control.Monad ( forever, when )

import Network.Socket

import System.IO

import Thrift
import Thrift.Transport.Handle()
import Thrift.Protocol.Binary


-- | A threaded sever that is capable of using any Transport or Protocol
-- instances.
runThreadedServer :: (Protocol i, Protocol o)
                  => (Socket -> IO (i, o))
                  -> h
                  -> (h -> (i, o) -> IO Bool)
                  -> Int
                  -> IO a
runThreadedServer accepter hand proc_ port = do
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          }
    addr <- head <$> getAddrInfo (Just hints) Nothing (Just $ show port)

    bracket (open addr) close $ \sock -> do
      acceptLoop (accepter sock) (proc_ hand)
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      fd <- fdSocket sock
      setCloseOnExecIfNeeded fd
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

-- | A basic threaded binary protocol socket server.
runBasicServer :: h
               -> (h -> (BinaryProtocol Handle, BinaryProtocol Handle) -> IO Bool)
               -> Int
               -> IO a
runBasicServer hand proc_ port = runThreadedServer binaryAccept hand proc_ port
  where binaryAccept s = do
            (aSock, _) <- accept s
            h <- socketToHandle aSock ReadWriteMode
            return (BinaryProtocol h, BinaryProtocol h)

acceptLoop :: IO t -> (t -> IO Bool) -> IO a
acceptLoop accepter proc_ = forever $
    do ps <- accepter
       forkIO $ handle (\(_ :: SomeException) -> return ())
                  (loop $ proc_ ps)
  where loop m = do { continue <- m; when continue (loop m) }
