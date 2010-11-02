{-# LANGUAGE FlexibleInstances #-}
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

module Thrift.Transport.HttpClient
    ( module Thrift.Transport
    , HttpClient (..)
    , openHttpClient
    ) where

import Thrift.Transport
import Network.URI
import Network.HTTP hiding (port, host)

import Control.Monad (liftM)
import Data.Maybe (fromJust)
import Data.Monoid (mappend, mempty)
import Control.Exception (throw)
import Control.Concurrent.MVar
import qualified Data.Binary.Builder as B
import qualified Data.ByteString.Lazy as LBS


-- | 'HttpClient', or THttpClient implements the Thrift Transport
-- | Layer over http or https.
data HttpClient =
    HttpClient {
      hstream :: HandleStream LBS.ByteString,
      uri :: URI,
      writeBuffer :: WriteBuffer,
      readBuffer :: ReadBuffer
    }

uriAuth :: URI -> URIAuth
uriAuth = fromJust . uriAuthority

host :: URI -> String
host = uriRegName . uriAuth

port :: URI -> Int
port uri_ =
    if portStr == mempty then
        httpPort
    else
        read portStr
    where
      portStr = dropWhile (== ':') $ uriPort $ uriAuth uri_
      httpPort = 80

-- | Use 'openHttpClient' to create an HttpClient connected to @uri@
openHttpClient :: URI -> IO HttpClient
openHttpClient uri_ = do
  stream <- openTCPConnection (host uri_) (port uri_)
  wbuf <- newWriteBuffer
  rbuf <- newReadBuffer
  return $ HttpClient stream uri_ wbuf rbuf

instance Transport HttpClient where

    tClose  = close . hstream

    tRead hclient n = readBuf (readBuffer hclient) n

    tWrite hclient = writeBuf (writeBuffer hclient)

    tFlush hclient = do
      body <- flushBuf $ writeBuffer hclient
      let request = Request {
                      rqURI = uri hclient,
                      rqHeaders = [
                       mkHeader HdrContentType "application/x-thrift",
                       mkHeader HdrContentLength $  show $ LBS.length body],
                      rqMethod = POST,
                      rqBody = body
                    }

      res <- sendHTTP (hstream hclient) request
      case res of
        Right response -> do
            fillBuf (readBuffer hclient) (rspBody response)
        Left _ -> do
            throw $ TransportExn "THttpConnection: HTTP failure from server" TE_UNKNOWN
      return ()

    tIsOpen _ = return True
-- Mini IO buffers

type WriteBuffer = MVar (B.Builder)

newWriteBuffer :: IO WriteBuffer
newWriteBuffer = newMVar mempty

writeBuf :: WriteBuffer -> LBS.ByteString -> IO ()
writeBuf w s = modifyMVar_ w $ return . (\builder ->
                 builder `mappend` (B.fromLazyByteString s))

flushBuf :: WriteBuffer -> IO (LBS.ByteString)
flushBuf w = B.toLazyByteString `liftM` swapMVar w mempty


type ReadBuffer = MVar (LBS.ByteString)

newReadBuffer :: IO ReadBuffer
newReadBuffer = newMVar mempty

fillBuf :: ReadBuffer -> LBS.ByteString -> IO ()
fillBuf r s = swapMVar r s >> return ()

readBuf :: ReadBuffer -> Int -> IO (LBS.ByteString)
readBuf r n = modifyMVar r $ return . flipPair . LBS.splitAt (fromIntegral n)
    where flipPair (a, b) = (b, a)
