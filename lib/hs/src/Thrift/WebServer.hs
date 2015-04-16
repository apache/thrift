{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Thrift.WebServer where

import           Data.ByteString.Char8

import           Network.HTTP.Types

import qualified Network.Wai                as W
import           Network.Wai.Internal

import           Thrift.Protocol.JSON
import           Thrift.Transport.HttpTrans

-- | Run with Network.Wai.Handler.Warp -> run 8080 app
basicWebServerApp :: h
    -> (h -> (JSONProtocol HttpTrans, JSONProtocol HttpTrans) -> IO Bool)
    -> W.Application
basicWebServerApp handler processor = \req respond -> let method = W.requestMethod req in if
        -- methodGet - Leave unimplemented for static middleware
        -- websocket - use websocketOr middleware with WSServer
    | method == methodPost    -> do
        htrans <- newHttpTrans req respond
        let proto = JSONProtocol htrans
            inout = (proto,proto)
        proc_ inout
        return ResponseReceived
    | method == methodOptions -> respond $ W.responseLBS status204 [("Content-Length", pack "0")] "No Content"
    | otherwise               -> respond $ W.responseLBS status403 [] "Not sure what's going on here"
    where
        proc_ = processor handler
