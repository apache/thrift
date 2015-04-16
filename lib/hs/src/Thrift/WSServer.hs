module Thrift.WSServer where

import qualified Network.WebSockets as WS
import Control.Monad (forever)

import Thrift.Transport.WSTrans
import Thrift.Protocol.JSON

-- Makes it easy to generate WebSocket middleware for a warp server
-- Example:
--   WS.websocketsOr WS.defaultConnectionOptions (wsHander yourHandler YourService.process)
wsHandler :: h
    -> (h -> (JSONProtocol WSTrans, JSONProtocol WSTrans) -> IO Bool)
    -> WS.ServerApp
wsHandler handler processor pconn = do
    cn <- WS.acceptRequest pconn
    WS.forkPingThread cn 30
    trns <- newWSTrans cn
    let proto = JSONProtocol trns
        inout = (proto, proto)
        doproc = processor handler
    forever (doproc inout)
    return ()
