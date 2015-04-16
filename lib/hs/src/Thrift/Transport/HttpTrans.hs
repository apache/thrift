{-# LANGUAGE OverloadedStrings #-}

module Thrift.Transport.HttpTrans where

import qualified Network.Wai               as W

import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy      as L
import           Network.HTTP.Types

import           Thrift
import           Thrift.Transport.Handle   ()
import           Thrift.Transport.IOBuffer

type DoResponse = (W.Response -> IO W.ResponseReceived)

data HttpTrans =
    HttpTrans {
         reqBuffer :: ReadBuffer
        ,resBuffer :: WriteBuffer
        ,respond   :: DoResponse
    }

newHttpTrans :: W.Request -> DoResponse -> IO HttpTrans
newHttpTrans request doresp = do
    rbuf <- newReadBuffer
    rbody <- W.strictRequestBody request
    fillBuf rbuf rbody
    wbuf <- newWriteBuffer
    return $ HttpTrans rbuf wbuf doresp

instance Transport HttpTrans where
    tIsOpen _ = return True
    tClose _ = return ()
    tPeek  = peekBuf . reqBuffer
    tRead  = readBuf . reqBuffer
    tWrite = writeBuf . resBuffer
    tFlush t = do
        body <- flushBuf $ resBuffer t
        respond t $ W.responseLBS
            status200
            [("Content-Type","application/x-thrift"),
             ("Content-Length", pack $ show $ L.length body) ]
            body
        return ()
