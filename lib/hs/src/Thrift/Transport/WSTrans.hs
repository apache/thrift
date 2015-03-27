module Thrift.Transport.WSTrans where

import           Data.ByteString.Lazy          as BL hiding (pack)
import           Data.IORef
import           Data.Monoid
import           Data.Text                     (pack)
import           Prelude                       hiding (head, read)

import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Connection as WS

import           Thrift
import           Thrift.Transport.IOBuffer

data WSTrans = WSTrans {
     conn      :: WS.Connection
    ,reqBuffer :: ReadBuffer
    ,resBuffer :: WriteBuffer
}

toByteString :: WS.DataMessage -> BL.ByteString
toByteString dm = case dm of
    (WS.Text bs) -> bs
    (WS.Binary bs) -> bs

newWSTrans :: WS.Connection -> IO WSTrans
newWSTrans conn = do
    wbuf <- newWriteBuffer
    rbuf <- newReadBuffer
    msg <- WS.receiveDataMessage conn
    fillBuf rbuf (toByteString msg)
    return $ WSTrans conn rbuf wbuf

readUntil :: WS.Connection -> Int -> BL.ByteString -> IO BL.ByteString
readUntil conn n s
    | sz > 1 && nsz < sz = return s
    | otherwise          = do
        msg <- WS.receiveDataMessage conn
        let ns = s <> toByteString msg
        if BL.length ns < fromIntegral n
            then readUntil conn n ns
            else return ns
    where
        sz = BL.length s
        nsz = fromIntegral n

instance Transport WSTrans where
    tIsOpen t = do
        stream <- readIORef $ (WS.connectionStream . conn) t
        case stream of
            WS.Closed -> return False
            _         -> return True
    tClose t = WS.sendClose (conn t) (pack "Closing ws...")
    tRead t n = do
        bs <- readIORef (reqBuffer t)
        fullbuff <- readUntil (conn t) n bs
        let (hd, tl) = BL.splitAt (fromIntegral n) fullbuff
        writeIORef (reqBuffer t) tl
        return hd
    tPeek = peekBuf . reqBuffer
    tWrite = writeBuf . resBuffer
    tFlush t = do
        body <- flushBuf $ resBuffer t
        WS.sendDataMessage (conn t) (WS.Text body)
