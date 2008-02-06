module TChannelTransport(TChannelTrans(..)) where
import System.IO
import IO
import Thrift
import Control.Exception
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

