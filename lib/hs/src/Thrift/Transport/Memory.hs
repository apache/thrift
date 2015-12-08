module Thrift.Transport.Memory
       ( openMemoryBuffer
       , MemoryBuffer(..)
       ) where

import Data.ByteString.Lazy.Builder
import Data.Functor
import Data.IORef
import Data.Monoid
import qualified Data.ByteString.Lazy as LBS

import Thrift.Transport


data MemoryBuffer = MemoryBuffer {
  writeBuffer :: IORef Builder,
  readBuffer :: IORef LBS.ByteString
}

openMemoryBuffer :: IO MemoryBuffer
openMemoryBuffer = do
  wbuf <- newIORef mempty
  rbuf <- newIORef mempty
  return MemoryBuffer {
    writeBuffer = wbuf,
    readBuffer = rbuf
  }

instance Transport MemoryBuffer where
  tIsOpen = const $ return False
  tClose  = const $ return ()
  tFlush trans = do
    let wBuf = writeBuffer trans
    wb <- readIORef wBuf
    modifyIORef (readBuffer trans) $ \rb -> mappend rb $ toLazyByteString wb
    writeIORef wBuf mempty

  tRead _ 0 = return mempty
  tRead trans n = do
    let rbuf = readBuffer trans
    rb <- readIORef rbuf
    let len = fromIntegral $ LBS.length rb
    if len == 0
      then do
        tFlush trans
        rb2 <- readIORef (readBuffer trans)
        if (fromIntegral $ LBS.length rb2) == 0
          then return mempty
          else tRead trans n
      else do
        let (ret, remain) = LBS.splitAt (fromIntegral n) rb
        writeIORef rbuf remain
        return ret

  tPeek trans = (fmap fst . LBS.uncons) <$> readIORef (readBuffer trans)

  tWrite trans v = do
    modifyIORef (writeBuffer trans) (<> lazyByteString v)
