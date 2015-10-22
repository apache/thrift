{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
       ( TestTransport(..)
       , propRoundTrip
       , propRoundTripMessage
       , aggregateResults
       ) where

import Control.Monad
import Data.Functor
import Data.Int
import Data.IORef
import Data.Monoid
import Prelude
import System.Exit
import Test.QuickCheck as QC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT

import Thrift.Arbitraries ()
import Thrift.Protocol
import Thrift.Transport
import Thrift.Transport.Framed
import Thrift.Types

import Hs_test_Types

data TestTransport = TestTransport (IORef LBS.ByteString)

instance Transport TestTransport where
  tIsOpen _ = return True
  tClose _ = return ()
  tPeek (TestTransport t) = (fmap fst . LBS.uncons) <$> readIORef t
  tRead (TestTransport t) i = do
    s <- readIORef t
    let (hd, tl) = LBS.splitAt (fromIntegral i) s
    writeIORef t tl
    return hd
  tWrite (TestTransport t) bs = modifyIORef' t (<> bs)
  tFlush _ = return ()

propRoundTrip :: Protocol p
              => (FramedTransport TestTransport
                  -> p (FramedTransport TestTransport))
              -> TestStruct
              -> Property
propRoundTrip pcons cf = ioProperty $ do
  ref <- newIORef ""
  t <- openFramedTransport (TestTransport ref)
  let p = pcons t
  write_TestStruct p cf
  tFlush t
  (==cf) <$> read_TestStruct p

propRoundTripMessage :: Protocol p
                     => (TestTransport -> p TestTransport)
                     -> (LT.Text, MessageType, Int32)
                     -> Property
propRoundTripMessage pcons args = ioProperty $ do
  ref <- newIORef ""
  let p = pcons (TestTransport ref)
  writeMessage p args (return ())
  (==args) <$> readMessage p return

aggregateResults :: [IO QC.Result] -> IO ()
aggregateResults qcs = do
  results <- sequence qcs
  if all successful results
    then exitSuccess
    else exitFailure
  where
    successful Success{} = True
    successful _ = False
