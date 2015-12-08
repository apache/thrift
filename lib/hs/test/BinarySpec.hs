module BinarySpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C

import Thrift.Types
import Thrift.Transport
import Thrift.Transport.Memory
import Thrift.Protocol
import Thrift.Protocol.Binary

spec :: Spec
spec = do
  describe "BinaryProtocol" $ do
    describe "double" $ do
      it "writes in big endian order" $ do
        let val = 2 ** 53
        trans <- openMemoryBuffer
        let proto = BinaryProtocol trans
        writeVal proto (TDouble val)
        bin <- tRead trans 8
        (LBS.unpack bin) `shouldBe`[67, 64, 0, 0, 0, 0, 0, 0]

      it "reads in big endian order" $ do
        let bin = LBS.pack [67, 64, 0, 0, 0, 0, 0, 0]
        trans <- openMemoryBuffer
        let proto = BinaryProtocol trans
        tWrite trans bin
        val <- readVal proto T_DOUBLE
        val `shouldBe` (TDouble $ 2 ** 53)

      prop "round trip" $ \val -> do
        trans <- openMemoryBuffer
        let proto = BinaryProtocol trans
        writeVal proto $ TDouble val
        val2 <- readVal proto T_DOUBLE
        val2 `shouldBe` (TDouble val)

    describe "string" $ do
      it "writes" $ do
        let val = C.pack "aaa"
        trans <- openMemoryBuffer
        let proto = BinaryProtocol trans
        writeVal proto (TString val)
        bin <- tRead trans 7
        (LBS.unpack bin) `shouldBe` [0, 0, 0, 3, 97, 97, 97]
