module CompactSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Data.ByteString.Lazy as LBS

import Thrift.Types
import Thrift.Transport
import Thrift.Transport.Memory
import Thrift.Protocol
import Thrift.Protocol.Compact

spec :: Spec
spec = do
  describe "CompactProtocol" $ do
    describe "double" $ do
      it "writes in little endian order" $ do
        let val = 2 ** 53
        trans <- openMemoryBuffer
        let proto = CompactProtocol trans
        writeVal proto (TDouble val)
        bin <- tReadAll trans 8
        (LBS.unpack bin) `shouldBe`[0, 0, 0, 0, 0, 0, 64, 67]

      it "reads in little endian order" $ do
        let bin = LBS.pack [0, 0, 0, 0, 0, 0, 64, 67]
        trans <- openMemoryBuffer
        let proto = CompactProtocol trans
        tWrite trans bin
        val <- readVal proto T_DOUBLE
        val `shouldBe` (TDouble $ 2 ** 53)

      prop "round trip" $ \val -> do
        trans <- openMemoryBuffer
        let proto = CompactProtocol trans
        writeVal proto $ TDouble val
        val2 <- readVal proto T_DOUBLE
        val2 `shouldBe` (TDouble val)
