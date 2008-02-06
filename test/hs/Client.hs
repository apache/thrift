module Client where
import Thrift
import ThriftTest_Client
import ThriftTest_Types
import TSocket
import TBinaryProtocol
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
t = TSocket "127.0.0.1" 9090 Nothing

main = do to <- topen t
          let p =  TBinaryProtocol to
          let ps = (p,p)
          print =<< testString ps "bya"
          print =<< testByte ps 8
          print =<< testByte ps (-8)
          print =<< testI32 ps 32
          print =<< testI32 ps (-32)
          print =<< testI64 ps 64
          print =<< testI64 ps (-64)
          print =<< testDouble ps 3.14
          print =<< testDouble ps (-3.14)
          print =<< testMap ps (Map.fromList [(1,1),(2,2),(3,3)])
          print =<< testList ps [1,2,3,4,5]
          print =<< testSet ps (Set.fromList [1,2,3,4,5])
          print =<< testStruct ps (Xtruct (Just "hi") (Just 4) (Just 5) Nothing)
          tclose to

