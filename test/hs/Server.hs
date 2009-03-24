module Server where
import Thrift
import ThriftTest
import ThriftTest_Iface
import Data.Map as Map
import TServer
import Control.Exception
import ThriftTest_Types


data TestHandler = TestHandler
instance ThriftTest_Iface TestHandler where
    testVoid a = return ()
    testString a (Just s) = do print s; return s
    testByte a (Just x) = do print x; return x
    testI32 a (Just x) = do print x; return x
    testI64 a (Just x) = do print x; return x
    testDouble a (Just x) = do print x; return x
    testStruct a (Just x) = do print x; return x
    testNest a (Just x) = do print x; return x
    testMap a (Just x) = do print x; return x
    testSet a (Just x) = do print x; return x
    testList a (Just x) = do print x; return x
    testEnum a (Just x) = do print x; return x
    testTypedef a (Just x) = do print x; return x
    testMapMap a (Just x) = return (Map.fromList [(1,Map.fromList [(2,2)])])
    testInsanity a (Just x) = return (Map.fromList [(1,Map.fromList [(ONE,x)])])
    testMulti a a1 a2 a3 a4 a5 a6 = return (Xtruct Nothing Nothing Nothing Nothing)
    testException a c = throwDyn (Xception (Just 1) (Just "bya"))
    testMultiException a c1 c2 = return (Xtruct Nothing Nothing Nothing Nothing)
    testOneway a (Just i) = do print i


main = do (run_basic_server TestHandler process 9090) `catchDyn` (\(TransportExn s t) -> print s)
