module BinaryTests where

import Test.QuickCheck
import Thrift.Protocol.Binary

import Util

main :: IO ()
main = aggregateResults
       [ quickCheckResult $ propRoundTrip BinaryProtocol
       , quickCheckResult $ propRoundTripMessage BinaryProtocol
       ]
