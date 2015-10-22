module CompactTests where

import Test.QuickCheck
import Thrift.Protocol.Compact

import Util

main :: IO ()
main = aggregateResults
       [ quickCheckResult $ propRoundTrip CompactProtocol
       , quickCheckResult $ propRoundTripMessage CompactProtocol
       ]
