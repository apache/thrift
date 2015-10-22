module JSONTests where

import Test.QuickCheck
import Thrift.Protocol.JSON

import Util

main :: IO ()
main = aggregateResults
  [ quickCheckResult $ propRoundTrip JSONProtocol
  , quickCheckResult $ propRoundTripMessage JSONProtocol
  ]
