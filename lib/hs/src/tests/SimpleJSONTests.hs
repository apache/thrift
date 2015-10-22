module SimpleJSONTests where

import Test.QuickCheck
import Thrift.Protocol.SimpleJSON

import Util

main :: IO ()
main = aggregateResults
  [ quickCheckResult $ propRoundTrip SimpleJSONProtocol
  , quickCheckResult $ propRoundTripMessage SimpleJSONProtocol
  ]
