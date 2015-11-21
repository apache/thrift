module IncludeTest where

import Test.QuickCheck

import IncludeTest_Types
import IncludedTest_Types

import Util

propToFoo = foo == roundTrip foo
  where foo = Foo { foo_Bar = 5, foo_Baz = 10 }
        roundTrip = to_Foo . from_Foo

propDefaultFoo = default_Foo { foo_Baz = 1 } == bar_baz default_Bar

main :: IO ()
main = aggregateResults $ fmap quickCheckResult [propToFoo, propDefaultFoo]
