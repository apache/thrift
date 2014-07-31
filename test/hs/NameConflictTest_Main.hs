{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main where

import qualified Prelude as P

import NameConflictTest_Consts
import NameConflictTest_Types
import Qualified
import Qualified_Client
import Qualified_Iface
import Extern
import Extern_Client
import Extern_Iface

main :: P.IO ()
main = do
  P.putStrLn "Values:"
  P.print ([JUST, TRUE, FALSE] :: [Maybe])
  P.print ([LEFT, RIGHT] :: [Either])
  P.print (Problem_ P.True P.False)
