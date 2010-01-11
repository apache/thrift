--
-- Licensed to the Apache Software Foundation (ASF) under one
-- or more contributor license agreements. See the NOTICE file
-- distributed with this work for additional information
-- regarding copyright ownership. The ASF licenses this file
-- to you under the Apache License, Version 2.0 (the
-- "License"); you may not use this file except in compliance
-- with the License. You may obtain a copy of the License at
--
--   http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing,
-- software distributed under the License is distributed on an
-- "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
-- KIND, either express or implied. See the License for the
-- specific language governing permissions and limitations
-- under the License.
--

import qualified Calculator
import Calculator_Iface
import Tutorial_Types
import SharedService_Iface
import Shared_Types

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Server

import Data.Maybe
import Text.Printf
import Control.Exception (throw)
import Control.Concurrent.MVar
import qualified Data.Map as M
import Data.Map ((!))
import Data.Monoid

data CalculatorHandler = CalculatorHandler {mathLog :: MVar (M.Map Int SharedStruct)}

newCalculatorHandler = do
  log <- newMVar mempty
  return $ CalculatorHandler log

instance SharedService_Iface CalculatorHandler where
  getStruct self k = do
    myLog <- readMVar (mathLog self)
    return $ (myLog ! (fromJust k))


instance Calculator_Iface CalculatorHandler where
  ping _ =
    print "ping()"

  add _ n1 n2 = do
    printf "add(%d,%d)\n" (fromJust n1) (fromJust n2)
    return ((fromJust n1)+(fromJust n2))

  calculate self mlogid mwork = do
    printf "calculate(%d, %s)\n" logid (show work)

    let val = case op work of
                ADD ->
                    num1 work + num2 work
                SUBTRACT ->
                    num1 work - num2 work
                MULTIPLY ->
                    num1 work * num2 work
                DIVIDE ->
                    if num2 work == 0 then
                        throw $
                              InvalidOperation {
                                 f_InvalidOperation_what = Just $ fromEnum $ op work,
                                 f_InvalidOperation_why = Just "Cannot divide by 0"
                                            }
                    else
                        num1 work `div` num2 work

    let logEntry = SharedStruct (Just logid) (Just (show val))
    modifyMVar_ (mathLog self) $ return .(M.insert logid logEntry)

    return val

   where
     -- stupid dynamic languages f'ing it up
     num1 = fromJust . f_Work_num1
     num2 = fromJust . f_Work_num2
     op = fromJust . f_Work_op
     logid = fromJust mlogid
     work = fromJust mwork


    --return val

  zip _ =
    print "zip()"

main =  do
  handler <- newCalculatorHandler
  print "Starting the server..."
  runBasicServer handler Calculator.process 9090
  print "done."
