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

{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.List.Split
import Data.String
import Network
import System.Environment
import System.Exit
import System.Posix.Unistd
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector

import ThriftTest_Iface
import ThriftTest_Types
import qualified ThriftTest_Client as Client

import Thrift.Transport
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

data Options = Options
  { host         :: String
  , port         :: Int
  , domainSocket :: String
  , transport    :: String
  , protocol     :: ProtocolType
  , ssl          :: Bool
  , testLoops    :: Int
  }
  deriving (Show, Eq)

data ProtocolType = Binary
                  | Compact
                  | JSON
                  deriving (Show, Eq)

getProtocol :: String -> ProtocolType
getProtocol "binary"  = Binary
getProtocol "compact" = Compact
getProtocol "json"    = JSON
getProtocol p = error $ "Unsupported Protocol: " ++ p

defaultOptions :: Options
defaultOptions = Options
  { port         = 9090
  , domainSocket = ""
  , host         = "localhost"
  , transport    = "framed"
  , protocol     = Binary
  , ssl          = False
  , testLoops    = 1
  }

runClient :: (Protocol p, Transport t) => p t -> IO ()
runClient p = do
  let prot = (p,p)
  putStrLn "Starting Tests"

  -- VOID Test
  Client.testVoid prot

  -- String Test
  s <- Client.testString prot "Test"
  when (s /= "Test") exitFailure

  -- Byte Test
  byte <- Client.testByte prot 1
  when (byte /= 1) exitFailure

  -- I32 Test
  i32 <- Client.testI32 prot (-1)
  when (i32 /= -1) exitFailure

  -- I64 Test
  i64 <- Client.testI64 prot (-34359738368)
  when (i64 /= -34359738368) exitFailure

  -- Double Test
  dub <- Client.testDouble prot (-5.2098523)
  when (abs (dub + 5.2098523) > 0.001) exitFailure

  -- Struct Test
  let structIn = Xtruct{ xtruct_string_thing = "Zero"
                       , xtruct_byte_thing   = 1
                       , xtruct_i32_thing    = -3
                       , xtruct_i64_thing    = -5
                       }
  structOut <- Client.testStruct prot structIn
  when (structIn /= structOut) exitFailure

  -- Nested Struct Test
  let nestIn = Xtruct2{ xtruct2_byte_thing   = 1
                      , xtruct2_struct_thing = structIn
                      , xtruct2_i32_thing    = 5
                      }
  nestOut <- Client.testNest prot nestIn
  when (nestIn /= nestOut) exitSuccess

  -- Map Test
  let mapIn = Map.fromList $ map (\i -> (i, i-10)) [1..5]
  mapOut <- Client.testMap prot mapIn
  when (mapIn /= mapOut) exitSuccess

  -- Set Test
  let setIn = Set.fromList [-2..3]
  setOut <- Client.testSet prot setIn
  when (setIn /= setOut) exitFailure

  -- List Test
  let listIn = Vector.fromList [-2..3]
  listOut <- Client.testList prot listIn
  when (listIn /= listOut) exitFailure

  -- Enum Test
  numz1 <- Client.testEnum prot ONE
  when (numz1 /= ONE) exitFailure

  numz2 <- Client.testEnum prot TWO
  when (numz2 /= TWO) exitFailure

  numz5 <- Client.testEnum prot FIVE
  when (numz5 /= FIVE) exitFailure

  -- Typedef Test
  uid <- Client.testTypedef prot 309858235082523
  when (uid /= 309858235082523) exitFailure

  -- Nested Map Test
  _ <- Client.testMapMap prot 1

  -- Exception Test
  exn1 <- try $ Client.testException prot "Xception"
  case exn1 of
    Left (Xception _ _) -> return ()
    _ -> putStrLn (show exn1) >> exitFailure

  exn2 <- try $ Client.testException prot "TException"
  case exn2 of
    Left (_ :: SomeException) -> return ()
    Right _ -> exitFailure

  exn3 <- try $ Client.testException prot "success"
  case exn3 of
    Left (_ :: SomeException) -> exitFailure
    Right _ -> return ()

  -- Multi Exception Test
  multi1 <- try $ Client.testMultiException prot "Xception" "test 1"
  case multi1 of
    Left (Xception _ _) -> return ()
    _ -> exitFailure

  multi2 <- try $ Client.testMultiException prot "Xception2" "test 2"
  case multi2 of
    Left (Xception2 _ _) -> return ()
    _ -> exitFailure

  multi3 <- try $ Client.testMultiException prot "success" "test 3"
  case multi3 of
    Left (_ :: SomeException) -> exitFailure
    Right _ -> return ()


main :: IO ()
main = do
  options <- flip parseFlags defaultOptions <$> getArgs
  case options of
    Nothing -> showHelp
    Just Options{..} -> do
      handle <- hOpen (host, PortNumber $ fromIntegral port)
      let client = case protocol of
            Binary  -> runClient $ BinaryProtocol handle
            Compact -> runClient $ CompactProtocol handle
            JSON    -> runClient $ JSONProtocol handle
      replicateM_ testLoops client
      putStrLn "COMPLETED SUCCESSFULLY"

parseFlags :: [String] -> Options -> Maybe Options
parseFlags (flag : flags) opts = do
  let pieces = splitOn "=" flag
  case pieces of
    "--port" : arg : _ -> parseFlags flags opts{ port = read arg }
    "--domain-socket" : arg : _ -> parseFlags flags opts{ domainSocket = read arg }
    "--host" : arg : _ -> parseFlags flags opts{ host = arg }
    "--transport" : arg : _ -> parseFlags flags opts{ transport = arg }
    "--protocol" : arg : _ -> parseFlags flags opts{ protocol = getProtocol arg }
    "-n" : arg : _ -> parseFlags flags opts{ testLoops = read arg }
    "--h" : _ -> Nothing
    "--help" : _ -> Nothing
    "--ssl" : _ -> parseFlags flags opts{ ssl = True }
    "--processor-events" : _ -> parseFlags flags opts
    _ -> Nothing
parseFlags [] opts = Just opts

showHelp :: IO ()
showHelp = putStrLn
  "Allowed options:\n\
  \  -h [ --help ]               produce help message\n\
  \  --host arg (=localhost)     Host to connect\n\
  \  --port arg (=9090)          Port number to connect\n\
  \  --domain-socket arg         Domain Socket (e.g. /tmp/ThriftTest.thrift),\n\
  \                              instead of host and port\n\
  \  --transport arg (=buffered) Transport: buffered, framed, http, evhttp\n\
  \  --protocol arg (=binary)    Protocol: binary, compact, json\n\
  \  --ssl                       Encrypted Transport using SSL\n\
  \  -n [ --testloops ] arg (=1) Number of Tests"