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

{-# LANGUAGE OverloadedStrings,RecordWildCards #-}
module Main where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.String
import Network
import System.Environment
import System.Exit
import System.IO
import System.Posix.Unistd
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Text.Lazy as Text
import qualified Data.Vector as Vector

import ThriftTest
import ThriftTest_Iface
import ThriftTest_Types

import Thrift
import Thrift.Server
import Thrift.Transport.Framed
import Thrift.Transport.Handle
import Thrift.Protocol.Binary
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

data Options = Options
  { port         :: Int
  , domainSocket :: String
  , serverType   :: ServerType
  , transport    :: String
  , protocol     :: ProtocolType
  , ssl          :: Bool
  , workers      :: Int
  }
  
data ServerType = Simple
                | ThreadPool
                | Threaded
                | NonBlocking
                deriving (Show, Eq)

instance IsString ServerType where
  fromString "simple"      = Simple
  fromString "thread-pool" = ThreadPool
  fromString "threaded"    = Threaded
  fromString "nonblocking" = NonBlocking
  fromString _ = error "not a valid server type"

data ProtocolType = Binary
                  | Compact
                  | JSON

getProtocol :: String -> ProtocolType
getProtocol "binary"  = Binary
getProtocol "compact" = Compact
getProtocol "json"    = JSON
getProtocol p = error $"Unsupported Protocol: " ++ p

defaultOptions :: Options
defaultOptions = Options
  { port         = 9090
  , domainSocket = ""
  , serverType   = Threaded
  , transport    = "framed"
  , protocol     = Binary
  , ssl          = False
  , workers      = 4
  }

stringifyMap :: (Show a, Show b) => Map.HashMap a b -> String
stringifyMap = intercalate ", " . map joinKV . Map.toList
  where joinKV (k, v) = show k ++ " => " ++ show v

stringifySet :: Show a => Set.HashSet a -> String
stringifySet = intercalate ", " . map show . Set.toList

stringifyList :: Show a => Vector.Vector a -> String
stringifyList = intercalate ", " . map show . Vector.toList

data TestHandler = TestHandler
instance ThriftTest_Iface TestHandler where  
  testVoid _ = putStrLn "testVoid()"

  testString _ s = do
    putStrLn $ "testString(" ++ show s ++ ")"
    return s

  testByte _ x = do
    putStrLn $ "testByte(" ++ show x ++ ")"
    return x

  testI32 _ x = do
    putStrLn $ "testI32(" ++ show x ++ ")"
    return x

  testI64 _ x = do
    putStrLn $ "testI64(" ++ show x ++ ")"
    return x
    
  testDouble _ x = do
    putStrLn $ "testDouble(" ++ show x ++ ")"
    return x

  testStruct _ struct@Xtruct{..} = do
    putStrLn $ "testStruct({" ++ show xtruct_string_thing
                      ++ ", " ++ show xtruct_byte_thing 
                      ++ ", " ++ show xtruct_i32_thing
                      ++ ", " ++ show xtruct_i64_thing
                      ++ "})"
    return struct

  testNest _ nest@Xtruct2{..} = do
    let Xtruct{..} = xtruct2_struct_thing
    putStrLn $ "testNest({" ++ show xtruct2_byte_thing
                   ++ "{, " ++ show xtruct_string_thing
                   ++  ", " ++ show xtruct_byte_thing
                   ++  ", " ++ show xtruct_i32_thing
                   ++  ", " ++ show xtruct_i64_thing
                   ++ "}, " ++ show xtruct2_i32_thing
    return nest

  testMap _ m = do
    putStrLn $ "testMap({" ++ stringifyMap m ++ "})"
    return m
            
  testStringMap _ m = do
    putStrLn $ "testStringMap(" ++ stringifyMap m ++ "})"
    return m

  testSet _ x = do
    putStrLn $ "testSet({" ++ stringifySet x ++ "})"
    return x

  testList _ x = do
    putStrLn $ "testList(" ++ stringifyList x ++ "})"
    return x

  testEnum _ x = do
    putStrLn $ "testEnum(" ++ show x ++ ")"
    return x

  testTypedef _ x = do
    putStrLn $ "testTypedef(" ++ show x ++ ")"
    return x

  testMapMap _ x = do
    putStrLn $ "testMapMap(" ++ show x ++ ")"
    return $ Map.fromList [ (-4, Map.fromList [ (-4, -4)
                                              , (-3, -3)
                                              , (-2, -2)
                                              , (-1, -1)
                                              ])
                          , (4,  Map.fromList [ (1, 1)
                                              , (2, 2)
                                              , (3, 3)
                                              , (4, 4)
                                              ])
                          ]

  testInsanity _ x = do
    putStrLn "testInsanity()"
    return $ Map.fromList [ (1, Map.fromList [ (TWO  , x)
                                             , (THREE, x)
                                             ])
                          , (2, Map.fromList [ (SIX, default_Insanity)
                                             ])
                          ]

  testMulti _ byte i32 i64 _ _ _ = do
    putStrLn "testMulti()"
    return Xtruct{ xtruct_string_thing = Text.pack "Hello2"
                 , xtruct_byte_thing   = byte
                 , xtruct_i32_thing    = i32
                 , xtruct_i64_thing    = i64
                 }
                                        
  testException _ s = do
    putStrLn $ "testException(" ++ show s ++ ")"
    case s of
      "Xception"   -> throw $ Xception 1001 s
      "TException" -> throw ThriftException
      _ -> return ()

  testMultiException _ s1 s2 = do
    putStrLn $ "testMultiException(" ++ show s1 ++ ", " ++ show s2 ++  ")"
    case s1 of
      "Xception"   -> throw $ Xception 1001 "This is an Xception" 
      "Xception2"  -> throw $ Xception2 2002 default_Xtruct 
      "TException" -> throw ThriftException
      _ -> return default_Xtruct{ xtruct_string_thing = s2 }

  testOneway _ i = do
    putStrLn $ "testOneway(" ++ show i ++ "): Sleeping..."
    sleep (fromIntegral i)
    putStrLn $ "testOneway(" ++ show i ++ "): done sleeping!"

main :: IO ()
main = do
  options <- flip parseFlags defaultOptions <$> getArgs
  case options of
    Nothing -> showHelp
    Just Options{..} -> do
      putStrLn $ "Starting \"" ++ show serverType ++ "\" server (" ++
        show transport ++ ") listen on: " ++ domainSocket ++ show port
      case protocol of
        Binary  -> runServer BinaryProtocol port
        Compact -> runServer CompactProtocol port
        JSON    -> runServer JSONProtocol port
      where
        runServer p = runThreadedServer (accepter p) TestHandler ThriftTest.process . PortNumber . fromIntegral
        accepter p s = do
          (h, _, _) <- accept s
          return (p h, p h)

parseFlags :: [String] -> Options -> Maybe Options
parseFlags (flag : arg : flags) opts
  | flag == "--port"          = parseFlags flags opts{ port = read arg }
  | flag == "--domain-socket" = parseFlags flags opts{ domainSocket = arg }
  | flag == "--server-type"   = parseFlags flags opts{ serverType = fromString arg }
  | flag == "--transport"     = parseFlags flags opts{ transport = arg }
  | flag == "--protocol"      = parseFlags flags opts{ protocol = getProtocol arg }
  | flag == "-n" ||
    flag == "--workers"       = parseFlags flags opts{ workers = read arg }
parseFlags (flag : flags) opts
  | flag == "-h"     = Nothing
  | flag == "--help" = Nothing
  | flag == "--ssl"  = parseFlags flags opts{ ssl = True }
  | flag == "--processor-events" = parseFlags flags opts
parseFlags [] opts = Just opts

showHelp :: IO ()
showHelp = putStrLn
  "Allowed options:\n\
  \  -h [ --help ]               produce help message\n\
  \  --port arg (=9090)          Port number to listen\n\
  \  --domain-socket arg         Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)\n\
  \  --server-type arg (=simple) type of server, \"simple\", \"thread-pool\",\n\
  \                              \"threaded\", or \"nonblocking\"\n\
  \  --transport arg (=buffered) transport: buffered, framed, http\n\
  \  --protocol arg (=binary)    protocol: binary, compact, json\n\
  \  --ssl                       Encrypted Transport using SSL\n\
  \  --processor-events          processor-events\n\
  \  -n [ --workers ] arg (=4)   Number of thread pools workers. Only valid for\n\ 
  \                              thread-pool server type"