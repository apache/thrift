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

module DebugProtoTest_TestServer where


import Control.Exception
import qualified Data.ByteString.Lazy as DBL
import Maybe

import Thrift
import Thrift.Server

import DebugProtoTest_Types
import Inherited
import Inherited_Iface
import Srv_Iface


data InheritedHandler = InheritedHandler
instance Srv_Iface InheritedHandler where
    janky _ arg = do
        print $ "Got janky method call: " ++ show arg
        return $ 31

    voidMethod _ = do
        print "Got voidMethod method call"
        return ()

    primitiveMethod _ = do
        print "Got primitiveMethod call"
        return $ 42

    structMethod _ = do
        print "Got structMethod call"
        return $ CompactProtoTestStruct {
            f_CompactProtoTestStruct_a_byte = Just 0x01,
            f_CompactProtoTestStruct_a_i16 = Just 0x02,
            f_CompactProtoTestStruct_a_i32 = Just 0x03,
            f_CompactProtoTestStruct_a_i64 = Just 0x04,
            f_CompactProtoTestStruct_a_double = Just 0.1,
            f_CompactProtoTestStruct_a_string = Just "abcdef",
            f_CompactProtoTestStruct_a_binary = Just DBL.empty,
            f_CompactProtoTestStruct_true_field = Just True,
            f_CompactProtoTestStruct_false_field = Just False,
            f_CompactProtoTestStruct_empty_struct_field = Just Empty,
            
            f_CompactProtoTestStruct_byte_list = Nothing,
            f_CompactProtoTestStruct_i16_list = Nothing,
            f_CompactProtoTestStruct_i32_list = Nothing,
            f_CompactProtoTestStruct_i64_list = Nothing,
            f_CompactProtoTestStruct_double_list = Nothing,
            f_CompactProtoTestStruct_string_list = Nothing,
            f_CompactProtoTestStruct_binary_list = Nothing,
            f_CompactProtoTestStruct_boolean_list = Nothing,
            f_CompactProtoTestStruct_struct_list = Just [Empty],

            f_CompactProtoTestStruct_byte_set = Nothing,
            f_CompactProtoTestStruct_i16_set = Nothing,
            f_CompactProtoTestStruct_i32_set = Nothing,
            f_CompactProtoTestStruct_i64_set = Nothing,
            f_CompactProtoTestStruct_double_set = Nothing,
            f_CompactProtoTestStruct_string_set = Nothing,
            f_CompactProtoTestStruct_binary_set = Nothing,
            f_CompactProtoTestStruct_boolean_set = Nothing,
            f_CompactProtoTestStruct_struct_set = Nothing,

            f_CompactProtoTestStruct_byte_byte_map = Nothing,
            f_CompactProtoTestStruct_i16_byte_map = Nothing,
            f_CompactProtoTestStruct_i32_byte_map = Nothing,
            f_CompactProtoTestStruct_i64_byte_map = Nothing,
            f_CompactProtoTestStruct_double_byte_map = Nothing,
            f_CompactProtoTestStruct_string_byte_map = Nothing,
            f_CompactProtoTestStruct_binary_byte_map = Nothing,
            f_CompactProtoTestStruct_boolean_byte_map = Nothing,

            f_CompactProtoTestStruct_byte_i16_map = Nothing,
            f_CompactProtoTestStruct_byte_i32_map = Nothing,
            f_CompactProtoTestStruct_byte_i64_map = Nothing,
            f_CompactProtoTestStruct_byte_double_map = Nothing,
            f_CompactProtoTestStruct_byte_string_map = Nothing,
            f_CompactProtoTestStruct_byte_binary_map = Nothing,
            f_CompactProtoTestStruct_byte_boolean_map = Nothing,

            f_CompactProtoTestStruct_list_byte_map = Nothing,
            f_CompactProtoTestStruct_set_byte_map = Nothing,
            f_CompactProtoTestStruct_map_byte_map = Nothing,

            f_CompactProtoTestStruct_byte_map_map = Nothing,
            f_CompactProtoTestStruct_byte_set_map = Nothing,
            f_CompactProtoTestStruct_byte_list_map = Nothing }

    methodWithDefaultArgs _ arg = do
        print $ "Got methodWithDefaultArgs: " ++ show arg
        return ()

    onewayMethod _ = do
        print "Got onewayMethod"

instance Inherited_Iface InheritedHandler where
    identity _ arg = do
        print $ "Got identity method: " ++ show arg
        return $ fromJust arg

main :: IO ()
main = do putStrLn "Server ready..."
          (runBasicServer InheritedHandler process 9090)
          `Control.Exception.catch`
          (\(TransportExn s _) -> print s)
