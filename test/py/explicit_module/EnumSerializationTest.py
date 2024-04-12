#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

from __future__ import annotations

import sys
from shared_types.ttypes import SharedEnum
from thrift.TSerialization import serialize, deserialize
from thrift.protocol import TBinaryProtocol
from thrift.transport import TTransport

def deserialize_immutable(base,
                buf,
                protocol_factory=TBinaryProtocol.TBinaryProtocolFactory()):
    transport = TTransport.TMemoryBuffer(buf)
    protocol = protocol_factory.getProtocol(transport)
    return base.read(protocol)

def serialization_deserialization_struct_enum_test():
    test_obj = TestStruct(param1="test_string", param2=TestEnum.TestEnum1, param3=SharedEnum.SharedEnum1)
    test_obj_serialized = serialize(test_obj)
    test_obj2 = deserialize(TestStruct(), test_obj_serialized)
    assert test_obj.param1 == test_obj2.param1
    assert test_obj.param2 == test_obj2.param2
    assert test_obj.param3 == test_obj2.param3

def serialization_deserialization_struct_enum_as_string_test():
    test_obj = TestStruct(param1="test_string", param2=TestEnum.TestEnum1.name, param3=SharedEnum.SharedEnum1.name)
    test_obj_serialized = serialize(test_obj)
    test_obj2 = deserialize(TestStruct(), test_obj_serialized)
    assert test_obj.param1 == test_obj2.param1
    assert test_obj.param2 == test_obj2.param2
    assert test_obj.param3 == test_obj2.param3

def serialization_deserialization_exception_enum_as_string_test():
    test_obj = TestException(whatOp=0, why=SharedEnum.SharedEnum0.name, who=TestEnum.TestEnum0.name)
    test_obj_serialized = serialize(test_obj)
    test_obj2 = deserialize_immutable(TestException, test_obj_serialized)
    assert test_obj.whatOp == test_obj2.whatOp
    assert test_obj.why == test_obj2.why
    assert test_obj.who == test_obj2.who

def serialization_deserialization_exception_enum_test():
    test_obj = TestException(whatOp=0, why=SharedEnum.SharedEnum0, who=TestEnum.TestEnum0)
    test_obj_serialized = serialize(test_obj)
    test_obj2 = deserialize_immutable(TestException, test_obj_serialized)
    assert test_obj.whatOp == test_obj2.whatOp
    assert test_obj.why == test_obj2.why
    assert test_obj.who == test_obj2.who



if __name__ == "__main__":
    args = sys.argv[1:]
    if args:
        from test5_slots.test5.ttypes import TestEnum, TestStruct, TestException
    else:
        from test5.ttypes import TestEnum, TestStruct, TestException
    serialization_deserialization_struct_enum_test()
    serialization_deserialization_struct_enum_as_string_test()
    serialization_deserialization_exception_enum_as_string_test()
    serialization_deserialization_exception_enum_test()