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

from test5.ttypes import TestEnum, TestStruct
from thrift.TSerialization import serialize, deserialize

def serialization_deserialization_enum_test():
    test_obj = TestStruct(param1="test_string", param2=TestEnum.TestEnum1)
    test_obj_serialized = serialize(test_obj)
    test_obj2 = deserialize(TestStruct(), test_obj_serialized)
    assert test_obj.param1 == test_obj2.param1
    assert test_obj.param2 == test_obj2.param2

def serialization_deserialization_string_test():
    test_obj = TestStruct(param1="test_string", param2=TestEnum.TestEnum1.name)
    test_obj_serialized = serialize(test_obj)
    test_obj2 = deserialize(TestStruct(), test_obj_serialized)
    assert test_obj.param1 == test_obj2.param1
    assert test_obj.param2 == test_obj2.param2


if __name__ == "__main__":
    serialization_deserialization_enum_test()
    serialization_deserialization_string_test