#!/usr/bin/env python

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
from ThriftTest import ThriftTest
from ThriftTest.ThriftTest import Client
from ThriftTest.ttypes import Xtruct

import unittest

# only run this test if the string 'options string: py:type_hints' esxists in the file 
def has_type_hints_option():
    with open(ThriftTest.__file__) as f:
        return 'options string: py:type_hints' in f.read()

@unittest.skipUnless(has_type_hints_option(), "type hints not enabled")
class TypeAnnotationsTest(unittest.TestCase):

    def test_void(self):
        self.assertEqual(Client.testVoid.__annotations__, {'return': None})

    def test_string(self):
        self.assertEqual(Client.testString.__annotations__, {'return': str, 'thing': str})

    def test_byte(self):
        self.assertEqual(Client.testByte.__annotations__, {'return': int, 'thing': int})

    def test_i32(self):
        self.assertEqual(Client.testI32.__annotations__, {'return': int, 'thing': int})

    def test_i64(self):
        self.assertEqual(Client.testI64.__annotations__, {'return': int, 'thing': int})

    def test_double(self):
        self.assertEqual(Client.testDouble.__annotations__, {'return': float, 'thing': float})

    def test_binary(self):
        self.assertEqual(Client.testBinary.__annotations__, {'return': bytes, 'thing': bytes})

    def test_struct(self):
        self.assertEqual(Client.testStruct.__annotations__, {'return': Xtruct, 'thing': Xtruct})

    def test_map(self):
        self.assertEqual(Client.testMap.__annotations__, {'return': dict[int, int], 'thing': dict[int, int]})
    
    def test_list(self):
        self.assertEqual(Client.testList.__annotations__, {'return': list[int], 'thing': list[int]})

    def test_set(self):
        self.assertEqual(Client.testSet.__annotations__, {'return': set[int], 'thing': set[int]})
