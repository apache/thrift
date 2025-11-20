/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

namespace go validatetest

enum EnumFoo {
  e1
  e2
}

struct Foo {
        1: bool Bool
}

struct BasicTest {
        1: bool Bool0 = true (vt.const = "true")
        2: optional bool Bool1 (vt.const = "true")
        3: i8 Byte0 = 1 (vt.lt = "2", vt.le = "2", vt.gt = "0", vt.ge = "0", vt.in = "[0, 1, 2]", vt.not_in = "[3, 4, 5]")
        4: optional i8 Byte1 (vt.lt = "1", vt.le = "1", vt.gt = "-1", vt.ge = "-1", vt.in = "[-1, 0, 1]", vt.not_in = "[1, 2, 3]")
        5: double Double0 = 1.0 (vt.lt = "2.0", vt.le = "2.0", vt.gt = "0", vt.ge = "0", vt.in = "[0, 1.0, 2.0]", vt.not_in = "[3.0, 4.0, 5.0]")
        6: optional double Double1 (vt.lt = "2.0", vt.le = "2.0", vt.gt = "0", vt.ge = "0", vt.in = "[0, 1.0, 2.0]", vt.not_in = "[3.0, 4.0, 5.0]")
        7: string String0 = "my const string" (vt.const = "my const string", vt.min_size = "0", vt.max_size = "100", vt.pattern = ".*", vt.prefix = "my", vt.suffix = "string", vt.contains = "const", vt.not_contains = "oh")
        8: optional string String1 (vt.const = "my const string", vt.min_size = "0", vt.max_size = "100", vt.pattern = ".*", vt.prefix = "my", vt.suffix = "string", vt.contains = "const", vt.not_contains = "oh")
        9: binary Binary0 = "my const string" (vt.const = "my const string", vt.min_size = "0", vt.max_size = "100", vt.pattern = ".*", vt.prefix = "my", vt.suffix = "string", vt.contains = "const", vt.not_contains = "oh")
        10: optional binary Binary1 = "my const string" (vt.const = "my const string", vt.min_size = "0", vt.max_size = "100", vt.pattern = ".*", vt.prefix = "my", vt.suffix = "string", vt.contains = "const", vt.not_contains = "oh")
        11: map<string, string> Map0 (vt.min_size = "0", vt.max_size = "10", vt.key.min_size = "0", vt.key.max_size = "10", vt.value.min_size = "0", vt.value.max_size = "10")
        12: optional map<string, string> Map1 (vt.min_size = "0", vt.max_size = "10", vt.key.min_size = "0", vt.key.max_size = "10", vt.value.min_size = "0", vt.value.max_size = "10")
        13: set<string> Set0 (vt.min_size = "0", vt.max_size = "10", vt.elem.min_size = "5")
        14: optional set<string> Set1 (vt.min_size = "0", vt.max_size = "10", vt.elem.min_size = "5")
        15: EnumFoo Enum0 = EnumFoo.e2 (vt.in = "[EnumFoo.e2]", vt.defined_only = "true")
        16: optional EnumFoo Enum1 (vt.in = "[EnumFoo.e1]", vt.defined_only = "true")
        17: Foo Struct0 (vt.skip = "true")
        18: optional Foo Struct1 (vt.skip = "true")
        19: i8 Byte2 = 1 (vt.in = "1", vt.not_in = "2")
        20: double Double2 = 3.0 (vt.in = "3.0", vt.not_in = "4.0")
        21: EnumFoo Enum2 = EnumFoo.e2 (vt.in = "EnumFoo.e2", vt.not_in = "EnumFoo.e1")
}

struct FieldReferenceTest {
        1: bool Bool0 (vt.const = "$Bool2")
        2: optional bool Bool1 (vt.const = "$Bool2")
        3: i8 Byte0 = 10 (vt.lt = "$Byte4", vt.le = "$Byte4", vt.gt = "$Byte2", vt.ge = "$Byte2", vt.in = "[$Byte2, $Byte3, $Byte4]", vt.not_in = "[$Byte2, $Byte4]")
        4: optional i8 Byte1 (vt.lt = "$Byte4", vt.le = "$Byte4", vt.gt = "$Byte2", vt.ge = "$Byte2", vt.in = "[$Byte2, $Byte3, $Byte4]", vt.not_in = "[$Byte2, $Byte4]")
        5: double Double0 = 10.0 (vt.lt = "$Double4", vt.le = "$Double4", vt.gt = "$Double2", vt.ge = "$Double2", vt.in = "[$Double2, $Double3, $Double4]", vt.not_in = "[$Double2, $Double4]")
        6: optional double Double1 (vt.lt = "$Double4", vt.le = "$Double4", vt.gt = "$Double2", vt.ge = "$Double2", vt.in = "[$Double2, $Double3, $Double4]", vt.not_in = "[$Double2, $Double4]")
        7: string String0 = "my string" (vt.const = "$String2", vt.min_size = "$Byte2", vt.max_size = "$Byte3", vt.pattern = "$String4", vt.prefix = "$String2", vt.suffix = "$String2", vt.contains = "$String2", vt.not_contains = "$String3")
        8: optional string String1 (vt.const = "$String2", vt.min_size = "$Byte2", vt.max_size = "$Byte3", vt.pattern = "$String4", vt.prefix = "$String2", vt.suffix = "$String2", vt.contains = "$String2", vt.not_contains = "$String3")
        9: binary Binary0 = "my binary" (vt.const = "$Binary2", vt.min_size = "$Byte2", vt.max_size = "$Byte3", vt.pattern = "$Binary4", vt.prefix = "$Binary2", vt.suffix = "$Binary2", vt.contains = "$Binary2", vt.not_contains = "$Binary3")
        10: optional binary Binary1 = "my binary" (vt.const = "$Binary2", vt.min_size = "$Byte2", vt.max_size = "$Byte3", vt.pattern = "$Binary4", vt.prefix = "$Binary2", vt.suffix = "$Binary2", vt.contains = "$Binary2", vt.not_contains = "$Binary3")
        11: map<string, string> Map0 (vt.min_size = "$Byte2", vt.max_size = "$MaxSize", vt.key.min_size = "$Byte2", vt.key.max_size = "$MaxSize", vt.value.min_size = "$Byte2", vt.value.max_size = "$MaxSize")
        12: optional map<string, string> Map1 (vt.min_size = "$Byte2", vt.max_size = "$MaxSize", vt.key.min_size = "$Byte2", vt.key.max_size = "$MaxSize", vt.value.min_size = "$Byte2", vt.value.max_size = "$MaxSize")
        13: list<string> List0 (vt.min_size = "$Byte2", vt.max_size = "$MaxSize", vt.elem.min_size = "$Byte2", vt.elem.max_size = "$MaxSize")
        14: optional list<string> List1 (vt.min_size = "$Byte2", vt.max_size = "$MaxSize", vt.elem.min_size = "$Byte2", vt.elem.max_size = "$MaxSize")
        15: set<string> Set0 (vt.min_size = "$Byte2", vt.max_size = "$MaxSize", vt.elem.min_size = "$Byte2", vt.elem.max_size = "$MaxSize")
        16: optional set<string> Set1 (vt.min_size = "$Byte2", vt.max_size = "$MaxSize", vt.elem.min_size = "$Byte2", vt.elem.max_size = "$MaxSize")
        17: bool Bool2 = false
        18: i8 Byte2 = 0
        19: i8 Byte3 = 10
        20: i8 Byte4 = 20
        21: double Double2 = 0
        22: double Double3 = 10.0
        23: double Double4 = 20.0
        24: string String2 = "my string"
        25: string String3 = "other string"
        26: string String4 = ".*"
        27: binary Binary2 = "my binary"
        28: binary Binary3 = "other binary"
        29: binary Binary4 = ".*"
        30: i64 MaxSize = 10
}

struct ValidationFunctionTest {
        1: string StringFoo
        2: i64 StringLength (vt.in = "[@len($StringFoo)]")
}

struct AnnotationCompatibleTest {
        1: bool Bool0 = true (vt.const = "true", go.tag = 'json:"bool1"')
        2: i8 Byte0 = 1 (vt.lt = "2", go.tag = 'json:"byte1"')
        3: double Double0 = 1.0 (vt.lt = "2.0", go.tag = 'json:"double1"')
        4: string String0 = "my const string" (vt.const = "my const string", go.tag = 'json:"string1"')
        5: binary Binary0 = "my const string" (vt.const = "my const string", go.tag = 'json:"binary1"')
        6: map<string, string> Map0 (vt.max_size = "2", go.tag = 'json:"map1"')
        7: set<string> Set0 (vt.max_size = "2", go.tag = 'json:"set1"')
        8: list<string> List0 (vt.max_size = "2", go.tag = 'json:"list1"')
        9: EnumFoo Enum0 = EnumFoo.e2 (vt.in = "[EnumFoo.e2]", go.tag = 'json:"enum1"')
        10: Foo Struct0 (vt.skip = "true", go.tag = 'json:"struct1"')
}
