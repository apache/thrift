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

namespace java org.apache.thrift.partial

// This thrift file is meant for testing partial deserialization.
// It includes all field types and most combinations of complex types.
// Those fields help ensure correctness of partial deserialization.

enum TstEnum {
    UNKNOWN = 0,
    E_ONE = 1,
    E_TWO = 2
}

struct SmallStruct {
    // Primitive fields
    1: optional byte byteField;
    2: optional i16 i16Field;
    3: optional i32 i32Field;
    4: optional i64 i64Field;
    5: optional double doubleField;
    6: optional string stringField;

    // Enum
    7: optional TstEnum enumField;
}

// TODO(kpandit): Need to add bool field
struct TestStruct {
    // Primitive fields
    1: optional byte byteField;
    2: optional i16 i16Field;
    3: optional i32 i32Field;
    4: optional i64 i64Field;
    5: optional double doubleField;
    6: optional string stringField;

    // Enum
    7: optional TstEnum enumField;

    8: optional binary binaryField;

    // List
    10: optional list<byte> byteList;
    11: optional list<i16> i16List;
    12: optional list<i32> i32List;
    13: optional list<i64> i64List;
    14: optional list<double> doubleList;
    15: optional list<string> stringList;
    16: optional list<TstEnum> enumList;
    17: optional list<list<i32>> listList;
    18: optional list<set<i32>> setList;
    19: optional list<map<string, i32>> mapList;
    20: optional list<SmallStruct> structList;
    21: optional list<binary> binaryList;

    // Set
    30: optional set<byte> byteSet;
    31: optional set<i16> i16Set;
    32: optional set<i32> i32Set;
    33: optional set<i64> i64Set;
    34: optional set<double> doubleSet;
    35: optional set<string> stringSet;
    36: optional set<TstEnum> enumSet;
    37: optional set<list<i32>> listSet (nolint = "set.value.type");
    38: optional set<set<i32>> setSet (nolint = "set.value.type");
    39: optional set<map<string, i32>> mapSet (nolint = "set.value.type");
    40: optional set<SmallStruct> structSet (nolint = "set.value.type");
    41: optional set<binary> binarySet;

    // Map
    50: optional map<byte, byte> byteMap;
    51: optional map<i16, i16> i16Map;
    52: optional map<i32, i32> i32Map;
    53: optional map<i64, i64> i64Map;
    54: optional map<double, double> doubleMap;
    55: optional map<string, string> stringMap;
    56: optional map<TstEnum, TstEnum> enumMap;
    57: optional map<i32, list<i32>> listMap;
    58: optional map<i32, set<i32>> setMap;
    59: optional map<i32, map<i32, i32>> mapMap;
    60: optional map<SmallStruct, SmallStruct> structMap (nolint = "map.key.type");
    61: optional map<i32, binary> binaryMap;

    70: optional SmallStruct structField;
}

struct InnermostStruct {
  1: optional string value;
  2: optional i32 intValue;
}

struct InnerStruct {
  1: optional InnermostStruct value;
  2: optional i32 intValue;
}

struct OuterStruct {
  1: optional InnerStruct value;
  2: optional map<string, InnerStruct> structMap;
}

union SimpleUnion {
  1: optional i32 intValue;
  2: optional string stringValue;
}

struct StructWithUnions {
  1: optional i32 intValue;
  2: optional SmallStruct smallStruct;
  3: optional SimpleUnion simpleUnion;
  4: optional list<SimpleUnion> unionList;
  5: optional set<SimpleUnion> unionSet (nolint = "set.value.type");
  6: optional map<SimpleUnion, string> keyUnionMap (nolint = "map.key.type");
  7: optional map<string, SimpleUnion> valUnionMap;
  8: optional map<SimpleUnion, SimpleUnion> unionMap (nolint = "map.key.type");
}
