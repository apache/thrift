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

namespace cpp fuzz
namespace java org.apache.thrift.fuzz
namespace py fuzz
namespace swift Fuzz

// Test typedefs
typedef i64 UserId
typedef binary BinaryData

// Test all primitive types in a compact struct
struct BasicTypes {
    1: bool bool_field,
    2: i8 byte_field,
    3: i16 i16_field,
    4: i32 i32_field,
    5: i64 i64_field,
    6: double double_field,
    7: string string_field,
    8: binary binary_field,
    9: uuid uuid_field
}

// Test optional/required/default requiredness
struct Requiredness {
    1: required i32 req_field,
    2: optional i32 opt_field,
    3: i32 default_field,  // default requiredness
    4: optional string opt_with_default = "test",
    5: required bool req_with_default = true
}

// Test field ID edge cases
struct FieldIDTest {
    1: i32 first,
    100: i32 gap,
    255: i32 medium_id,
    32767: i32 large_id,
}

// Test empty struct
struct EmptyStruct {}

// Test union
union TestUnion {
    1: i32 int_field,
    2: string string_field,
    3: BasicTypes struct_field,
    4: binary binary_field
}

// Test containers (but not too deeply nested)
struct Containers {
    1: list<i32> int_list,
    2: set<string> string_set,
    3: map<i32, string> int_string_map,
    4: list<BasicTypes> struct_list,
    5: map<string, list<i32>> nested_map,
    6: set<UserId> typedef_set,
}

// Test enum with various values
enum TestEnum {
    ZERO = 0,
    ONE = 1,
    TWO = 2,
    NEGATIVE = -1,
    LARGE = 32767,
    HEX_VALUE = 0xFF
}

// Test recursive structure
struct RecursiveStruct {
    1: optional RecursiveStruct & recurse,
    2: i32 data,
    3: optional list<RecursiveStruct> children
}

// Main test structure - kept minimal but comprehensive
struct FuzzTest {
    1: required BasicTypes basic,
    2: required Requiredness required_test,
    3: required Containers containers,
    4: required TestUnion union_field,
    5: optional RecursiveStruct recursive,
    6: optional EmptyStruct empty,
    7: optional FieldIDTest field_ids,
    8: required TestEnum enum_field,
    9: optional map<TestEnum, string> enum_map,
    10: UserId user_id,
    11: BinaryData data,
}