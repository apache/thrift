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

typedef i8 mybyte
typedef string mystr
typedef binary mybin

enum EnumFoo {
  e1
  e2
}

struct BasicEqualsFoo {
  1: bool BoolFoo,
  2: optional bool OptBoolFoo,
  3: i8 I8Foo,
  4: optional i8 OptI8Foo,
  5: i16 I16Foo,
  6: optional i16 OptI16Foo,
  7: i32 I32Foo,
  8: optional i32 OptI32Foo,
  9: i64 I64Foo,
  10: optional i64 OptI64Foo,
  11: double DoubleFoo,
  12: optional double OptDoubleFoo,
  13: string StrFoo,
  14: optional string OptStrFoo,
  15: binary BinFoo,
  16: optional binary OptBinFoo,
  17: EnumFoo EnumFoo,
  18: optional EnumFoo OptEnumFoo,
  19: mybyte MyByteFoo,
  20: optional mybyte OptMyByteFoo,
  21: mystr MyStrFoo,
  22: optional mystr OptMyStrFoo,
  23: mybin MyBinFoo,
  24: optional mybin OptMyBinFoo,
}

struct StructEqualsFoo {
  1: BasicEqualsFoo StructFoo,
  2: optional BasicEqualsFoo OptStructFoo,
}

struct ListEqualsFoo {
    1: list<i64> I64ListFoo,
    2: optional list<i64> OptI64ListFoo,
    3: list<string> StrListFoo,
    4: optional list<string> OptStrListFoo,
    5: list<binary> BinListFoo,
    6: optional list<binary> OptBinListFoo,
    7: list<BasicEqualsFoo> StructListFoo,
    8: optional list<BasicEqualsFoo> OptStructListFoo,
    9: list<list<i64>> I64ListListFoo,
    10: optional list<list<i64>> OptI64ListListFoo,
    11: list<set<i64>> I64SetListFoo,
    12: optional list<set<i64>> OptI64SetListFoo,
    13: list<map<i64, string>> I64StringMapListFoo,
    14: optional list<map<i64, string>> OptI64StringMapListFoo,
    15: list<mybyte> MyByteListFoo,
    16: optional list<mybyte> OptMyByteListFoo,
    17: list<mystr> MyStrListFoo,
    18: optional list<mystr> OptMyStrListFoo,
    19: list<mybin> MyBinListFoo,
    20: optional list<mybin> OptMyBinListFoo,
}

struct SetEqualsFoo {
    1: set<i64> I64SetFoo,
    2: optional set<i64> OptI64SetFoo,
    3: set<string> StrSetFoo,
    4: optional set<string> OptStrSetFoo,
    5: set<binary> BinSetFoo,
    6: optional set<binary> OptBinSetFoo,
    7: set<BasicEqualsFoo> StructSetFoo,
    8: optional set<BasicEqualsFoo> OptStructSetFoo,
    9: set<list<i64>> I64ListSetFoo,
    10: optional set<list<i64>> OptI64ListSetFoo,
    11: set<set<i64>> I64SetSetFoo,
    12: optional set<set<i64>> OptI64SetSetFoo,
    13: set<map<i64, string>> I64StringMapSetFoo,
    14: optional set<map<i64, string>> OptI64StringMapSetFoo,
    15: set<mybyte> MyByteSetFoo,
    16: optional set<mybyte> OptMyByteSetFoo,
    17: set<mystr> MyStrSetFoo,
    18: optional set<mystr> OptMyStrSetFoo,
    19: set<mybin> MyBinSetFoo,
    20: optional set<mybin> OptMyBinSetFoo,
}

struct MapEqualsFoo {
    1: map<i64, string> I64StrMapFoo,
    2: optional map<i64, string> OptI64StrMapFoo,
    3: map<string, i64> StrI64MapFoo,
    4: optional map<string, i64>  OptStrI64MapFoo,
    5: map<BasicEqualsFoo, binary> StructBinMapFoo,
    6: optional map<BasicEqualsFoo, binary> OptStructBinMapFoo,
    7: map<binary, BasicEqualsFoo> BinStructMapFoo,
    8: optional map<binary, BasicEqualsFoo> OptBinStructMapFoo,
    9: map<i64, list<i64>> I64I64ListMapFoo,
    10: optional map<i64, list<i64>> OptI64I64ListMapFoo,
    11: map<i64, set<i64>> I64I64SetMapFoo,
    12: optional map<i64, set<i64>> OptI64I64SetMapFoo,
    13: map<i64, map<i64, string>> I64I64StringMapMapFoo,
    14: optional map<i64, map<i64, string>> OptI64I64StringMapMapFoo,
    15: map<mystr, mybin> MyStrMyBinMapFoo,
    16: optional map<mystr, mybin> OptMyStrMyBinMapFoo,
    17: map<i64, mybyte> Int64MyByteMapFoo,
    18: optional map<i64, mybyte> OptInt64MyByteMapFoo,
    19: map<mybyte, i64> MyByteInt64MapFoo,
    20: optional map<mybyte, i64> OptMyByteInt64MapFoo,
}
