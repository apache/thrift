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

namespace go constoptionalfieldb

include "ConstOptionalFieldImport.thrift"

typedef ConstOptionalFieldImport.Foo TypedefBFoo

typedef bool TypedefBBool
typedef i8 TypedefBI8
typedef i16 TypedefBI16
typedef i32 TypedefBI32
typedef i64 TypedefBI64
typedef double TypedefBDouble
typedef string TypedefBString
typedef binary TypedefBBinary

struct Bar {
  1: optional ConstOptionalFieldImport.Foo optFoo,
  2: optional ConstOptionalFieldImport.TypedefAFoo aFoo,
  3: optional TypedefBFoo bFoo,

  4: optional bool optBool,
  5: optional ConstOptionalFieldImport.TypedefABool aBool,
  6: optional TypedefBBool bBool,

  7: optional i8 optI8,
  8: optional ConstOptionalFieldImport.TypedefAI8 aI8,
  9: optional TypedefBI8 bI8,

  10: optional i16 optI16,
  11: optional ConstOptionalFieldImport.TypedefAI16 aI16,
  12: optional TypedefBI16 bI16,

  13: optional i32 optI32,
  14: optional ConstOptionalFieldImport.TypedefAI32 aI32,
  15: optional TypedefBI32 bI32,

  16: optional i64 optI64,
  17: optional ConstOptionalFieldImport.TypedefAI64 aI64,
  18: optional TypedefBI64 bI64,

  19: optional double optDouble,
  20: optional ConstOptionalFieldImport.TypedefADouble aDouble,
  21: optional TypedefBDouble bDouble,

  22: optional string optString,
  23: optional ConstOptionalFieldImport.TypedefAString aString,
  24: optional TypedefBString bString,

  25: optional binary optBinary,
  26: optional ConstOptionalFieldImport.TypedefABinary aBinary,
  27: optional TypedefBBinary bBinary,
}

const list<Bar> CONSTANTS = [
  {
    "optFoo": ConstOptionalFieldImport.Foo.One,
    "aFoo": ConstOptionalFieldImport.Foo.One,
    "bFoo": ConstOptionalFieldImport.Foo.One,

    "optBool": true,
    "aBool": true,
    "bBool": true,

    "optI8": 8,
    "aI8": 8,
    "bI8": 8,

    "optI16": 16,
    "aI16": 16,
    "bI16": 16,

    "optI32": 32,
    "aI32": 32,
    "bI32": 32,

    "optI64": 64,
    "aI64": 64,
    "bI64": 64,

    "optDouble": 1234,
    "aDouble": 1234,
    "bDouble": 1234,

    "optString": "string",
    "aString": "string",
    "bString": "string",

    "optBinary": "binary",
    "aBinary": "binary",
    "bBinary": "binary",
  },
]
