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

# Generated with the struct_key_entries option: maps keyed by a struct, an
# exception or a union become []thrift.MapEntry[*K, V] instead of map[*K]V.
# ContainerKeyTest.thrift pins the default map[*K]V representation.

struct Key {
  1: i32 id
  2: string name
}

exception KeyErr {
  1: string msg
}

union KeyUnion {
  1: i32 num
  2: string text
}

typedef Key KeyAlias

enum KeyKind {
  UNKNOWN = 0
  ALPHA = 1
  BETA = 2
}

# Every field is a non-pointer scalar, so the write-time uniqueness check
# takes the linear seen-map path that compares keys with ==.
struct ComparableKey {
  1: bool flag
  2: i8 tiny
  3: i16 small
  4: i32 id
  5: i64 big
  6: double ratio
  7: string name
  8: KeyKind kind
}

# binary is a slice in Go, so == does not work on the struct and the
# uniqueness check falls back to the pairwise Equals scan.
struct PairwiseKey {
  1: i32 id
  2: binary blob
}

# An optional field generates as a pointer, so this key uses the pairwise
# scan as well.
struct OptionalFieldKey {
  1: i32 id
  2: optional string note
}

typedef KeyUnion KeyUnionAlias

struct ValidatedKey {
  1: i32 id (vt.gt = "0")
}

struct StructKeyStruct {
  1: map<Key, string> byKey
  2: optional map<KeyErr, i32> byErr
  3: map<string, i32> plain
  4: map<KeyUnion, i32> byUnion
  5: optional map<KeyAlias, string> byAlias
  6: list<map<Key, string>> nested
  7: map<Key, map<Key, i32>> valueAlsoKeyed
  8: map<ValidatedKey, string> validated (vt.key.skip = "false")
  9: map<Key, list<string>> listValue
  10: map<Key, set<string>> setValue
  11: map<ComparableKey, string> byComparable
  12: map<PairwiseKey, string> byPairwise
  13: map<OptionalFieldKey, string> byOptionalField
  14: map<KeyUnionAlias, i32> byUnionAlias
}

const map<Key, i32> STRUCT_KEYED_CONST = {{"id": 1, "name": "one"}: 1, {"id": 2, "name": "two"}: 2}
