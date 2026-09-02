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

# Maps keyed by list, set, map or a typedef of one of those cannot be Go
# maps. The generator represents them as []thrift.MapEntry[K, V].

typedef list<i32> IntList

struct ContainerKeyStruct {
  1: map<list<string>, string> listKey
  2: map<set<i32>, i64> setKey
  3: map<map<string, i32>, bool> mapKey
  4: optional map<IntList, string> typedefKey
  5: list<map<list<string>, list<i32>>> nested
  6: map<list<string>, map<set<i32>, string>> valueAlsoKeyed
  7: map<list<string>, string> validated (vt.key.min_size = "1", vt.value.min_size = "1")
}

const map<list<string>, i32> LIST_KEYED_CONST = {["a", "b"]: 2, []: 0}
