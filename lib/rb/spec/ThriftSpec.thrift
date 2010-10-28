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

namespace rb SpecNamespace

struct Hello {
  1: string greeting = "hello world"
}

struct Foo {
  1: i32 simple = 53,
  2: string words = "words",
  3: Hello hello = {'greeting' : "hello, world!"},
  4: list<i32> ints = [1, 2, 2, 3],
  5: map<i32, map<string, double>> complex,
  6: set<i16> shorts = [5, 17, 239],
  7: optional string opt_string
}

struct BoolStruct {
  1: bool yesno = 1
}

struct SimpleList {
  1: list<bool> bools,
  2: list<byte> bytes,
  3: list<i16> i16s,
  4: list<i32> i32s,
  5: list<i64> i64s,
  6: list<double> doubles,
  7: list<string> strings,
  8: list<map<i16, i16>> maps,
  9: list<list<i16>> lists,
  10: list<set<i16>> sets,
  11: list<Hello> hellos
}

exception Xception {
  1: string message,
  2: i32 code = 1
}

service NonblockingService {
  Hello greeting(1:bool english)
  bool block()
  oneway void unblock(1:i32 n)
  oneway void shutdown()
  void sleep(1:double seconds)
}
