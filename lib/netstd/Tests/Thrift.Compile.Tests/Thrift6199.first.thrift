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

// Testcase for THRIFT-6199, see Thrift6199.thrift.
//
// One of two sibling programs. It shares its C# namespace with the other one
// and uses the same container types, but neither of the two includes the other,
// so neither generator can know about the duplicate.

namespace * Thrift6199

struct FirstHolder {
  1: list<i32> numbers
  2: map<string, list<i32>> nested
}
