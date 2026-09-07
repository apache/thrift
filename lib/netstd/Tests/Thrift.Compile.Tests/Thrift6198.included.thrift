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

// Testcase for THRIFT-6198 CS0121 ambiguous extension methods generated for
// container types referencing included structs.
//
// This is the included program. It deliberately shares its C# namespace with
// the including program, and it uses the very same container types that the
// including program uses.

namespace * Thrift6198

struct InnerStruct {
  1: string data
}

typedef InnerStruct InnerAlias

struct OuterInIncluded {
  1: list<InnerAlias> inners
  2: set<InnerAlias> inner_set
  3: map<string, InnerAlias> inner_map
  4: list<string> shared_base_container
}
