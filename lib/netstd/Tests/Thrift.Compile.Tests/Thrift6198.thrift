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
// Both programs share the same C# namespace and both use the same container
// types. Before the fix, both extension classes declared the same
// DeepCopy()/Equals()/GetHashCode() signatures, so every call site became
// ambiguous (CS0121).
//
// The fields below cover all three directions at once:
// - values/value_set/value_map are also used by the included program, so they
//   have to be left to it (a second copy would be CS0121 again)
// - shared_base_container is a container over a base type that the included
//   program uses as well, so the same applies
// - own_container is ours alone and has to stay right here, or the generated
//   call site in HoldsIncluded.DeepCopy() would not resolve at all (CS1061)
//
// Both the typedef'd and the directly named form of the included struct have
// to end up as the same container type here.

namespace * Thrift6198

include "Thrift6198.included.thrift"

typedef Thrift6198.included.InnerStruct IncludedAlias

struct HoldsIncluded {
  1: list<IncludedAlias> values
  2: set<Thrift6198.included.InnerStruct> value_set
  3: map<string, IncludedAlias> value_map
  4: list<string> shared_base_container
  5: list<double> own_container
  6: Thrift6198.included.OuterInIncluded borrowed
}
