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

// Testcase for THRIFT-6199 CS0121 for container extension methods shared by
// programs that have no include relation.
//
// Pulling both siblings in is what puts their two extension classes into one
// compilation. They share a C# namespace and declare the same container
// extension methods, but since neither includes the other, the duplicate
// cannot be removed the way THRIFT-6198 removes it for an include.
//
// The generated call sites therefore have to name the class that owns the
// method rather than rely on extension method syntax, or FirstHolder.cs and
// SecondHolder.cs both fail with CS0121.

namespace * Thrift6199

include "Thrift6199.first.thrift"
include "Thrift6199.second.thrift"

struct BothHolders {
  1: Thrift6199.first.FirstHolder first
  2: Thrift6199.second.SecondHolder second
  3: list<i32> numbers
}
