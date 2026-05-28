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

// Recursive types used by TestRecursionDepth to exercise the recursion-depth
// guard in the generated read/write code. This is a JavaME-local copy (rather
// than test/Recursive.thrift) on purpose: that shared file also defines a
// recursive union and a service, and JavaME has no TUnion runtime class, so a
// generated union does not compile. Structs and exceptions are all JavaME can
// express, and the guard reaches all of them through the same generated path.

struct CoRec {
  1: CoRec2 & other
}

struct CoRec2 {
  1: CoRec other
}

struct RecTree {
  1: list<RecTree> children
  2: i16 item
}

// 'leaf' gives the generated all-fields constructor a signature distinct from
// the copy constructor RecError(RecError); without it a self-recursive
// exception whose only field is its own type produces two RecError(RecError).
exception RecError {
  1: RecError & other
  2: i32 leaf
}
