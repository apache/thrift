/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

// Recursive types mirroring test/Recursive.thrift, used by RecursionDepthTest
// to drive the generated struct/union/exception read/write path. CoRec <->
// CoRec2 form a mutually recursive chain; RecTree is a wide tree of nested
// structs; RecUnion and RecError are self-recursive union and exception types,
// each carrying a non-recursive leaf so a finite value can be constructed.

namespace java org.apache.thrift.recursion

struct CoRec {
  1: CoRec2 other
}

struct CoRec2 {
  1: CoRec other
}

struct RecTree {
  1: list<RecTree> children
  2: i16 item
}

union RecUnion {
  1: RecUnion child
  2: i16 leaf
}

exception RecError {
  1: RecError child
  2: i16 leaf
}
