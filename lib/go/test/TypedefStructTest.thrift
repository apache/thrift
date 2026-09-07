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

// https://issues.apache.org/jira/browse/THRIFT-6197

namespace go typedefstructtest

// A typedef used before it is declared. THRIFT-5601, THRIFT-5489.
struct UsesForwardTypedef {
  1: optional ForwardTypedef value,
  2: optional list<ForwardTypedef> values,
}

typedef i32 ForwardTypedef

// A struct used before it is declared. The first attempt at THRIFT-5601 made
// this field a value rather than a pointer, so GetForward().GetPayload() no
// longer compiled; that is THRIFT-5685, and this shape guards against it.
struct UsesForwardStruct {
  1: optional ForwardStruct forward,
}

struct ForwardStruct {
  1: optional i64 payload,
}

struct InnerStruct {
  1: required string s,
}

// A typedef of a struct. THRIFT-4901.
typedef InnerStruct InnerAlias

// A typedef of a typedef of a struct.
typedef InnerAlias NestedAlias

exception ExcStruct {
  1: optional i32 code,
}

typedef ExcStruct ExcAlias

// A typedef of a container, which names the container and not its element.
typedef list<InnerStruct> InnerList
typedef map<string, InnerStruct> InnerMap

struct Outer {
  1: optional InnerAlias inner,
  2: optional list<InnerAlias> inners,
  3: optional map<string, InnerAlias> byName,
  4: optional NestedAlias nested,
  5: optional InnerList listAlias,
  6: optional InnerMap mapAlias,
}

// A typedef'd struct in a service signature, which also covers the -remote
// stub the compiler writes for it. THRIFT-3491.
service AliasService {
  InnerAlias echo(1: InnerAlias value) throws (1: ExcAlias err),
}
