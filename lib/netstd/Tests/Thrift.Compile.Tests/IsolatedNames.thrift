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

// Fixture for the netstd:isolated option: names of the IDL that look like names of the generated isolated client.
// The generated client must compile, and the shared-protocol operations must keep the parameter names of the IDL.

namespace * IsolatedNames

// the name of a runtime type that the call-context overloads use
struct TCallContext {
  1: i32 x
}

service Names {
  // names of members of the client base class
  i32 IsCallIsolated(1: i32 a)
  i32 CreateCallContextAsync(1: i32 a)

  // parameter names that look like the generated locals
  i32 collide(1: i32 callContext, 2: i32 callContext_, 3: i32 timeoutCause_)
}

// names of members of the client base class, declared by an ancestor service: in the client of the child service,
// "base" is the client of the parent, not the client base class
service Parent {
  i32 IsCallIsolated(1: i32 a)
  i32 CreateCallContextAsync(1: i32 a)
}

service Child extends Parent {
  i32 ping(1: i32 a)
}
