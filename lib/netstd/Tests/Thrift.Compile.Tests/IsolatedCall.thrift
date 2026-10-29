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

// Fixture for the netstd:isolated generator option (per-call TCallContext clients)

namespace * IsolatedCall

exception Oops {
  1: string why
}

service IsolatedBase {
  i32 ping(1: i32 value)
}

service IsolatedService extends IsolatedBase {
  void nothing()
  string echo(1: string text) throws (1: Oops oops)
  i32 add(1: i32 a, 2: i32 b)
  // argument names that collide with generated parameters
  i32 collide(1: i32 callContext, 2: i32 cancellationToken)
  oneway void fire(1: string text)
}
