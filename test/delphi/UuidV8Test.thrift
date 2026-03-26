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

/**
 * Test file for UUIDv8 deterministic GUID generation.
 * Covers: struct field-order sensitivity, service inheritance (parent hash),
 * and various field/return types.
 */

namespace delphi UuidV8Test

// --- Structs ---

struct Point {
  1: i32 x
  2: i32 y
}

// Same fields as Point but in reverse order — must produce a different GUID
struct PointReversed {
  1: i32 y
  2: i32 x
}

struct Container {
  1: list<string> items
  2: map<string, i32> counts
  3: set<i32> flags
}

// --- Services ---

service BaseService {
  void ping()
  string echo(1: string msg)
}

// Extends BaseService — GUID must incorporate parent's hash
service ExtendedService extends BaseService {
  i32 add(1: i32 a, 2: i32 b)
  oneway void fire(1: string event)
}
