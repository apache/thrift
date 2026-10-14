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

// THRIFT-6406: names that are Rust keywords. async, await, dyn and try
// became keywords with Rust 2018 and gen with Rust 2024; type and match
// have always been keywords. proc is no longer reserved.

struct KeywordFields {
  1: i32 async
  2: i32 await
  3: i32 dyn
  4: i32 try
  5: i32 gen
  6: i32 type
  7: i32 match
}

service KeywordMethods {
  i32 gen(1: i32 dyn, 2: i32 try)
  void try()
  void type(1: i32 type)
  void match()
  oneway void await(1: i32 async)
  void proc()
}
