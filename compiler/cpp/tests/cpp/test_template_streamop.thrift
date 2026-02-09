// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

namespace cpp test.template_streamop

enum Status {
  ACTIVE = 1,
  INACTIVE = 2,
  PENDING = 3,
}

struct SimpleStruct {
  1: i32 id;
  2: string name;
  3: optional string description;
}

struct NestedStruct {
  1: i32 value;
  2: SimpleStruct inner;
}

struct StructWithEnum {
  1: Status status;
  2: string name;
}
