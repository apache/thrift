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

namespace cpp test.forward_setter

struct InnerStruct {
  1: required string name;
  2: optional i32 value;
}

struct TestForwardSetter {
  1: required i32 primitive_field;
  2: optional string complex_string;
  3: optional InnerStruct complex_struct;
  4: optional list<string> complex_list;
  5: optional map<string, i32> complex_map;
  6: required bool primitive_bool;
}
