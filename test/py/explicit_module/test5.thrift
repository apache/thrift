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

namespace py test5

include "shared_types.thrift"

enum TestEnum {
  TestEnum0 = 0,
  TestEnum1 = 1,
}

struct TestStruct {
    1: optional string param1
    2: optional TestEnum param2
    3: optional shared_types.SharedEnum param3
}

/**
 * Structs can also be exceptions, if they are nasty.
 */
exception TestException {
  1: i32 whatOp,
  2: shared_types.SharedEnum why
  3: TestEnum who
}