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

// Testcase for THRIFT-6474 A type named Exception breaks the catch-all clause of the generated processor

namespace * Thrift6474.exceptions

include "Thrift6474.enum.thrift"
include "Thrift6474.service.thrift"
include "Thrift6474.struct.thrift"
include "Thrift6474.union.thrift"

exception Exception {
  1: string msg
}

service ThrowingService {
  i32 Call(1: i32 value) throws (1: Exception error)
}
