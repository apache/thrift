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

namespace * test.ExceptionStruct

enum ErrorCode {
  GenericError,
  ServerOverload,
  InvalidData
}

struct GetRequest {
  1: string id
  2: binary data     // some arbitrary data
}

struct GetResponse {
  1: i32 job_nr
  2: binary data     // some arbitrary data
}

struct BatchGetRequest {
  1: list<GetRequest> requests
}

struct BatchGetResponse {
  1: map<string, GetRequest> responses,  // key is id
  2: map<string, SomeException> errors,  // key is id
}

exception SomeException {
  2: ErrorCode error
}

service Foo {
  GetResponse get(1: GetRequest request) throws(1: SomeException error);
  BatchGetResponse batchGet(1: BatchGetRequest request) throws(1: SomeException error); // may or may not be the same exception type, only throw exception when full request failed
}

# eof
