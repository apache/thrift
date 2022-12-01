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

namespace cpp apache.thrift.test

exception MyError {
  1: string message
}

service ParentService {
  i32 incrementGeneration()
  i32 getGeneration()
  void addString(1: string s)
  list<string> getStrings()

  binary getDataWait(1: i32 length)
  oneway void onewayWait()
  void exceptionWait(1: string message) throws (2: MyError error)
  void unexpectedExceptionWait(1: string message)
}

service ChildService extends ParentService {
  i32 setValue(1: i32 value)
  i32 getValue()
}
