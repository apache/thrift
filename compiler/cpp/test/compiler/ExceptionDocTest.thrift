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

namespace cpp main_ns
namespace haxe com.main
namespace java com.main
namespace netstd Main.Ns

exception native {
  1: string message
}

exception object {
  1: string message
}

service ExceptionDocTest {
  /** Method documentation uses x < y && y > z. */
  void documented(
    /** Parameter documentation uses a < b && c > d. */
    1: i32 value
  ) throws (1: object object_error),

  void undocumented() throws (
    /** First line of the explanation
     * second line uses a < b && c > d
     */
    1: native native_error,
    /** Kotlin reserved identifier. */
    2: object object_error
  )
}
