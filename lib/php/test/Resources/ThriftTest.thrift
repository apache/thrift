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

namespace php TestValidators

include "../../../../test/ThriftTest.thrift"

union UnionOfStrings {
  1: string aa;
  2: string bb;
}

// Regression for THRIFT-1209: PHP reserved words must round-trip through the
// generator as class constants. PHP 7.1+ permits reserved identifiers as
// class constants (except `class`); the generator must not mangle or reject
// them. See lib/php/test/Unit/Compiler/ReservedKeywordEnumTest.php.
enum ReservedKeywordEnum {
  GLOBAL = 1,
  STATIC = 2,
  LIST = 3,
  RETURN = 4,
}

service TestService {
    void test() throws(1: ThriftTest.Xception xception);
}
