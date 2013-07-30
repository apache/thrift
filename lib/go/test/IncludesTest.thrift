#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

include "ThriftTest.thrift"

const ThriftTest.UserId USERID = 42
const ThriftTest.MapType MAPCONSTANT = {'hello':{}, 'goodnight':{}}

struct testStruct {
  1: list<ThriftTest.Numberz> listNumbers
}

struct TestStruct2 {
  1: testStruct blah,
  2: ThriftTest.UserId id
}

service testService extends ThriftTest.SecondService {
  ThriftTest.CrazyNesting getCrazyNesting(
    1: ThriftTest.StructA a,
    2: ThriftTest.Numberz numbers
  ) throws(1: ThriftTest.Xception err1),

  void getSomeValue_DO_NOT_CALL(),
}

service ExtendedService extends testService {
  void extendedMethod(),
}
