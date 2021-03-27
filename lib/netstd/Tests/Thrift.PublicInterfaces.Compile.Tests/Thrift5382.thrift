# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

// Testcase for THRIFT-5382 Netstd default list/set enums values are generated incorrectly

namespace * Thrift5382

include "Thrift5382.objs.thrift"

struct RequestModel {
	// Breaks
	1: optional set<Thrift5382.objs.FoobarEnum> data_1 = [ FoobarEnum.Val_1, FoobarEnum.Val_2 ],
	// Breaks
	2: optional list<Thrift5382.objs.FoobarEnum> data_2 = [ FoobarEnum.Val_1, FoobarEnum.Val_2 ],
	// Works
	3: optional Thrift5382.objs.FoobarEnum data_3 = FoobarEnum.Val_1
}

service Test {
    void CallMe( 
		1 : RequestModel foo,
	)
}
