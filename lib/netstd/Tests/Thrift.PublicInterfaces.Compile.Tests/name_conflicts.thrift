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

// Testcases for 
// - THRIFT-5091 Netstd generator produces uncompileable code for struct names ending with "_result" or "_args"
// - THRIFT-5444 netstd generator produces uncompileable code for enums ending with "_result" or "_args"
// - THRIFT-5445 "cancellationToken" cannot be used as argument name

namespace * name_conflicts

include "name_conflicts.enum.thrift"

struct some_struct_args {
	1: name_conflicts.enum.some_args    some_args
	2: name_conflicts.enum.some_result  some_result
	3: required i32 cancellationToken
}

exception some_error_result {
	1: name_conflicts.enum.some_args    some_args
	2: name_conflicts.enum.some_result  some_result
	3: optional i32 cancellationToken
}

service some_service {	

	name_conflicts.enum.some_result some_method( 
		1: name_conflicts.enum.some_args some_args
		2: some_struct_args more_args
		3: i32 cancellationToken
	) throws (
		1: some_error_result cancellationToken
	)
	
}

// EOF
