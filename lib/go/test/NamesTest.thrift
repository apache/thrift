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

struct NamesTest {
    1: required string type
}

service NameCollisionOne
{
    void blahBlah()
}

service NameCollisionTwo
{
    void blahBlah()
}

// A field named isSetX next to a field named x must not collide with the
// generated IsSetX() accessor.
struct SetFlagNamesTest {
    1: optional i32 queryParallelism
    2: optional bool isSetQueryParallelism
    3: optional string default_pool_path
    4: optional bool is_set_default_pool_path
}
