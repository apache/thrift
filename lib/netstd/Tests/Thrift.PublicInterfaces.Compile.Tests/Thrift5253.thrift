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

// Testcase for THRIFT-5253 using Result in result name generates wrong IAsync interface

namespace * Thrift5253


// this works
struct WorksArrrgs { 1: i32 foo }
struct WorksRslt   { 1: i32 foo }

// this does not
struct BrokenResult{ 1: i32 foo }
struct BrokenArgs  { 1: i32 foo }

struct InternalStructs { 1: optional i32 foo }
struct AsyncProcessor  { 1: optional i32 foo }
struct Client          { 1: optional i32 foo }
struct IAsync          { 1: optional i32 foo }

struct ReservedMemberName { 1: optional i32 Isset }

service MyService{
    BrokenResult Broken( 1 : BrokenArgs  foo)
    WorksRslt     Works( 1 : WorksArrrgs foo)

    InternalStructs InternalStructs( 1: InternalStructs foo)
    AsyncProcessor  AsyncProcessor ( 1: AsyncProcessor  foo)
    Client          Client         ( 1: Client  foo)
    IAsync          IAsync         ( 1: IAsync  foo)
}

