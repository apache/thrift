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

// The -remote stub declares local variables such as client and host; packages
// with those names, used by the arguments below, must not be shadowed by them.
namespace go remoteshadowtest

include "RemoteShadowClient.thrift"
include "RemoteShadowHost.thrift"

service ShadowService {
  void f(1: RemoteShadowClient.Info info, 2: RemoteShadowHost.Addr addr)
}
