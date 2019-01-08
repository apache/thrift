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

#ifndef _THRIFT_CONCURRENCY_PLATFORMTHREADFACTORY_H_
#define _THRIFT_CONCURRENCY_PLATFORMTHREADFACTORY_H_ 1

// clang-format off
#include <thrift/thrift-config.h>
#include <thrift/concurrency/StdThreadFactory.h>
// clang-format on

namespace apache {
namespace thrift {
namespace concurrency {

// clang-format off
using PlatformThreadFactory = StdThreadFactory;
// clang-format on

}
}
} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_PLATFORMTHREADFACTORY_H_
