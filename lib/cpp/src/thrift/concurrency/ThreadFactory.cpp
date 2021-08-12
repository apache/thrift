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

#include <thrift/thrift-config.h>

#include <thrift/concurrency/ThreadFactory.h>
#include <memory>

namespace apache {
namespace thrift {
namespace concurrency {

std::shared_ptr<Thread> ThreadFactory::newThread(std::shared_ptr<Runnable> runnable) const {
  std::shared_ptr<Thread> result = std::make_shared<Thread>(isDetached(), runnable);
  runnable->thread(result);
  return result;
}

Thread::id_t ThreadFactory::getCurrentThreadId() const {
  return std::this_thread::get_id();
}
}
}
} // apache::thrift::concurrency
