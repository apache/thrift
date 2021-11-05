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

#ifndef _THRIFT_CONCURRENCY_MUTEX_H_
#define _THRIFT_CONCURRENCY_MUTEX_H_ 1

#include <memory>
#include <thrift/TNonCopyable.h>

namespace apache {
namespace thrift {
namespace concurrency {

/**
 * NOTE: All mutex implementations throw an exception on failure.  See each
 *       specific implementation to understand the exception type(s) used.
 */

/**
 * A simple mutex class
 *
 * @version $Id:$
 */
class Mutex {
public:
  Mutex();
  virtual ~Mutex() = default;

  virtual void lock() const;
  virtual bool trylock() const;
  virtual bool timedlock(int64_t milliseconds) const;
  virtual void unlock() const;

  void* getUnderlyingImpl() const;

private:
  class impl;
  std::shared_ptr<impl> impl_;
};


class Guard : apache::thrift::TNonCopyable {
public:
  Guard(const Mutex& value, int64_t timeout = 0) : mutex_(&value) {
    if (timeout == 0) {
      value.lock();
    } else if (timeout < 0) {
      if (!value.trylock()) {
        mutex_ = nullptr;
      }
    } else {
      if (!value.timedlock(timeout)) {
        mutex_ = nullptr;
      }
    }
  }
  ~Guard() {
    if (mutex_) {
      mutex_->unlock();
    }
  }

  operator bool() const { return (mutex_ != nullptr); }

private:
  const Mutex* mutex_;
};

}
}
} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_MUTEX_H_
