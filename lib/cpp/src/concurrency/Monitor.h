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

#ifndef _THRIFT_CONCURRENCY_MONITOR_H_
#define _THRIFT_CONCURRENCY_MONITOR_H_ 1

#include "Exception.h"
#include "Mutex.h"

#include <boost/utility.hpp>


namespace apache { namespace thrift { namespace concurrency {

/**
 * A monitor is a combination mutex and condition-event.  Waiting and
 * notifying condition events requires that the caller own the mutex.  Mutex
 * lock and unlock operations can be performed independently of condition
 * events.  This is more or less analogous to java.lang.Object multi-thread
 * operations.
 *
 * Note the Monitor can create a new, internal mutex; alternatively, a
 * separate Mutex can be passed in and the Monitor will re-use it without
 * taking ownership.  It's the user's responsibility to make sure that the
 * Mutex is not deallocated before the Monitor.
 *
 * Note that all methods are const.  Monitors implement logical constness, not
 * bit constness.  This allows const methods to call monitor methods without
 * needing to cast away constness or change to non-const signatures.
 *
 * @version $Id:$
 */
class Monitor : boost::noncopyable {
 public:
  /** Creates a new mutex, and takes ownership of it. */
  Monitor();

  /** Uses the provided mutex without taking ownership. */
  explicit Monitor(Mutex* mutex);

  /** Uses the mutex inside the provided Monitor without taking ownership. */
  explicit Monitor(Monitor* monitor);

  /** Deallocates the mutex only if we own it. */
  virtual ~Monitor();

  Mutex& mutex() const;

  virtual void lock() const;

  virtual void unlock() const;

  virtual void wait(int64_t timeout=0LL) const;

  virtual void notify() const;

  virtual void notifyAll() const;

 private:

  class Impl;

  Impl* impl_;
};

class Synchronized {
 public:
 Synchronized(const Monitor* monitor) : g(monitor->mutex()) { }
 Synchronized(const Monitor& monitor) : g(monitor.mutex()) { }

 private:
  Guard g;
};


}}} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_MONITOR_H_
