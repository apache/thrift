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

#include "Monitor.h"
#include "Exception.h"
#include "Util.h"

#include <boost/scoped_ptr.hpp>

#include <assert.h>
#include <errno.h>

#include <iostream>

#include <pthread.h>

namespace apache { namespace thrift { namespace concurrency {

using boost::scoped_ptr;

/**
 * Monitor implementation using the POSIX pthread library
 *
 * @version $Id:$
 */
class Monitor::Impl {

 public:

  Impl()
     : ownedMutex_(new Mutex()),
       mutex_(NULL),
       condInitialized_(false) {
    init(ownedMutex_.get());
  }

  Impl(Mutex* mutex)
     : mutex_(NULL),
       condInitialized_(false) {
    init(mutex);
  }

  Impl(Monitor* monitor)
     : mutex_(NULL),
       condInitialized_(false) {
    init(&(monitor->mutex()));
  }

  ~Impl() { cleanup(); }

  Mutex& mutex() { return *mutex_; }
  void lock() { mutex().lock(); }
  void unlock() { mutex().unlock(); }

  void wait(int64_t timeout) const {
    assert(mutex_);
    pthread_mutex_t* mutexImpl =
      reinterpret_cast<pthread_mutex_t*>(mutex_->getUnderlyingImpl());
    assert(mutexImpl);

    // XXX Need to assert that caller owns mutex
    assert(timeout >= 0LL);
    if (timeout == 0LL) {
      int iret = pthread_cond_wait(&pthread_cond_, mutexImpl);
      assert(iret == 0);
    } else {
      struct timespec abstime;
      int64_t now = Util::currentTime();
      Util::toTimespec(abstime, now + timeout);
      int result = pthread_cond_timedwait(&pthread_cond_,
                                          mutexImpl,
                                          &abstime);
      if (result == ETIMEDOUT) {
        // pthread_cond_timedwait has been observed to return early on
        // various platforms, so comment out this assert.
        //assert(Util::currentTime() >= (now + timeout));
        throw TimedOutException();
      }
    }
  }

  void notify() {
    // XXX Need to assert that caller owns mutex
    int iret = pthread_cond_signal(&pthread_cond_);
    assert(iret == 0);
  }

  void notifyAll() {
    // XXX Need to assert that caller owns mutex
    int iret = pthread_cond_broadcast(&pthread_cond_);
    assert(iret == 0);
  }

 private:

  void init(Mutex* mutex) {
    mutex_ = mutex;

    if (pthread_cond_init(&pthread_cond_, NULL) == 0) {
      condInitialized_ = true;
    }

    if (!condInitialized_) {
      cleanup();
      throw SystemResourceException();
    }
  }

  void cleanup() {
    if (condInitialized_) {
      condInitialized_ = false;
      int iret = pthread_cond_destroy(&pthread_cond_);
      assert(iret == 0);
    }
  }

  scoped_ptr<Mutex> ownedMutex_;
  Mutex* mutex_;

  mutable pthread_cond_t pthread_cond_;
  mutable bool condInitialized_;
};

Monitor::Monitor() : impl_(new Monitor::Impl()) {}
Monitor::Monitor(Mutex* mutex) : impl_(new Monitor::Impl(mutex)) {}
Monitor::Monitor(Monitor* monitor) : impl_(new Monitor::Impl(monitor)) {}

Monitor::~Monitor() { delete impl_; }

Mutex& Monitor::mutex() const { return impl_->mutex(); }

void Monitor::lock() const { impl_->lock(); }

void Monitor::unlock() const { impl_->unlock(); }

void Monitor::wait(int64_t timeout) const { impl_->wait(timeout); }

void Monitor::notify() const { impl_->notify(); }

void Monitor::notifyAll() const { impl_->notifyAll(); }

}}} // apache::thrift::concurrency
