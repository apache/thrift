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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include "Monitor.h"
#include "Exception.h"
#include "Util.h"

#include <assert.h>
#include <errno.h>

#include <boost/scoped_ptr.hpp>
#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/interprocess_condition.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>

namespace apache { namespace thrift { namespace concurrency {

using namespace boost::interprocess;

/**
 * Monitor implementation using the boost interprocess library
 *
 * @version $Id:$
 */
class Monitor::Impl : public interprocess_condition {

 public:

  Impl()
     : ownedMutex_(new Mutex()),
       mutex_(NULL) {
    init(ownedMutex_.get());
  }

  Impl(Mutex* mutex)
     : mutex_(NULL) {
    init(mutex);
  }

  Impl(Monitor* monitor)
     : mutex_(NULL) {
    init(&(monitor->mutex()));
  }

  Mutex& mutex() { return *mutex_; }
  void lock() { mutex().lock(); }
  void unlock() { mutex().unlock(); }

  /**
   * Exception-throwing version of waitForTimeRelative(), called simply
   * wait(int64) for historical reasons.  Timeout is in milliseconds.
   *
   * If the condition occurs,  this function returns cleanly; on timeout or
   * error an exception is thrown.
   */
  void wait(int64_t timeout_ms) {
    int result = waitForTimeRelative(timeout_ms);
    if (result == ETIMEDOUT) {
      throw TimedOutException();
    } else if (result != 0) {
      throw TException(
        "Monitor::wait() failed");
    }
  }

  /**
   * Waits until the specified timeout in milliseconds for the condition to
   * occur, or waits forever if timeout_ms == 0.
   *
   * Returns 0 if condition occurs, ETIMEDOUT on timeout, or an error code.
   */
  int waitForTimeRelative(int64_t timeout_ms) {
    if (timeout_ms == 0LL) {
      return waitForever();
    }

    assert(mutex_);
    interprocess_mutex* mutexImpl =
      reinterpret_cast<interprocess_mutex*>(mutex_->getUnderlyingImpl());
    assert(mutexImpl);

	scoped_lock<interprocess_mutex> lock(*mutexImpl, accept_ownership_type());
	int res = timed_wait(lock, boost::get_system_time()+boost::posix_time::milliseconds(timeout_ms)) ? 0 : ETIMEDOUT;
	lock.release();
	return res;
  }

  /**
   * Waits until the absolute time specified using struct timespec.
   * Returns 0 if condition occurs, ETIMEDOUT on timeout, or an error code.
   */
  int waitForTime(const timespec* abstime) {
    assert(mutex_);
    interprocess_mutex* mutexImpl =
      reinterpret_cast<interprocess_mutex*>(mutex_->getUnderlyingImpl());
    assert(mutexImpl);

    struct timespec currenttime;
    Util::toTimespec(currenttime, Util::currentTime());

	long tv_sec = abstime->tv_sec - currenttime.tv_sec;
	long tv_nsec = abstime->tv_nsec - currenttime.tv_nsec;
	if(tv_sec < 0)
		tv_sec = 0;
	if(tv_nsec < 0)
		tv_nsec = 0;

	scoped_lock<interprocess_mutex> lock(*mutexImpl, accept_ownership_type());
	int res = timed_wait(lock, boost::get_system_time() +
		boost::posix_time::seconds(tv_sec) +
		boost::posix_time::microseconds(tv_nsec / 1000)
		) ? 0 : ETIMEDOUT;
	lock.release();
	return res;
  }

  /**
   * Waits forever until the condition occurs.
   * Returns 0 if condition occurs, or an error code otherwise.
   */
  int waitForever() {
    assert(mutex_);
    interprocess_mutex* mutexImpl =
      reinterpret_cast<interprocess_mutex*>(mutex_->getUnderlyingImpl());
    assert(mutexImpl);

	scoped_lock<interprocess_mutex> lock(*mutexImpl, accept_ownership_type());
	((interprocess_condition*)this)->wait(lock);
	lock.release();
    return 0;
  }


  void notify() {
	  notify_one();
  }

  void notifyAll() {
	  notify_all();
  }

 private:

  void init(Mutex* mutex) {
    mutex_ = mutex;
  }

  boost::scoped_ptr<Mutex> ownedMutex_;
  Mutex* mutex_;
};

Monitor::Monitor() : impl_(new Monitor::Impl()) {}
Monitor::Monitor(Mutex* mutex) : impl_(new Monitor::Impl(mutex)) {}
Monitor::Monitor(Monitor* monitor) : impl_(new Monitor::Impl(monitor)) {}

Monitor::~Monitor() { delete impl_; }

Mutex& Monitor::mutex() const { return const_cast<Monitor::Impl*>(impl_)->mutex(); }

void Monitor::lock() const { const_cast<Monitor::Impl*>(impl_)->lock(); }

void Monitor::unlock() const { const_cast<Monitor::Impl*>(impl_)->unlock(); }

void Monitor::wait(int64_t timeout) const { const_cast<Monitor::Impl*>(impl_)->wait(timeout); }

int Monitor::waitForTime(const timespec* abstime) const {
  return const_cast<Monitor::Impl*>(impl_)->waitForTime(abstime);
}

int Monitor::waitForTimeRelative(int64_t timeout_ms) const {
  return const_cast<Monitor::Impl*>(impl_)->waitForTimeRelative(timeout_ms);
}

int Monitor::waitForever() const {
  return const_cast<Monitor::Impl*>(impl_)->waitForever();
}

void Monitor::notify() const { const_cast<Monitor::Impl*>(impl_)->notify(); }

void Monitor::notifyAll() const { const_cast<Monitor::Impl*>(impl_)->notifyAll(); }

}}} // apache::thrift::concurrency
