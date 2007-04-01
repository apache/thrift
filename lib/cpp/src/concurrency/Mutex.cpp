// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "Mutex.h"

#include <assert.h>
#include <pthread.h>

namespace facebook { namespace thrift { namespace concurrency { 

/** 
 * Implementation of Mutex class using POSIX mutex
 *
 * @author marc
 * @version $Id:$
 */
class Mutex::impl {
 public:
  impl() : initialized_(false) {
    int ret = pthread_mutex_init(&pthread_mutex_, NULL);
    assert(ret == 0);
    initialized_ = true;
  }

  ~impl() {
    if (initialized_) {
      initialized_ = false;
      int ret = pthread_mutex_destroy(&pthread_mutex_);
      assert(ret == 0);
    }
  }

  void lock() const { pthread_mutex_lock(&pthread_mutex_); }

  void unlock() const { pthread_mutex_unlock(&pthread_mutex_); }

 private:
  mutable pthread_mutex_t pthread_mutex_;
  mutable bool initialized_;
};

Mutex::Mutex() : impl_(new Mutex::impl()) {}

void Mutex::lock() const { impl_->lock(); }

void Mutex::unlock() const { impl_->unlock(); }

}}} // facebook::thrift::concurrency

