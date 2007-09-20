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

  bool trylock() const { return (0 == pthread_mutex_trylock(&pthread_mutex_)); }

  void unlock() const { pthread_mutex_unlock(&pthread_mutex_); }

 private:
  mutable pthread_mutex_t pthread_mutex_;
  mutable bool initialized_;
};

Mutex::Mutex() : impl_(new Mutex::impl()) {}

Mutex::~Mutex() { delete impl_; }

void Mutex::lock() const { impl_->lock(); }

bool Mutex::trylock() const { return impl_->trylock(); }

void Mutex::unlock() const { impl_->unlock(); }

/** 
 * Implementation of ReadWriteMutex class using POSIX rw lock
 *
 * @author boz
 * @version $Id:$
 */
class ReadWriteMutex::impl {
public:
  impl() : initialized_(false) {
    int ret = pthread_rwlock_init(&rw_lock_, NULL);
    assert(ret == 0);
    initialized_ = true;
  }

  ~impl() {
    if(initialized_) {
      initialized_ = false;
      int ret = pthread_rwlock_destroy(&rw_lock_);
      assert(ret == 0);
    }
  }

  void acquireRead() const { pthread_rwlock_rdlock(&rw_lock_); }

  void acquireWrite() const { pthread_rwlock_wrlock(&rw_lock_); }

  bool attemptRead() const { return pthread_rwlock_tryrdlock(&rw_lock_); }

  bool attemptWrite() const { return pthread_rwlock_trywrlock(&rw_lock_); }

  void release() const { pthread_rwlock_unlock(&rw_lock_); }

private: 
  mutable pthread_rwlock_t rw_lock_;
  mutable bool initialized_;
};

ReadWriteMutex::ReadWriteMutex() : impl_(new ReadWriteMutex::impl()) {}

ReadWriteMutex::~ReadWriteMutex() { delete impl_; }

void ReadWriteMutex::acquireRead() const { impl_->acquireRead(); }

void ReadWriteMutex::acquireWrite() const { impl_->acquireWrite(); }

bool ReadWriteMutex::attemptRead() const { return impl_->attemptRead(); }

bool ReadWriteMutex::attemptWrite() const { return impl_->attemptWrite(); }

void ReadWriteMutex::release() const { impl_->release(); }

}}} // facebook::thrift::concurrency

