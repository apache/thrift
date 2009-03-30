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

#include "Mutex.h"

#include <assert.h>
#include <pthread.h>

using boost::shared_ptr;

namespace apache { namespace thrift { namespace concurrency {

/**
 * Implementation of Mutex class using POSIX mutex
 *
 * @version $Id:$
 */
class Mutex::impl {
 public:
  impl(Initializer init) : initialized_(false) {
    init(&pthread_mutex_);
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

Mutex::Mutex(Initializer init) : impl_(new Mutex::impl(init)) {}

void Mutex::lock() const { impl_->lock(); }

bool Mutex::trylock() const { return impl_->trylock(); }

void Mutex::unlock() const { impl_->unlock(); }

void Mutex::DEFAULT_INITIALIZER(void* arg) {
  pthread_mutex_t* pthread_mutex = (pthread_mutex_t*)arg;
  int ret = pthread_mutex_init(pthread_mutex, NULL);
  assert(ret == 0);
}

static void init_with_kind(pthread_mutex_t* mutex, int kind) {
  pthread_mutexattr_t mutexattr;
  int ret = pthread_mutexattr_init(&mutexattr);
  assert(ret == 0);

  // Apparently, this can fail.  Should we really be aborting?
  ret = pthread_mutexattr_settype(&mutexattr, kind);
  assert(ret == 0);

  ret = pthread_mutex_init(mutex, &mutexattr);
  assert(ret == 0);

  ret = pthread_mutexattr_destroy(&mutexattr);
  assert(ret == 0);
}

#ifdef PTHREAD_ADAPTIVE_MUTEX_INITIALIZER_NP
void Mutex::ADAPTIVE_INITIALIZER(void* arg) {
  // From mysql source: mysys/my_thr_init.c
  // Set mutex type to "fast" a.k.a "adaptive"
  //
  // In this case the thread may steal the mutex from some other thread
  // that is waiting for the same mutex. This will save us some
  // context switches but may cause a thread to 'starve forever' while
  // waiting for the mutex (not likely if the code within the mutex is
  // short).
  init_with_kind((pthread_mutex_t*)arg, PTHREAD_MUTEX_ADAPTIVE_NP);
}
#endif

#ifdef PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
void Mutex::RECURSIVE_INITIALIZER(void* arg) {
  init_with_kind((pthread_mutex_t*)arg, PTHREAD_MUTEX_RECURSIVE_NP);
}
#endif


/**
 * Implementation of ReadWriteMutex class using POSIX rw lock
 *
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

void ReadWriteMutex::acquireRead() const { impl_->acquireRead(); }

void ReadWriteMutex::acquireWrite() const { impl_->acquireWrite(); }

bool ReadWriteMutex::attemptRead() const { return impl_->attemptRead(); }

bool ReadWriteMutex::attemptWrite() const { return impl_->attemptWrite(); }

void ReadWriteMutex::release() const { impl_->release(); }

}}} // apache::thrift::concurrency

