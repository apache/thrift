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

#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift { namespace concurrency {

/**
 * A simple mutex class
 *
 * @version $Id:$
 */
class Mutex {
 public:
  typedef void (*Initializer)(void*);

  Mutex(Initializer init = DEFAULT_INITIALIZER);
  virtual ~Mutex() {}
  virtual void lock() const;
  virtual bool trylock() const;
  virtual void unlock() const;

  static void DEFAULT_INITIALIZER(void*);
  static void ADAPTIVE_INITIALIZER(void*);
  static void RECURSIVE_INITIALIZER(void*);

 private:

  class impl;
  boost::shared_ptr<impl> impl_;
};

class ReadWriteMutex {
public:
  ReadWriteMutex();
  virtual ~ReadWriteMutex() {}

  // these get the lock and block until it is done successfully
  virtual void acquireRead() const;
  virtual void acquireWrite() const;

  // these attempt to get the lock, returning false immediately if they fail
  virtual bool attemptRead() const;
  virtual bool attemptWrite() const;

  // this releases both read and write locks
  virtual void release() const;

private:

  class impl;
  boost::shared_ptr<impl> impl_;
};

class Guard {
 public:
  Guard(const Mutex& value) : mutex_(value) {
    mutex_.lock();
  }
  ~Guard() {
    mutex_.unlock();
  }

 private:
  const Mutex& mutex_;
};

class RWGuard {
  public:
    RWGuard(const ReadWriteMutex& value, bool write = 0) : rw_mutex_(value) {
      if (write) {
        rw_mutex_.acquireWrite();
      } else {
        rw_mutex_.acquireRead();
      }
    }
    ~RWGuard() {
      rw_mutex_.release();
    }
  private:
    const ReadWriteMutex& rw_mutex_;
};


// A little hack to prevent someone from trying to do "Guard(m);"
// Such a use is invalid because the temporary Guard object is
// destoryed at the end of the line, releasing the lock.
// Sorry for polluting the global namespace, but I think it's worth it.
#define Guard(m) incorrect_use_of_Guard(m)
#define RWGuard(m) incorrect_use_of_RWGuard(m)


}}} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_MUTEX_H_
