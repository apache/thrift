// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_CONCURRENCY_MUTEX_H_
#define _THRIFT_CONCURRENCY_MUTEX_H_ 1

namespace facebook { namespace thrift { namespace concurrency { 

/**
 * A simple mutex class
 *
 * @author marc
 * @version $Id:$
 */
class Mutex {
 public:
  Mutex();
  virtual ~Mutex() {}
  virtual void lock() const;
  virtual void unlock() const;

 private:
  class impl;
  impl* impl_;
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
  impl* impl_;
};

class Guard{
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


}}} // facebook::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_MUTEX_H_
