#if !defined(_concurrency_mutex_h_)
#define _concurrency_mutex_h_ 1

namespace facebook { namespace thrift { namespace concurrency { 

/**  A simple mutex class

     @author marc
     @version $Id:$ */

class Mutex {

 public:

  Mutex();

  virtual ~Mutex() {}

  virtual void lock() const;

  virtual void unlock() const;

 private:

  class impl;

  impl* _impl;
};

class MutexMonitor {
 public:
  
  MutexMonitor(const Mutex& value) : _mutex(value) {
    _mutex.lock();
  }

  ~MutexMonitor() {
    _mutex.unlock();
  }

 private:
  const Mutex& _mutex;
};


}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_mutex_h_)
