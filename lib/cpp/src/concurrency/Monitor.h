#ifndef _THRIFT_CONCURRENCY_MONITOR_H_
#define _THRIFT_CONCURRENCY_MONITOR_H_ 1

namespace facebook { namespace thrift { namespace concurrency { 

/**
 * A monitor is a combination mutex and condition-event.  Waiting and
 * notifying condition events requires that the caller own the mutex.  Mutex
 * lock and unlock operations can be performed independently of condition
 * events.  This is more or less analogous to java.lang.Object multi-thread
 * operations
 *
 * Note that all methods are const.  Monitors implement logical constness, not
 * bit constness.  This allows const methods to call monitor methods without
 * needing to cast away constness or change to non-const signatures.
 *
 * @author marc
 * @version $Id:$
 */
class Monitor {

 public:

  Monitor();

  virtual ~Monitor();

  virtual void lock() const;

  virtual void unlock() const;

  virtual void wait(long long timeout=0LL) const;

  virtual void notify() const;

  virtual void notifyAll() const;

 private:

  class Impl;

  Impl* _impl;
};

class Synchronized {
 public:
  
 Synchronized(const Monitor& value) : _monitor(value) {
    _monitor.lock();
  }

  ~Synchronized() {
    _monitor.unlock();
  }

 private:
  const Monitor& _monitor;
};


}}} // facebook::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_MONITOR_H_
