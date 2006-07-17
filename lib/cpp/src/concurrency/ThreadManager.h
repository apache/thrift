#if !defined(_concurrency_ThreadManager_h_)
#define _concurrency_ThreadManager_h_ 1

#include <sys/types.h>

#include "Thread.h"

namespace facebook { namespace thrift { namespace concurrency { 

/** Thread Pool Manager and related classes

    @author marc
    @version $Id:$ */

class ThreadManager;

/** PoolPolicy class 

    Tracks performance of ThreadManager object and makes desired changes in thread pool count if any. */

class PoolPolicy {

 public:

  PoolPolicy() {}

  virtual ~PoolPolicy() {}

  virtual void onEmpty(ThreadManager* source) const = 0;

  virtual void onLowWatermark(ThreadManager* source) const = 0;

  virtual void onHighWatermark(ThreadManager* source) const = 0;

};

class BasicPoolPolicy : public PoolPolicy {

 public:

  BasicPoolPolicy();

  virtual ~BasicPoolPolicy();

  virtual void onEmpty(ThreadManager* source) const;

  virtual void onLowWatermark(ThreadManager* source) const;

  virtual void onHighWatermark(ThreadManager* source) const;

 private:

  class Impl;

  Impl* _impl;
};

/** ThreadManager class
    
    This class manages a pool of threads.  It uses a ThreadFactory to create threads.  It never actually creates or destroys worker threads, rather
    it maintains statistics on number of idle threads, number of active threads, task backlog, and average wait and service times and informs the
    PoolPolicy object bound to instances of this manager of interesting transitions.  It is then up the PoolPolicy object to decide if the thread pool
    size needs to be adjusted and call this object addThread and removeThread methods to make changes.

    This design allows different policy implementations to used this code to handle basic worker thread management and worker task execution and focus on
    policy issues.  The simplest policy, StaticPolicy, does nothing other than create a fixed number of threads. */

class ThreadManager {

 public:

  ThreadManager(size_t highWatermark=4, size_t lowWatermark=2) {};

  virtual ~ThreadManager() {};

  virtual const PoolPolicy* poolPolicy() const = 0;

  virtual void poolPolicy(const PoolPolicy* value) = 0;

  virtual const ThreadFactory* threadFactory() const = 0;

  virtual void threadFactory(const ThreadFactory* value) = 0;

  virtual size_t highWatermark() const = 0;

  virtual void highWatermark(size_t value) = 0;

  virtual size_t lowWatermark() const = 0;

  virtual void lowWatermark(size_t value) = 0;

  virtual void addThread(size_t value=1) = 0;

  virtual void removeThread(size_t value=1) = 0;

  /** Gets the current number of idle worker threads */

  virtual size_t idleWorkerCount() const = 0;

  /** Gets the current number of total worker threads */

  virtual size_t workerCount() const = 0;

  /** Gets the current number of pending tasks */

  virtual size_t pendingTaskCount() const  = 0;

  /** Gets the current number of pending and executing tasks */

  virtual size_t totalTaskCount() const = 0;

  /** Adds a task to be execued at some time in the future by a worker thread. */

  virtual void add(Runnable* value) = 0;

  /** Removes a pending task */

  virtual void remove(Runnable* task) = 0;

  static ThreadManager* newThreadManager(size_t lowWatermark=2, size_t highWatermark=4);

  class Task;
  
  class Worker;

  class Impl;
};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_ThreadManager_h_)
