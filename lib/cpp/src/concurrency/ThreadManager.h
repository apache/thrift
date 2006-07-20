#if !defined(_concurrency_ThreadManager_h_)
#define _concurrency_ThreadManager_h_ 1

#include <sys/types.h>

#include "Thread.h"

namespace facebook { namespace thrift { namespace concurrency { 

/** Thread Pool Manager and related classes

    @author marc
    @version $Id:$ */

class ThreadManager;

/** ThreadManager class
    
    This class manages a pool of threads.  It uses a ThreadFactory to create threads.  It never actually creates or destroys worker threads, rather
    it maintains statistics on number of idle threads, number of active threads, task backlog, and average wait and service times and informs the
    PoolPolicy object bound to instances of this manager of interesting transitions.  It is then up the PoolPolicy object to decide if the thread pool
    size needs to be adjusted and call this object addWorker and removeWorker methods to make changes.

    This design allows different policy implementations to used this code to handle basic worker thread management and worker task execution and focus on
    policy issues.  The simplest policy, StaticPolicy, does nothing other than create a fixed number of threads. */

class ThreadManager {

 public:

  ThreadManager() {}

  virtual ~ThreadManager() {}

  /** Starts the thread manager.  Verifies all attributes have been properly initialized, then allocates necessary resources to begin operation */
  
  virtual void start() = 0;

  /** Stops the thread manager.  Aborts all remaining unprocessed task, shuts down all created worker threads, and realeases all allocated resources.
      This method blocks for all worker threads to complete, thus it can potentially block forever if a worker thread is running a task that 
      won't terminate. */

  virtual void stop() = 0;

  enum STATE {
    UNINITIALIZED,
    STARTING,
    STARTED,
    STOPPING,
    STOPPED
  };
  
  virtual const STATE state() const = 0;

  virtual const ThreadFactory* threadFactory() const = 0;

  virtual void threadFactory(const ThreadFactory* value) = 0;

  virtual void addWorker(size_t value=1) = 0;

  virtual void removeWorker(size_t value=1) = 0;

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

  static ThreadManager* newThreadManager();

  /** Creates a simple thread manager the uses count number of worker threads */

  static ThreadManager* newSimpleThreadManager(size_t count=4);

  class Task;
  
  class Worker;

  class Impl;
};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_ThreadManager_h_)
