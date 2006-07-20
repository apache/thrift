#if !defined(_concurrency_TimerManager_h_)
#define _concurrency_TimerManager_h_ 1

#include "Exception.h"
#include "Monitor.h"
#include "Thread.h"

#include <map>

#include <time.h>

namespace facebook { namespace thrift { namespace concurrency { 

/** Timer Manager 
	  
    This class dispatches timer tasks when they fall due.
	  
    @author marc
    @version $Id:$ */

class TimerManager {

 public:

  TimerManager();

  virtual ~TimerManager();

  virtual const ThreadFactory* threadFactory() const;

  virtual void threadFactory(const ThreadFactory* value);

  /** Starts the timer manager service 

      @throws IllegalArgumentException Missing thread factory attribute */

  virtual void start();

  /** Stops the timer manager service */

  virtual void stop();

  virtual size_t taskCount() const ;

  /** Adds a task to be executed at some time in the future by a worker thread.
      
      @param task The task to execute
      @param timeout Time in milliseconds to delay before executing task */

  virtual void add(Runnable* task, long long timeout);

  /** Adds a task to be executed at some time in the future by a worker thread.
      
      @param task The task to execute
      @param timeout Absolute time in the future to execute task. */ 

  virtual void add(Runnable* task, const struct timespec& timeout);

  /** Removes a pending task 

      @throws NoSuchTaskException Specified task doesn't exist.  It was either processed already or this call was made for a task that
      was never added to this timer

      @throws UncancellableTaskException Specified task is already being executed or has completed execution. */

  virtual void remove(Runnable* task);

  enum STATE {
    UNINITIALIZED,
    STARTING,
    STARTED,
    STOPPING,
    STOPPED
  };
  
  virtual const STATE state() const;

 private:
  
  const ThreadFactory* _threadFactory;
  
  class Task;

  friend class Task;

  std::multimap<long long, Task*> _taskMap;

  size_t _taskCount;

  Monitor _monitor;

  STATE _state;

  class Dispatcher;

  friend class Dispatcher;

  Dispatcher* _dispatcher;

  Thread* _dispatcherThread;

};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_TimerManager_h_)
