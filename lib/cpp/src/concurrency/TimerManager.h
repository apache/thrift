#if !defined(_concurrency_TimerManager_h_)
#define _concurrency_TimerManager_h_ 1

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

  virtual ~TimerManager() = 0;

  virtual const ThreadFactory* threadFactory() const = 0;

  virtual void threadFactory(const ThreadFactory* value) = 0;

  virtual size_t taskCount() const  = 0;

  /** Adds a task to be executed at some time in the future by a worker thread.
      
      @param task The task to execute
      @param timeout Time in milliseconds to delay before executing task */

  virtual void add(Runnable* task, long long timeout) = 0;

  /** Adds a task to be executed at some time in the future by a worker thread.
      
      @param task The task to execute
      @param timeout Absolute time in the future to execute task. */ 

  virtual void add(Runnable* task, const struct timespec& timeout) = 0;

  /** Removes a pending task */

  virtual void remove(Runnable* task) = 0;

 private:
  
  const ThreadFactory* _threadFactory;
  
  class Task;

  friend class Task;

  std::multimap<long long, Task*> _taskMap;

  size_t _taskCount;

  long long _nextTimeout;

  Monitor _monitor;

  class Dispatcher;

  friend class Dispatcher;

  Dispatcher* _dispatcher;
};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_TimerManager_h_)
