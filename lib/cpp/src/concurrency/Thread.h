#if !defined(_concurrency_Thread_h_)
#define _concurrency_Thread_h_ 1

#include <boost/shared_ptr.hpp>
#include <boost/weak_ptr.hpp>

namespace facebook { namespace thrift { namespace concurrency { 

using namespace boost;

class Thread;

/** Minimal runnable class.  More or less analogous to java.lang.Runnable. 

    @author marc
    @version $Id:$ */

class Runnable {

 public:
  
  virtual ~Runnable() {};

  virtual void run() = 0;

  /** Gets the thread object that is hosting this runnable object  - can return an empty shared pointer if no references remain on thet thread  object */

  virtual shared_ptr<Thread> thread() {return _thread.lock();}

  /** Sets the thread that is executing this object.  This is only meant for use by concrete implementations of Thread. */

  virtual void thread(shared_ptr<Thread> value) {_thread = value;}

 private:

  weak_ptr<Thread> _thread;
};

/** Minimal thread class.  Returned by thread factory bound to a Runnable object and ready to start execution.  More or less analogous to java.lang.Thread
    (minus all the thread group, priority, mode and other baggage, since that is difficult to abstract across platforms and is left for platform-specific
    ThreadFactory implemtations to deal with - @see facebook::thrift::concurrency::ThreadFactory) */
    
 
class Thread {
  
 public:

  virtual ~Thread() {};

  /** Starts the thread.  Does platform specific thread creation and configuration then invokes the run method of the Runnable object bound to this 
      thread. */

  virtual void start() = 0;

  /** Join this thread

      Current thread blocks until this target thread completes. */

  virtual void join() = 0;

  /** Gets the runnable object this thread is hosting */
  
  virtual shared_ptr<Runnable> runnable() const {return _runnable;}

 protected:

  virtual void runnable(shared_ptr<Runnable> value) {_runnable = value;}

 private:
  shared_ptr<Runnable> _runnable;
};

/** Factory to create platform-specific thread object and bind them to Runnable object for execution */

class ThreadFactory {

 public:

  virtual ~ThreadFactory() {}

  virtual shared_ptr<Thread> newThread(shared_ptr<Runnable> runnable) const = 0;
};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_Thread_h_)
