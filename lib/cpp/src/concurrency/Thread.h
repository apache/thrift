#if !defined(_concurrency_Thread_h_)
#define _concurrency_Thread_h_ 1

namespace facebook { namespace thrift { namespace concurrency { 

class Thread;

/** Minimal runnable class.  More or less analogous to java.lang.Runnable. 

    @author marc
    @version $Id:$ */

class Runnable {

 public:
  
  virtual ~Runnable() {};

  virtual void run() = 0;

  virtual Thread* thread() {return _thread;}

 private:

  /** Sets the thread that is executing this object.  This is only meant for use by concrete implementations of Thread. */

  friend class Thread;

  virtual void thread(Thread* value) {_thread = value;}

  Thread* _thread;
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
  
  virtual Runnable* runnable() const {return _runnable;}

 protected:

  virtual void runnable(Runnable* value, bool x=false) {_runnable = value; _runnable->thread(this);}

 private:
  
  Runnable* _runnable;
};

/** Factory to create platform-specific thread object and bind them to Runnable object for execution */

class ThreadFactory {

 public:

  virtual ~ThreadFactory() {}

  virtual Thread* newThread(Runnable* runnable) const = 0;
};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_Thread_h_)
