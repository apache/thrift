#include "Monitor.h" 

#include <assert.h>
#include <errno.h>
#include <pthread.h>

namespace facebook { namespace thrift { namespace concurrency { 

/** Monitor implementation using the POSIX pthread library
    
    @author marc
    @version $Id$ */

class Monitor::Impl {

 public:

  Impl() : 
    mutexInitialized(false) {
    
    /* XXX
       Need to fix this to handle failures without leaking.  */

    assert(pthread_mutex_init(&_pthread_mutex, NULL) == 0);

    mutexInitialized = true;

    assert(pthread_cond_init(&_pthread_cond, NULL) == 0);
  }

  ~Impl() {

    if(mutexInitialized) {

      mutexInitialized = false;

      assert(pthread_mutex_destroy(&_pthread_mutex) == 0);
    }

    if(condInitialized) {

      condInitialized = false;

      assert(pthread_cond_destroy(&_pthread_cond) == 0);
    }
  }

  void lock() const {pthread_mutex_lock(&_pthread_mutex);}

  void unlock() const {pthread_mutex_unlock(&_pthread_mutex);}

  void wait(long long timeout) const {

    // XXX Need to assert that caller owns mutex

    if(timeout == 0LL) {

      pthread_cond_wait(&_pthread_cond, &_pthread_mutex);

    } else {

      struct timespec abstime;

      toAbsoluteTimespec(abstime, timeout);

      int result  = pthread_cond_timedwait(&_pthread_cond, &_pthread_mutex, &abstime);

      if(result == ETIMEDOUT) {

	// XXX If result is timeout need to throw timeout exception
      }
    }
  }

  void notify() {

    // XXX Need to assert that caller owns mutex

    assert(pthread_cond_signal(&_pthread_cond) == 0);
  }

  void notifyAll() {

    // XXX Need to assert that caller owns mutex

    assert(pthread_cond_broadcast(&_pthread_cond) == 0);
  }

private:

  /** Converts relative timeout specified as a duration in milliseconds to a struct timespec structure
      specifying current time plus timeout 

      @param timeout time to delay in milliseconds
      @return struct timespec current time plus timeout  */

  static const void toAbsoluteTimespec(struct timespec& result, long long timeout) {

    // XXX Darwin doesn't seem to have any readily useable hi-res clock.

    time_t seconds; 

    assert(time(&seconds) != (time_t)-1);

    seconds+= (timeout / 1000);

    long nanoseconds = (timeout % 1000) * 1000000;

    result.tv_sec = seconds + (nanoseconds / 1000000000);

    result.tv_nsec = nanoseconds % 1000000000;
  }

  mutable pthread_mutex_t _pthread_mutex;

  mutable bool mutexInitialized;

  mutable pthread_cond_t _pthread_cond;

  mutable bool condInitialized;
};

Monitor::Monitor() : _impl(new Monitor::Impl()) {}

      Monitor::~Monitor() { delete _impl;}

void Monitor::lock() const {_impl->lock();}

void Monitor::unlock() const {_impl->unlock();}

void Monitor::wait(long long timeout) const {_impl->wait(timeout);}

void Monitor::notify() const {_impl->notify();}

void Monitor::notifyAll() const {_impl->notifyAll();}

}}} // facebook::thrift::concurrency

