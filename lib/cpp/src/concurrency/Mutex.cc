#include "Mutex.h"

#include <assert.h>
#include <pthread.h>

namespace facebook { namespace thrift { namespace concurrency { 

/** 
 * Implementation of Mutex class using POSIX mutex
 *
 * @author marc
 * @version $Id:$
 */
class Mutex::impl {
 public:
  impl() : initialized(false) {
    assert(pthread_mutex_init(&_pthread_mutex, NULL) == 0);
    initialized = true;
  }

  ~impl() {
    if (initialized) {
      initialized = false;
      assert(pthread_mutex_destroy(&_pthread_mutex) == 0);
    }
  }

  void lock() const { pthread_mutex_lock(&_pthread_mutex); }

  void unlock() const { pthread_mutex_unlock(&_pthread_mutex); }

 private:
  mutable pthread_mutex_t _pthread_mutex;
  mutable bool initialized;
};

Mutex::Mutex() : _impl(new Mutex::impl()) {}

void Mutex::lock() const { _impl->lock(); }

void Mutex::unlock() const { _impl->unlock(); }

}}} // facebook::thrift::concurrency

