#if !defined(_concurrency_Util_h_)
#define _concurrency_Util_h_ 1

#include <assert.h>
#include <sys/time.h>

namespace facebook { namespace thrift { namespace concurrency { 

/**  Utility methods

     This class contains basic utility methods for converting time formats, and other common platform-dependent concurrency operations.
     It should not be included in API headers for other concurrency library headers, since it will, by definition, pull in all sorts of
     horrid platform dependent crap.  Rather it should be inluded directly in concurrency library implementation source. 

     @author marc
     @version $Id:$ */

class Util {

 public:

  /** Converts timespec to milliseconds

      @param struct timespec& result
      @param time or duration in milliseconds */

  static void toTimespec(struct timespec& result, long long value) {
    
    result.tv_sec = value / 1000; // ms to s
    
    result.tv_nsec = (value % 1000) * 1000000; // ms to ns
  }

  /** Converts timespec to milliseconds */

  static const void toMilliseconds(long long& result, const struct timespec& value) {

    result = value.tv_sec * 1000 + value.tv_nsec / 1000000;
  }

  /** Get current time as milliseconds from epoch */

  static const long long currentTime() {

    struct timeval now;

    assert(gettimeofday(&now, NULL) == 0);

    return ((long long)now.tv_sec) * 1000LL + now.tv_usec / 1000;
  }
};


}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_Util_h_)
