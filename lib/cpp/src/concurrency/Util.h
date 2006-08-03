#if !defined(_concurrency_Util_h_)
#define _concurrency_Util_h_ 1

#include <config.h>

#include <assert.h>
#include <stddef.h>
#include <time.h>

namespace facebook { namespace thrift { namespace concurrency { 

/**  Utility methods

     This class contains basic utility methods for converting time formats, and other common platform-dependent concurrency operations.
     It should not be included in API headers for other concurrency library headers, since it will, by definition, pull in all sorts of
     horrid platform dependent crap.  Rather it should be inluded directly in concurrency library implementation source. 

     @author marc
     @version $Id:$ */

class Util {

  static const long long NS_PER_S = 1000000000LL;

  static const long long MS_PER_S = 1000LL;

  static const long long NS_PER_MS = 1000000LL;

 public:

  /** Converts timespec to milliseconds

      @param struct timespec& result
      @param time or duration in milliseconds */

  static void toTimespec(struct timespec& result, long long value) {

    result.tv_sec = value / MS_PER_S; // ms to s
    
    result.tv_nsec = (value % MS_PER_S) * NS_PER_MS; // ms to ns
  }

  /** Converts timespec to milliseconds */

  static const void toMilliseconds(long long& result, const struct timespec& value) {

    result = (value.tv_sec * MS_PER_S) + (value.tv_nsec / NS_PER_MS) + (value.tv_nsec % NS_PER_MS >= 500000 ? 1 : 0) ;
  }

  /** Get current time as milliseconds from epoch */

  static const long long currentTime() {

#if defined(HAVE_CLOCK_GETTIME)

    struct timespec now;

    assert(clock_gettime(CLOCK_REALTIME, &now) == 0);

    return (now.tv_sec * MS_PER_S) + (now.tv_nsec / NS_PER_MS) + (now.tv_nsec % NS_PER_MS >= 500000 ? 1 : 0) ;

#elif defined(HAVE_GETTIMEOFDAY)

    struct timeval now;

    assert(gettimeofday(&now, NULL) == 0);

    return (((long long)now.tv_sec) * MS_PER_S) + (now.tv_usec / MS_PER_S) + (now.tv_usec % MS_PER_S >= 500 ? 1 : 0);

#endif // defined(HAVE_GETTIMEDAY)
  }
};

}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_Util_h_)
