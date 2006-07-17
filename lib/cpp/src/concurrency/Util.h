#if !defined(_concurrency_Util_h_)
#define _concurrency_Util_h_ 1

#include <assert.h>
#include <time.h>

namespace facebook { namespace thrift { namespace concurrency { 

/**  Utility methods

     This class contains basic utility methods for converting time formats, and other common platform-dependent concurrency operations.
     It should not be included in API headers for other concurrency library headers, since it will, by definition, pull in all sorts of
     horrid platform dependent crap.  Rather it should be inluded directly in concurrency library implementation source. 

     @author marc
     @version $Id:$ */

class Util {

 public:

  /** Converts relative timeout specified as a duration in milliseconds to a struct timespec structure
      specifying current time plus timeout 

      @param struct timespec& current time plus timeout result  
      @param timeout time to delay in milliseconds */

  static const void toAbsoluteTimespec(struct timespec& result, long long value) {
    
    // XXX Darwin doesn't seem to have any readily useable hi-res clock.
    
    time_t seconds; 
    
    assert(time(&seconds) != (time_t)-1);
    
    seconds+= (value / 1000);
    
    long nanoseconds = (value % 1000) * 1000000;
    
    result.tv_sec = seconds + (nanoseconds / 1000000000);
    
    result.tv_nsec = nanoseconds % 1000000000;
  }

  /** Converts absolute timespec to milliseconds from epoch */

  static const void toMilliseconds(long long& result, const struct timespec& value) {

    result = value.tv_sec * 1000 + value.tv_nsec * 1000000;
  }

  /** Get current time as milliseconds from epoch */

  static const long long currentTime() {

    time_t now;

    time(&now);

    return (long long)now * 1000;
  }
};


}}} // facebook::thrift::concurrency

#endif // !defined(_concurrency_Util_h_)
