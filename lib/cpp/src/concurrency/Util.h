// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_CONCURRENCY_UTIL_H_
#define _THRIFT_CONCURRENCY_UTIL_H_ 1

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <time.h>
#include <sys/time.h>

namespace apache { namespace thrift { namespace concurrency {

/**
 * Utility methods
 *
 * This class contains basic utility methods for converting time formats,
 * and other common platform-dependent concurrency operations.
 * It should not be included in API headers for other concurrency library
 * headers, since it will, by definition, pull in all sorts of horrid
 * platform dependent crap.  Rather it should be inluded directly in
 * concurrency library implementation source.
 *
 * @version $Id:$
 */
class Util {

  static const int64_t NS_PER_S = 1000000000LL;
  static const int64_t US_PER_S = 1000000LL;
  static const int64_t MS_PER_S = 1000LL;

  static const int64_t NS_PER_MS = NS_PER_S / MS_PER_S;
  static const int64_t US_PER_MS = US_PER_S / MS_PER_S;

 public:

  /**
   * Converts millisecond timestamp into a timespec struct
   *
   * @param struct timespec& result
   * @param time or duration in milliseconds
   */
  static void toTimespec(struct timespec& result, int64_t value) {
    result.tv_sec = value / MS_PER_S; // ms to s
    result.tv_nsec = (value % MS_PER_S) * NS_PER_MS; // ms to ns
  }

  static void toTimeval(struct timeval& result, int64_t value) {
    result.tv_sec = value / MS_PER_S; // ms to s
    result.tv_usec = (value % MS_PER_S) * US_PER_MS; // ms to us
  }

  /**
   * Converts struct timespec to milliseconds
   */
  static const void toMilliseconds(int64_t& result, const struct timespec& value) {
    result = (value.tv_sec * MS_PER_S) + (value.tv_nsec / NS_PER_MS);
    // round up -- int64_t cast is to avoid a compiler error for some GCCs
    if (int64_t(value.tv_nsec) % NS_PER_MS >= (NS_PER_MS / 2)) {
      ++result;
    }
  }

  /**
   * Converts struct timeval to milliseconds
   */
  static const void toMilliseconds(int64_t& result, const struct timeval& value) {
    result = (value.tv_sec * MS_PER_S) + (value.tv_usec / US_PER_MS);
    // round up -- int64_t cast is to avoid a compiler error for some GCCs
    if (int64_t(value.tv_usec) % US_PER_MS >= (US_PER_MS / 2)) {
      ++result;
    }
  }

  /**
   * Get current time as milliseconds from epoch
   */
  static const int64_t currentTime();
};

}}} // apache::thrift::concurrency

#endif // #ifndef _THRIFT_CONCURRENCY_UTIL_H_
