// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "Util.h"

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#if defined(HAVE_CLOCK_GETTIME)
#include <time.h>
#elif defined(HAVE_GETTIMEOFDAY)
#include <sys/time.h>
#endif // defined(HAVE_CLOCK_GETTIME)

namespace apache { namespace thrift { namespace concurrency {

const int64_t Util::currentTime() {
  int64_t result;

#if defined(HAVE_CLOCK_GETTIME)
  struct timespec now;
  int ret = clock_gettime(CLOCK_REALTIME, &now);
  assert(ret == 0);
  toMilliseconds(result, now);
#elif defined(HAVE_GETTIMEOFDAY)
  struct timeval now;
  int ret = gettimeofday(&now, NULL);
  assert(ret == 0);
  toMilliseconds(result, now);
#else
#error "No high-precision clock is available."
#endif // defined(HAVE_CLOCK_GETTIME)

  return result;
}


}}} // apache::thrift::concurrency
