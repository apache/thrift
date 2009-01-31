// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

namespace apache { namespace thrift { namespace server {

int increase_max_fds(int max_fds=(1<<24))  {
  struct rlimit fdmaxrl;

  for(fdmaxrl.rlim_cur = max_fds, fdmaxrl.rlim_max = max_fds;
      max_fds && (setrlimit(RLIMIT_NOFILE, &fdmaxrl) < 0);
      fdmaxrl.rlim_cur = max_fds, fdmaxrl.rlim_max = max_fds) {
    max_fds /= 2;
  }

  return  fdmaxrl.rlim_cur;
}

}}} // apache::thrift::server
