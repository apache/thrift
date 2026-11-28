/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#define BOOST_TEST_MODULE TUnixSocketOptionsTest
#include <boost/test/unit_test.hpp>

#include <thrift/thrift-config.h>

#if defined(__linux__)

#include <atomic>
#include <cerrno>
#include <dlfcn.h>
#include <string>
#include <sys/socket.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TSocket.h>
#include <unistd.h>
#ifdef THRIFT_TEST_NONBLOCKING_SERVER_SOCKET
#include <thrift/transport/TNonblockingServerSocket.h>
#endif

using apache::thrift::transport::TServerSocket;
using apache::thrift::transport::TSocket;

namespace {
// Counts SO_LINGER calls made on AF_UNIX sockets and fails them the way WSL does
// (THRIFT-5374). Linux itself accepts the option, so the call has to be intercepted.
std::atomic<int> unixLingerCalls{0};
} // namespace

extern "C" int setsockopt(int fd, int level, int optname, const void* optval, socklen_t optlen) {
  using Fn = int (*)(int, int, int, const void*, socklen_t);
  static Fn real = reinterpret_cast<Fn>(dlsym(RTLD_NEXT, "setsockopt"));
  if (level == SOL_SOCKET && optname == SO_LINGER) {
    sockaddr_storage ss;
    socklen_t len = sizeof(ss);
    if (getsockname(fd, reinterpret_cast<sockaddr*>(&ss), &len) == 0 && ss.ss_family == AF_UNIX) {
      ++unixLingerCalls;
      errno = ENOPROTOOPT;
      return -1;
    }
  }
  return real(fd, level, optname, optval, optlen);
}

BOOST_AUTO_TEST_SUITE(TUnixSocketOptionsTest)

BOOST_AUTO_TEST_CASE(test_server_listen_does_not_set_linger_on_unix_socket) {
  std::string path = "/tmp/thrift5374-" + std::to_string(::getpid()) + ".sock";
  ::unlink(path.c_str());
  unixLingerCalls = 0;
  {
    TServerSocket server(path);
    BOOST_CHECK_NO_THROW(server.listen());
    BOOST_CHECK_EQUAL(unixLingerCalls.load(), 0);

    TSocket client(path);
    BOOST_CHECK_NO_THROW(client.open());
    BOOST_CHECK_EQUAL(unixLingerCalls.load(), 0);
    client.close();
    server.close();
  }
  ::unlink(path.c_str());
}

#ifdef THRIFT_TEST_NONBLOCKING_SERVER_SOCKET
BOOST_AUTO_TEST_CASE(test_nonblocking_server_listen_does_not_set_linger_on_unix_socket) {
  std::string path = "/tmp/thrift5374-nb-" + std::to_string(::getpid()) + ".sock";
  ::unlink(path.c_str());
  unixLingerCalls = 0;
  {
    apache::thrift::transport::TNonblockingServerSocket server(path);
    BOOST_CHECK_NO_THROW(server.listen());
    BOOST_CHECK_EQUAL(unixLingerCalls.load(), 0);
    server.close();
  }
  ::unlink(path.c_str());
}
#endif

BOOST_AUTO_TEST_SUITE_END()

#else
BOOST_AUTO_TEST_SUITE(TUnixSocketOptionsTest)
BOOST_AUTO_TEST_CASE(test_skipped) {
  BOOST_CHECK(true);
}
BOOST_AUTO_TEST_SUITE_END()
#endif
