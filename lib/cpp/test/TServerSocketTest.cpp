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

#include <thrift/thrift-config.h>

#include <boost/test/unit_test.hpp>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TServerSocket.h>
#include <thrift/transport/TServerSocketErrors.h>
#include <chrono>
#include <memory>
#include <thread>
#include <vector>
#include "TTransportCheckThrow.h"
#include <iostream>
#include <cerrno>
#if !defined(_WIN32) && defined(HAVE_SYS_RESOURCE_H)
#include <fcntl.h>
#include <sys/resource.h>
#include <unistd.h>
#endif

using apache::thrift::transport::TServerSocket;
using apache::thrift::transport::TSocket;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::detail::isConnectionError;
using apache::thrift::transport::detail::isResourceExhaustion;
using apache::thrift::transport::detail::nextBackoffMs;
using std::shared_ptr;

BOOST_AUTO_TEST_SUITE(TServerSocketTest)

BOOST_AUTO_TEST_CASE(test_bind_to_address) {
  TServerSocket sock1("localhost", 0);
  sock1.listen();
  BOOST_CHECK(sock1.isOpen());
  int port = sock1.getPort();
  TSocket clientSock("localhost", port);
  clientSock.open();
  shared_ptr<TTransport> accepted = sock1.accept();
  accepted->close();
  sock1.close();

  std::cout << "An error message from getaddrinfo on the console is expected:" << '\n';
  TServerSocket sock2("257.258.259.260", 0);
  BOOST_CHECK_THROW(sock2.listen(), TTransportException);
  sock2.close();
}

BOOST_AUTO_TEST_CASE(test_listen_invalid_port) {
  TServerSocket sock1(-1);
  TTRANSPORT_CHECK_THROW(sock1.listen(), TTransportException::BAD_ARGS);
  BOOST_CHECK(!sock1.isOpen());

  TServerSocket sock2(65536);
  TTRANSPORT_CHECK_THROW(sock2.listen(), TTransportException::BAD_ARGS);
  BOOST_CHECK(!sock1.isOpen());
}

BOOST_AUTO_TEST_CASE(test_close_before_listen) {
  TServerSocket sock1("localhost", 0);
  sock1.close();
  BOOST_CHECK(!sock1.isOpen());
}

BOOST_AUTO_TEST_CASE(test_get_port) {
  TServerSocket sock1("localHost", 888);
  BOOST_CHECK_EQUAL(888, sock1.getPort());
}

BOOST_AUTO_TEST_CASE(test_accept_error_classes) {
  // accept() goes back to poll() on these: only the one connection is lost.
  BOOST_CHECK(isConnectionError(THRIFT_EINTR));
  BOOST_CHECK(isConnectionError(THRIFT_EAGAIN));
  BOOST_CHECK(isConnectionError(THRIFT_EWOULDBLOCK));
#ifdef _WIN32
  BOOST_CHECK(isConnectionError(WSAECONNRESET));
  BOOST_CHECK(isConnectionError(WSAECONNABORTED));
  BOOST_CHECK(isResourceExhaustion(WSAEMFILE));
  BOOST_CHECK(isResourceExhaustion(WSAENOBUFS));
  BOOST_CHECK(!isConnectionError(WSAENOTSOCK));
  BOOST_CHECK(!isResourceExhaustion(WSAENOTSOCK));
#else
  BOOST_CHECK(isConnectionError(ECONNABORTED));
#ifdef EPROTO
  BOOST_CHECK(isConnectionError(EPROTO));
#endif
#ifdef __linux__
  BOOST_CHECK(isConnectionError(EPERM));
  BOOST_CHECK(isConnectionError(ENETDOWN));
  BOOST_CHECK(isConnectionError(ENETUNREACH));
  BOOST_CHECK(isConnectionError(EHOSTUNREACH));
  BOOST_CHECK(isConnectionError(EHOSTDOWN));
  BOOST_CHECK(isConnectionError(ENONET));
  BOOST_CHECK(isConnectionError(ENOPROTOOPT));
  BOOST_CHECK(isConnectionError(EOPNOTSUPP));
#endif
  // accept() backs off and retries on these.
  BOOST_CHECK(isResourceExhaustion(EMFILE));
  BOOST_CHECK(isResourceExhaustion(ENFILE));
  BOOST_CHECK(isResourceExhaustion(ENOBUFS));
  BOOST_CHECK(isResourceExhaustion(ENOMEM));
  // accept() throws on anything else, such as a listening socket that is no longer valid.
  BOOST_CHECK(!isConnectionError(EBADF));
  BOOST_CHECK(!isResourceExhaustion(EBADF));
  BOOST_CHECK(!isConnectionError(EINVAL));
  BOOST_CHECK(!isResourceExhaustion(EINVAL));
#endif
}

BOOST_AUTO_TEST_CASE(test_accept_backoff_doubles_up_to_one_second) {
  const int expected[] = {5, 10, 20, 40, 80, 160, 320, 640, 1000, 1000, 1000};
  int backoffMs = 0;
  for (int step : expected) {
    backoffMs = nextBackoffMs(backoffMs);
    BOOST_CHECK_EQUAL(step, backoffMs);
  }
}

#if !defined(_WIN32) && defined(HAVE_SYS_RESOURCE_H)
// Sends a byte: TCP_DEFER_ACCEPT (Linux) holds accept() until data arrives.
void connectAndSend(TSocket& socket) {
  socket.open();
  uint8_t byte = 0;
  socket.write(&byte, 1);
  socket.flush();
}

// Opens descriptors until EMFILE. Process-wide: tests must run serially.
class DescriptorExhaustion {
public:
  DescriptorExhaustion() {
    BOOST_REQUIRE_EQUAL(0, getrlimit(RLIMIT_NOFILE, &saved_));
    struct rlimit low = saved_;
    if (low.rlim_cur == RLIM_INFINITY || low.rlim_cur > 256) {
      low.rlim_cur = 256;
    }
    BOOST_REQUIRE_EQUAL(0, setrlimit(RLIMIT_NOFILE, &low));
    int fd = ::open("/dev/null", O_RDONLY);
    if (fd >= 0) {
      int base = fd;
      do {
        fds_.push_back(fd);
      } while ((fd = dup(base)) >= 0);
    }
    exhausted_ = (errno == EMFILE);
  }
  bool exhausted() const { return exhausted_; }
  ~DescriptorExhaustion() { release(); }
  void release() {
    for (int fd : fds_) {
      ::close(fd);
    }
    fds_.clear();
    BOOST_WARN_EQUAL(0, setrlimit(RLIMIT_NOFILE, &saved_));
  }

private:
  struct rlimit saved_;
  std::vector<int> fds_;
  bool exhausted_;
};

BOOST_AUTO_TEST_CASE(test_accept_waits_out_descriptor_exhaustion) {
  TServerSocket server("localhost", 0);
  server.setAcceptTimeout(5000); // fail rather than hang if accept() never returns
  server.listen();
  TSocket client("localhost", server.getPort());
  connectAndSend(client);

  // Linux keeps the connection queued after EMFILE, macOS drops it; a second client covers both.
  TSocket lateClient("localhost", server.getPort());
  shared_ptr<TTransport> accepted;
  {
    DescriptorExhaustion exhaustion;
    BOOST_REQUIRE(exhaustion.exhausted());
    std::thread releaser([&exhaustion, &lateClient] {
      std::this_thread::sleep_for(std::chrono::milliseconds(200));
      exhaustion.release();
      connectAndSend(lateClient);
    });
    try {
      accepted = server.accept();
    } catch (TTransportException& ex) {
      BOOST_ERROR("accept() gave up while descriptors were exhausted: " << ex.what());
    }
    releaser.join();
  }
  BOOST_CHECK(accepted);
  if (accepted) {
    accepted->close();
  }
  lateClient.close();
  client.close();
  server.close();
}

BOOST_AUTO_TEST_CASE(test_interrupt_ends_accept_while_descriptors_are_exhausted) {
  TServerSocket server("localhost", 0);
  server.setAcceptTimeout(5000); // fail rather than hang if accept() never returns
  server.listen();
  TSocket client("localhost", server.getPort());
  connectAndSend(client);

  {
    DescriptorExhaustion exhaustion;
    BOOST_REQUIRE(exhaustion.exhausted());
    std::thread interrupter([&server] {
      std::this_thread::sleep_for(std::chrono::milliseconds(200));
      server.interrupt();
    });
    TTRANSPORT_CHECK_THROW(server.accept(), TTransportException::INTERRUPTED);
    interrupter.join();
  }
  client.close();
  server.close();
}
#endif

BOOST_AUTO_TEST_SUITE_END()
