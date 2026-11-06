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
#include <cstring>
#include <string>
#include <thrift/transport/TSocket.h>
#ifndef _WIN32
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
#endif

using apache::thrift::transport::TSocket;

#ifndef _WIN32

// Fills the stack below the caller with bytes that are not printable, so
// that a buffer left uninitialized by getPeerHost() shows up as garbage.
#if defined(__GNUC__)
__attribute__((noinline))
#endif
static void poisonStack() {
  volatile char buf[16384];
  for (size_t i = 0; i < sizeof(buf); ++i) {
    buf[i] = static_cast<char>(0xFF);
  }
}

BOOST_AUTO_TEST_SUITE(TSocketPeerHostTest)

// THRIFT-5327: a link-local IPv6 peer has no reverse DNS entry and the
// reverse lookup can fail (for example EAI_AGAIN behind a stub resolver).
// getPeerHost() must then return the numeric address, not an uninitialized
// buffer.
BOOST_AUTO_TEST_CASE(test_peer_host_when_reverse_lookup_fails) {
  int fd = ::socket(AF_INET6, SOCK_STREAM, 0);
  if (fd < 0) {
    BOOST_TEST_MESSAGE("IPv6 is not available, skipping");
    return;
  }

  sockaddr_in6 peer;
  std::memset(&peer, 0, sizeof(peer));
  peer.sin6_family = AF_INET6;
  peer.sin6_port = htons(9090);
  BOOST_REQUIRE_EQUAL(inet_pton(AF_INET6, "fe80::1", &peer.sin6_addr), 1);

  {
    TSocket socket(fd);
    socket.setCachedAddress(reinterpret_cast<sockaddr*>(&peer), sizeof(peer));

    poisonStack();
    std::string host = socket.getPeerHost();

    BOOST_CHECK(!host.empty());
    for (char c : host) {
      BOOST_CHECK_MESSAGE(c >= 0x20 && c < 0x7F, "non-printable byte in peer host");
    }
    // The cached address is not touched by the lookup.
    BOOST_CHECK_EQUAL(socket.getPeerAddress(), "fe80::1");
  }
}

BOOST_AUTO_TEST_SUITE_END()

#endif
