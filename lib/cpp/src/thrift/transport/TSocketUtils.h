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

#ifndef _THRIFT_TRANSPORT_SOCKETUTILS_H_
#define _THRIFT_TRANSPORT_SOCKETUTILS_H_ 1

#include <memory>
#include <string>
#include <system_error>
#include <vector>

#include <sys/types.h>
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#include <thrift/transport/PlatformSocket.h>

namespace apache {
namespace thrift {

/**
 * A helper to resolve hostnames to struct addrinfo's -- and not leak memory.
 *
 * Use like this:
 *
 *   apache::thrift::AddressResolutionHelper addresses("localhost", "80");
 *
 *   for (auto addr : addresses.iterate()) {
 *      connect(sock, addr->ai_addr, addr->ai_addrlen);
 *      // ...
 *   }
 */
struct AddressResolutionHelper {

private:
  struct addrinfo_deleter {
    void operator()(addrinfo* addr) {
      ::freeaddrinfo(addr); // frees the whole list
    }
  };

public:
  using PtrOwnedList = std::unique_ptr<addrinfo, addrinfo_deleter>;

  struct Iter : std::iterator<std::forward_iterator_tag, const addrinfo*> {
    value_type ptr = nullptr;

    Iter() = default;
    Iter(const addrinfo* head) : ptr(head) {}

    value_type operator*() const { return ptr; }

    bool operator==(const Iter& other) { return this->ptr == other.ptr; }
    bool operator!=(const Iter& other) { return this->ptr != other.ptr; }

    operator bool() { return this->ptr != nullptr; }
    bool operator!() { return this->ptr == nullptr; }

    Iter& operator++() {
      if (ptr == nullptr) {
        throw std::out_of_range("won't go pass end of linked list");
      }
      ptr = ptr->ai_next;
      return *this;
    }
    Iter operator++(int) {
      Iter tmp(*this);
      ++(*this);
      return tmp;
    }
  };

  struct gai_error : std::error_category {
    virtual const char* name() const noexcept override { return "getaddrinfo"; }
    virtual std::string message(int code) const override { return THRIFT_GAI_STRERROR(code); }
  };

private:
  PtrOwnedList gai_results;

  addrinfo* query(const std::string& host, const std::string& port, int socktype, int flags) {
    addrinfo hints{};
    hints.ai_flags = flags;
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = socktype;

    addrinfo* head;
    int ret = ::getaddrinfo(host.empty() ? NULL : host.c_str(), port.c_str(), &hints, &head);
    if (ret == 0) {
      return head;
#ifdef _WIN32
    } else {
      throw std::system_error{THRIFT_GET_SOCKET_ERROR, std::system_category()};
#else
    } else if (ret == EAI_SYSTEM) {
      throw std::system_error{THRIFT_GET_SOCKET_ERROR, std::system_category()};
    } else {
      throw std::system_error{ret, gai_error()};
#endif
    }
  }

public:
  /**
   * Constructor. May block. Throws errors.
   *
   * @param port Port number, or service name, as a string.
   * @param socktype Socket type, SOCK_STREAM or SOCK_DGRAM.
   * @param flags Standard getaddrinfo() flags.
   */
  AddressResolutionHelper(const std::string& host,
                          const std::string& port, // pass "25" or "smtp" for port 25
                          int socktype = SOCK_STREAM,
                          int flags = AI_V4MAPPED | AI_ADDRCONFIG)
    : gai_results(query(host, port, socktype, flags)) {}

  AddressResolutionHelper() = default;

  /**
   * Manual query. May block. Throws errors.
   *
   * @param port Port number, or service name, as a string.
   * @param socktype Socket type, SOCK_STREAM or SOCK_DGRAM.
   * @param flags Standard getaddrinfo() flags.
   */
  AddressResolutionHelper& resolve(const std::string& host,
                                   const std::string& port, // pass "25" or "smtp" for port 25
                                   int socktype = SOCK_STREAM,
                                   int flags = AI_V4MAPPED | AI_ADDRCONFIG) {
    gai_results.reset(query(host, port, socktype, flags));
    return *this;
  }

  /**
   * Return ForwardIterator to struct addrinfo* results.
   */
  Iter iterate() const { return Iter{gai_results.get()}; }
};

} // namespace thrift
} // namespace apache

#endif
