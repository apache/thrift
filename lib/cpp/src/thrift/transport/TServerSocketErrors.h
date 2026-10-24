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

#ifndef _THRIFT_TRANSPORT_TSERVERSOCKETERRORS_H_
#define _THRIFT_TRANSPORT_TSERVERSOCKETERRORS_H_ 1

#include <algorithm>
#include <cerrno>

#include <thrift/transport/PlatformSocket.h>

namespace apache {
namespace thrift {
namespace transport {
namespace detail {

// accept() error classes for TServerSocket. Installed, but no compatibility promise.

// Failure of this connection only; the listening socket is fine. Linux: see accept(2) NOTES.
inline bool isConnectionError(int err) {
  switch (err) {
  case THRIFT_EINTR:
  case THRIFT_EAGAIN:
#if THRIFT_EWOULDBLOCK != THRIFT_EAGAIN
  case THRIFT_EWOULDBLOCK:
#endif
#ifdef _WIN32
  case WSAECONNRESET:
  case WSAECONNABORTED:
#else
  case ECONNABORTED:
#ifdef EPROTO
  case EPROTO:
#endif
#ifdef __linux__
  case EPERM:
  case ENETDOWN:
  case ENETUNREACH:
  case EHOSTUNREACH:
  case EHOSTDOWN:
  case ENONET:
  case ENOPROTOOPT:
  case EOPNOTSUPP:
#endif
#endif
    return true;
  default:
    return false;
  }
}

// Out of descriptors or memory; transient, so retry after a backoff.
inline bool isResourceExhaustion(int err) {
  switch (err) {
#ifdef _WIN32
  case WSAEMFILE:
  case WSAENOBUFS:
#else
  case EMFILE:
  case ENFILE:
  case ENOBUFS:
  case ENOMEM:
#endif
    return true;
  default:
    return false;
  }
}

// Next wait: 5 ms, doubling to 1 s. Pass 0 for the first.
inline int nextBackoffMs(int previousMs) {
  return (previousMs == 0) ? 5 : (std::min)(previousMs * 2, 1000);
}

} // namespace detail
} // namespace transport
} // namespace thrift
} // namespace apache

#endif // #ifndef _THRIFT_TRANSPORT_TSERVERSOCKETERRORS_H_
