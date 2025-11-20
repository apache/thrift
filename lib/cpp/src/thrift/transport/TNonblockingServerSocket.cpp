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

#include <cstring>
#include <memory>
#include <stdexcept>
#include <sys/types.h>
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif
#ifdef HAVE_POLL_H
#include <poll.h>
#endif
#ifdef HAVE_SYS_POLL_H
#include <sys/poll.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#include <netinet/tcp.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#include <thrift/transport/PlatformSocket.h>
#include <thrift/transport/TNonblockingServerSocket.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TSocketUtils.h>
#include <thrift/transport/SocketCommon.h>

#ifndef AF_LOCAL
#define AF_LOCAL AF_UNIX
#endif

#ifndef SOCKOPT_CAST_T
#ifndef _WIN32
#define SOCKOPT_CAST_T void
#else
#define SOCKOPT_CAST_T char
#endif // _WIN32
#endif

template <class T>
inline const SOCKOPT_CAST_T* const_cast_sockopt(const T* v) {
  return reinterpret_cast<const SOCKOPT_CAST_T*>(v);
}

template <class T>
inline SOCKOPT_CAST_T* cast_sockopt(T* v) {
  return reinterpret_cast<SOCKOPT_CAST_T*>(v);
}

using std::shared_ptr;
using std::string;

namespace apache {
namespace thrift {
namespace transport {

TNonblockingServerSocket::TNonblockingServerSocket(int port)
  : port_(port),
    listenPort_(port),
    serverSocket_(THRIFT_INVALID_SOCKET),
    acceptBacklog_(DEFAULT_BACKLOG),
    sendTimeout_(0),
    recvTimeout_(0),
    retryLimit_(0),
    retryDelay_(0),
    tcpSendBuffer_(0),
    tcpRecvBuffer_(0),
    keepAlive_(false),
    listening_(false) {
}

TNonblockingServerSocket::TNonblockingServerSocket(int port, int sendTimeout, int recvTimeout)
  : port_(port),
    listenPort_(port),
    serverSocket_(THRIFT_INVALID_SOCKET),
    acceptBacklog_(DEFAULT_BACKLOG),
    sendTimeout_(sendTimeout),
    recvTimeout_(recvTimeout),
    retryLimit_(0),
    retryDelay_(0),
    tcpSendBuffer_(0),
    tcpRecvBuffer_(0),
    keepAlive_(false),
    listening_(false) {
}

TNonblockingServerSocket::TNonblockingServerSocket(const string& address, int port)
  : port_(port),
    listenPort_(port),
    address_(address),
    serverSocket_(THRIFT_INVALID_SOCKET),
    acceptBacklog_(DEFAULT_BACKLOG),
    sendTimeout_(0),
    recvTimeout_(0),
    retryLimit_(0),
    retryDelay_(0),
    tcpSendBuffer_(0),
    tcpRecvBuffer_(0),
    keepAlive_(false),
    listening_(false) {
}

TNonblockingServerSocket::TNonblockingServerSocket(const string& path)
  : port_(0),
    listenPort_(0),
    path_(path),
    serverSocket_(THRIFT_INVALID_SOCKET),
    acceptBacklog_(DEFAULT_BACKLOG),
    sendTimeout_(0),
    recvTimeout_(0),
    retryLimit_(0),
    retryDelay_(0),
    tcpSendBuffer_(0),
    tcpRecvBuffer_(0),
    keepAlive_(false),
    listening_(false) {
}

TNonblockingServerSocket::~TNonblockingServerSocket() {
  close();
}

bool TNonblockingServerSocket::isOpen() const {
  if (serverSocket_ == THRIFT_INVALID_SOCKET)
    return false;

  if (!listening_)
    return false;

  if (isUnixDomainSocket() && (path_[0] != '\0')) {
    // On some platforms the domain socket file may not be instantly
    // available yet, i.e. the Windows file system can be slow. Therefore
    // we should check that the domain socket file actually exists.
#ifdef _MSC_VER
    // Currently there is a bug in ClangCl on Windows so the stat() call
    // does not work. Workaround is a Windows-specific call if file exists:
    DWORD const f_attrib = GetFileAttributesA(path_.c_str());
    if (f_attrib == INVALID_FILE_ATTRIBUTES) {
#else
    struct THRIFT_STAT path_info;
    if (::THRIFT_STAT(path_.c_str(), &path_info) < 0) {
#endif
      const std::string vError = "TNonblockingServerSocket::isOpen(): The domain socket path '" + path_ + "' does not exist (yet).";
      GlobalOutput.perror(vError.c_str(), THRIFT_GET_SOCKET_ERROR);
      return false;
    }
  }

  return true;
}

void TNonblockingServerSocket::setSendTimeout(int sendTimeout) {
  sendTimeout_ = sendTimeout;
}

void TNonblockingServerSocket::setRecvTimeout(int recvTimeout) {
  recvTimeout_ = recvTimeout;
}

void TNonblockingServerSocket::setAcceptBacklog(int accBacklog) {
  acceptBacklog_ = accBacklog;
}

void TNonblockingServerSocket::setRetryLimit(int retryLimit) {
  retryLimit_ = retryLimit;
}

void TNonblockingServerSocket::setRetryDelay(int retryDelay) {
  retryDelay_ = retryDelay;
}

void TNonblockingServerSocket::setTcpSendBuffer(int tcpSendBuffer) {
  tcpSendBuffer_ = tcpSendBuffer;
}

void TNonblockingServerSocket::setTcpRecvBuffer(int tcpRecvBuffer) {
  tcpRecvBuffer_ = tcpRecvBuffer;
}

void TNonblockingServerSocket::_setup_sockopts() {
  int one = 1;
  if (!isUnixDomainSocket()) {
    // Set THRIFT_NO_SOCKET_CACHING to prevent 2MSL delay on accept.
    // This does not work with Domain sockets on most platforms. And
    // on Windows it completely breaks the socket. Therefore do not
    // use this on Domain sockets.
    if (-1 == setsockopt(serverSocket_,
                         SOL_SOCKET,
                         THRIFT_NO_SOCKET_CACHING,
                         cast_sockopt(&one),
                         sizeof(one))) {
      // NOTE: SO_EXCLUSIVEADDRUSE socket option can only be used by members
      // of the Administrators security group on Windows XP and earlier. But
      // we do not target WinXP anymore so no special checks required.
      int errno_copy = THRIFT_GET_SOCKET_ERROR;
      GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() THRIFT_NO_SOCKET_CACHING ",
                          errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN,
                                "Could not set THRIFT_NO_SOCKET_CACHING",
                                errno_copy);
    }
  }

  // Set TCP buffer sizes
  if (tcpSendBuffer_ > 0) {
    if (-1 == setsockopt(serverSocket_,
                         SOL_SOCKET,
                         SO_SNDBUF,
                         cast_sockopt(&tcpSendBuffer_),
                         sizeof(tcpSendBuffer_))) {
      int errno_copy = THRIFT_GET_SOCKET_ERROR;
      GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() SO_SNDBUF ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN,
                                "Could not set SO_SNDBUF",
                                errno_copy);
    }
  }

  if (tcpRecvBuffer_ > 0) {
    if (-1 == setsockopt(serverSocket_,
                         SOL_SOCKET,
                         SO_RCVBUF,
                         cast_sockopt(&tcpRecvBuffer_),
                         sizeof(tcpRecvBuffer_))) {
      int errno_copy = THRIFT_GET_SOCKET_ERROR;
      GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() SO_RCVBUF ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN,
                                "Could not set SO_RCVBUF",
                                errno_copy);
    }
  }

  // Turn linger off, don't want to block on calls to close
  struct linger ling = {0, 0};
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_LINGER, cast_sockopt(&ling), sizeof(ling))) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() SO_LINGER ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not set SO_LINGER", errno_copy);
  }

  // Keepalive to ensure full result flushing
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_KEEPALIVE, const_cast_sockopt(&one), sizeof(one))) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() SO_KEEPALIVE ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
      "Could not set TCP_NODELAY",
      errno_copy);
  }

#ifdef SO_NOSIGPIPE
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one))) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() SO_NOSIGPIPE", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
                              "Could not set SO_NOSIGPIPE",
                              errno_copy);
  }
#endif

  // Set NONBLOCK on the accept socket
  int flags = THRIFT_FCNTL(serverSocket_, THRIFT_F_GETFL, 0);
  if (flags == -1) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() THRIFT_FCNTL() THRIFT_F_GETFL ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
                              "THRIFT_FCNTL() THRIFT_F_GETFL failed",
                              errno_copy);
  }

  if (-1 == THRIFT_FCNTL(serverSocket_, THRIFT_F_SETFL, flags | THRIFT_O_NONBLOCK)) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() THRIFT_FCNTL() THRIFT_O_NONBLOCK ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
                              "THRIFT_FCNTL() THRIFT_F_SETFL THRIFT_O_NONBLOCK failed",
                              errno_copy);
  }
}

void TNonblockingServerSocket::_setup_unixdomain_sockopts() {
}

void TNonblockingServerSocket::_setup_tcp_sockopts() {
  int one = 1;

  // Set TCP nodelay if available, MAC OS X Hack
  // See http://lists.danga.com/pipermail/memcached/2005-March/001240.html
#ifndef TCP_NOPUSH
  // TCP Nodelay, speed over bandwidth
  if (-1
      == setsockopt(serverSocket_, IPPROTO_TCP, TCP_NODELAY, cast_sockopt(&one), sizeof(one))) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() TCP_NODELAY ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
                              "Could not set TCP_NODELAY",
                              errno_copy);
  }
#endif

#ifdef TCP_LOW_MIN_RTO
  if (TSocket::getUseLowMinRto()) {
    if (-1 == setsockopt(s, IPPROTO_TCP, TCP_LOW_MIN_RTO, const_cast_sockopt(&one), sizeof(one))) {
      int errno_copy = THRIFT_GET_SOCKET_ERROR;
      GlobalOutput.perror("TNonblockingServerSocket::listen() setsockopt() TCP_LOW_MIN_RTO ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN,
        "Could not set TCP_NODELAY",
        errno_copy);
    }
  }
#endif

} // _setup_tcp_sockopts()

void TNonblockingServerSocket::listen() {
#ifdef _WIN32
  TWinsockSingleton::create();
#endif // _WIN32


  // Validate port number
  if (port_ < 0 || port_ > 0xFFFF) {
    throw TTransportException(TTransportException::BAD_ARGS, "Specified port is invalid");
  }

  // Resolve host:port strings into an iterable of struct addrinfo*
  AddressResolutionHelper resolved_addresses;
  if (!isUnixDomainSocket()) {
    try {
      resolved_addresses.resolve(address_, std::to_string(port_), SOCK_STREAM,
#ifdef ANDROID
                                 AI_PASSIVE | AI_ADDRCONFIG);
#else
                                 AI_PASSIVE | AI_V4MAPPED);
#endif
    } catch (const std::system_error& e) {
      GlobalOutput.printf("getaddrinfo() -> %d; %s", e.code().value(), e.what());
      close();
      throw TTransportException(TTransportException::NOT_OPEN,
                                "Could not resolve host for server socket.");
    }
  }

  // we may want to try to bind more than once, since THRIFT_NO_SOCKET_CACHING doesn't
  // always seem to work. The client can configure the retry variables.
  int retries = 0;
  int errno_copy = 0;

  if (isUnixDomainSocket()) {
    // -- Unix Domain Socket -- //

    serverSocket_ = socket(PF_UNIX, SOCK_STREAM, IPPROTO_IP);

    if (serverSocket_ == THRIFT_INVALID_SOCKET) {
      int errno_copy = THRIFT_GET_SOCKET_ERROR;
      GlobalOutput.perror("TNonblockingServerSocket::listen() socket() ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN,
                                "Could not create server socket.",
                                errno_copy);
    }

    _setup_sockopts();
    _setup_unixdomain_sockopts();

    // Windows supports Unix domain sockets since it ships the header
    // HAVE_AF_UNIX_H (see https://devblogs.microsoft.com/commandline/af_unix-comes-to-windows/)
#if (!defined(_WIN32) || defined(HAVE_AF_UNIX_H))
    struct sockaddr_un address;
    socklen_t structlen = fillUnixSocketAddr(address, path_);

    do {
      if (0 == ::bind(serverSocket_, (struct sockaddr*)&address, structlen)) {
        break;
      }
      errno_copy = THRIFT_GET_SOCKET_ERROR;
      // use short circuit evaluation here to only sleep if we need to
    } while ((retries++ < retryLimit_) && (THRIFT_SLEEP_SEC(retryDelay_) == 0));
#else
    GlobalOutput.perror("TNonblockingServerSocket::open() Unix Domain socket path not supported on this version of Windows", -99);
    throw TTransportException(TTransportException::NOT_OPEN,
                              " Unix Domain socket path not supported");
#endif
  } else {
    // -- TCP socket -- //

    auto addr_iter = AddressResolutionHelper::Iter{};

    // Via DNS or somehow else, single hostname can resolve into many addresses.
    // Results may contain perhaps a mix of IPv4 and IPv6.  Here, we iterate
    // over what system gave us, picking the first address that works.
    do {
      if (!addr_iter) {
        // init + recycle over many retries
        addr_iter = resolved_addresses.iterate();
      }
      auto trybind = *addr_iter++;

      serverSocket_ = socket(trybind->ai_family, trybind->ai_socktype, trybind->ai_protocol);
      if (serverSocket_ == -1) {
        errno_copy = THRIFT_GET_SOCKET_ERROR;
        continue;
      }

      _setup_sockopts();
      _setup_tcp_sockopts();

#ifdef IPV6_V6ONLY
      if (trybind->ai_family == AF_INET6) {
        int zero = 0;
        if (-1 == setsockopt(serverSocket_,
                             IPPROTO_IPV6,
                             IPV6_V6ONLY,
                             cast_sockopt(&zero),
                             sizeof(zero))) {
          GlobalOutput.perror("TNonblockingServerSocket::listen() IPV6_V6ONLY ", THRIFT_GET_SOCKET_ERROR);
        }
      }
#endif // #ifdef IPV6_V6ONLY

      if (0 == ::bind(serverSocket_, trybind->ai_addr, static_cast<int>(trybind->ai_addrlen))) {
        break;
      }
      errno_copy = THRIFT_GET_SOCKET_ERROR;

      // use short circuit evaluation here to only sleep if we need to
    } while ((retries++ < retryLimit_) && (THRIFT_SLEEP_SEC(retryDelay_) == 0));

    // retrieve bind info
    if (port_ == 0 && retries <= retryLimit_) {
      struct sockaddr_storage sa;
      socklen_t len = sizeof(sa);
      std::memset(&sa, 0, len);
      if (::getsockname(serverSocket_, reinterpret_cast<struct sockaddr*>(&sa), &len) < 0) {
        errno_copy = THRIFT_GET_SOCKET_ERROR;
        GlobalOutput.perror("TNonblockingServerSocket::getPort() getsockname() ", errno_copy);
      } else {
        if (sa.ss_family == AF_INET6) {
          const auto* sin = reinterpret_cast<const struct sockaddr_in6*>(&sa);
          listenPort_ = ntohs(sin->sin6_port);
        } else {
          const auto* sin = reinterpret_cast<const struct sockaddr_in*>(&sa);
          listenPort_ = ntohs(sin->sin_port);
        }
      }
    }
  } // TCP socket //

  // throw error if socket still wasn't created successfully
  if (serverSocket_ == THRIFT_INVALID_SOCKET) {
    GlobalOutput.perror("TNonblockingServerSocket::listen() socket() ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
                              "Could not create server socket.",
                              errno_copy);
  }

  // throw an error if we failed to bind properly
  if (retries > retryLimit_) {
    char errbuf[1024];
    if (isUnixDomainSocket()) {
#ifdef _WIN32
      THRIFT_SNPRINTF(errbuf, sizeof(errbuf), "TNonblockingServerSocket::listen() Could not bind to domain socket path %s, error %d", path_.c_str(), WSAGetLastError());
#else
      // Fixme: This does not currently handle abstract domain sockets:
      THRIFT_SNPRINTF(errbuf, sizeof(errbuf), "TNonblockingServerSocket::listen() Could not bind to domain socket path %s", path_.c_str());
#endif
    } else {
      THRIFT_SNPRINTF(errbuf, sizeof(errbuf), "TNonblockingServerSocket::listen() Could not bind to port %d", port_);
    }
    GlobalOutput(errbuf);
    close();
    throw TTransportException(TTransportException::NOT_OPEN,
                              "Could not bind",
                              errno_copy);
  }

  if (listenCallback_)
    listenCallback_(serverSocket_);

  // Call listen
  if (-1 == ::listen(serverSocket_, acceptBacklog_)) {
    errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::listen() listen() ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not listen", errno_copy);
  }

  // The socket is now listening!
  listening_ = true;
}

int TNonblockingServerSocket::getPort() {
  return port_;
}

int TNonblockingServerSocket::getListenPort() {
  return listenPort_;
}

std::string TNonblockingServerSocket::getPath() const {
    return path_;
}

bool TNonblockingServerSocket::isUnixDomainSocket() const {
    return !path_.empty();
}

shared_ptr<TSocket> TNonblockingServerSocket::acceptImpl() {
  if (serverSocket_ == THRIFT_INVALID_SOCKET) {
    throw TTransportException(TTransportException::NOT_OPEN,
                              "TNonblockingServerSocket not listening");
  }

  struct sockaddr_storage clientAddress;
  int size = sizeof(clientAddress);
  THRIFT_SOCKET clientSocket
      = ::accept(serverSocket_, (struct sockaddr*)&clientAddress, (socklen_t*)&size);

  if (clientSocket == THRIFT_INVALID_SOCKET) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    GlobalOutput.perror("TNonblockingServerSocket::acceptImpl() ::accept() ", errno_copy);
    throw TTransportException(TTransportException::UNKNOWN, "accept()", errno_copy);
  }

  // Explicitly set this socket to NONBLOCK mode
  int flags = THRIFT_FCNTL(clientSocket, THRIFT_F_GETFL, 0);
  if (flags == -1) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    ::THRIFT_CLOSESOCKET(clientSocket);
    GlobalOutput.perror("TNonblockingServerSocket::acceptImpl() THRIFT_FCNTL() THRIFT_F_GETFL ", errno_copy);
    throw TTransportException(TTransportException::UNKNOWN,
                              "THRIFT_FCNTL(THRIFT_F_GETFL)",
                              errno_copy);
  }

  if (-1 == THRIFT_FCNTL(clientSocket, THRIFT_F_SETFL, flags | THRIFT_O_NONBLOCK)) {
    int errno_copy = THRIFT_GET_SOCKET_ERROR;
    ::THRIFT_CLOSESOCKET(clientSocket);
    GlobalOutput
        .perror("TNonblockingServerSocket::acceptImpl() THRIFT_FCNTL() THRIFT_F_SETFL ~THRIFT_O_NONBLOCK ",
                errno_copy);
    throw TTransportException(TTransportException::UNKNOWN,
                              "THRIFT_FCNTL(THRIFT_F_SETFL)",
                              errno_copy);
  }

  shared_ptr<TSocket> client = createSocket(clientSocket);
  client->setPath(path_);
  if (sendTimeout_ > 0) {
    client->setSendTimeout(sendTimeout_);
  }
  if (recvTimeout_ > 0) {
    client->setRecvTimeout(recvTimeout_);
  }
  if (keepAlive_) {
    client->setKeepAlive(keepAlive_);
  }
  client->setCachedAddress((sockaddr*)&clientAddress, size);

  if (acceptCallback_)
    acceptCallback_(clientSocket);

  return client;
}

shared_ptr<TSocket> TNonblockingServerSocket::createSocket(THRIFT_SOCKET clientSocket) {
  return std::make_shared<TSocket>(clientSocket);
}

void TNonblockingServerSocket::close() {
  if (serverSocket_ != THRIFT_INVALID_SOCKET) {
    shutdown(serverSocket_, THRIFT_SHUT_RDWR);
    ::THRIFT_CLOSESOCKET(serverSocket_);
  }
  serverSocket_ = THRIFT_INVALID_SOCKET;
  listening_ = false;
}
} // namespace transport
} // namespace thrift
} // namespace apache
