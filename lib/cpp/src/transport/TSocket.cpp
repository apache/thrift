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

#include <config.h>
#include <cstring>
#include <sstream>
#include <sys/socket.h>
#include <sys/poll.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>

#include "concurrency/Monitor.h"
#include "TSocket.h"
#include "TTransportException.h"

namespace apache { namespace thrift { namespace transport {

using namespace std;

// Global var to track total socket sys calls
uint32_t g_socket_syscalls = 0;

/**
 * TSocket implementation.
 *
 */

TSocket::TSocket(string host, int port) :
  host_(host),
  port_(port),
  socket_(-1),
  connTimeout_(0),
  sendTimeout_(0),
  recvTimeout_(0),
  lingerOn_(1),
  lingerVal_(0),
  noDelay_(1),
  maxRecvRetries_(5) {
  recvTimeval_.tv_sec = (int)(recvTimeout_/1000);
  recvTimeval_.tv_usec = (int)((recvTimeout_%1000)*1000);
}

TSocket::TSocket() :
  host_(""),
  port_(0),
  socket_(-1),
  connTimeout_(0),
  sendTimeout_(0),
  recvTimeout_(0),
  lingerOn_(1),
  lingerVal_(0),
  noDelay_(1),
  maxRecvRetries_(5) {
  recvTimeval_.tv_sec = (int)(recvTimeout_/1000);
  recvTimeval_.tv_usec = (int)((recvTimeout_%1000)*1000);
}

TSocket::TSocket(int socket) :
  host_(""),
  port_(0),
  socket_(socket),
  connTimeout_(0),
  sendTimeout_(0),
  recvTimeout_(0),
  lingerOn_(1),
  lingerVal_(0),
  noDelay_(1),
  maxRecvRetries_(5) {
  recvTimeval_.tv_sec = (int)(recvTimeout_/1000);
  recvTimeval_.tv_usec = (int)((recvTimeout_%1000)*1000);
}

TSocket::~TSocket() {
  close();
}

bool TSocket::isOpen() {
  return (socket_ >= 0);
}

bool TSocket::peek() {
  if (!isOpen()) {
    return false;
  }
  uint8_t buf;
  int r = recv(socket_, &buf, 1, MSG_PEEK);
  if (r == -1) {
    int errno_copy = errno;
    #if defined __FreeBSD__ || defined __MACH__
    /* shigin:
     * freebsd returns -1 and ECONNRESET if socket was closed by 
     * the other side
     */
    if (errno_copy == ECONNRESET)
    {
      close();
      return false;
    }
    #endif
    GlobalOutput.perror("TSocket::peek() recv() " + getSocketInfo(), errno_copy);
    throw TTransportException(TTransportException::UNKNOWN, "recv()", errno_copy);
  }
  return (r > 0);
}

void TSocket::openConnection(struct addrinfo *res) {
  if (isOpen()) {
    throw TTransportException(TTransportException::ALREADY_OPEN);
  }

  socket_ = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
  if (socket_ == -1) {
    int errno_copy = errno;
    GlobalOutput.perror("TSocket::open() socket() " + getSocketInfo(), errno_copy);
    throw TTransportException(TTransportException::NOT_OPEN, "socket()", errno_copy);
  }

  // Send timeout
  if (sendTimeout_ > 0) {
    setSendTimeout(sendTimeout_);
  }

  // Recv timeout
  if (recvTimeout_ > 0) {
    setRecvTimeout(recvTimeout_);
  }

  // Linger
  setLinger(lingerOn_, lingerVal_);

  // No delay
  setNoDelay(noDelay_);

  // Set the socket to be non blocking for connect if a timeout exists
  int flags = fcntl(socket_, F_GETFL, 0);
  if (connTimeout_ > 0) {
    if (-1 == fcntl(socket_, F_SETFL, flags | O_NONBLOCK)) {
      int errno_copy = errno;
      GlobalOutput.perror("TSocket::open() fcntl() " + getSocketInfo(), errno_copy);
      throw TTransportException(TTransportException::NOT_OPEN, "fcntl() failed", errno_copy);
    }
  } else {
    if (-1 == fcntl(socket_, F_SETFL, flags & ~O_NONBLOCK)) {
      int errno_copy = errno;
      GlobalOutput.perror("TSocket::open() fcntl " + getSocketInfo(), errno_copy);
      throw TTransportException(TTransportException::NOT_OPEN, "fcntl() failed", errno_copy);
    }
  }

  // Connect the socket
  int ret = connect(socket_, res->ai_addr, res->ai_addrlen);

  // success case
  if (ret == 0) {
    goto done;
  }

  if (errno != EINPROGRESS) {
    int errno_copy = errno;
    GlobalOutput.perror("TSocket::open() connect() " + getSocketInfo(), errno_copy);
    throw TTransportException(TTransportException::NOT_OPEN, "connect() failed", errno_copy);
  }


  struct pollfd fds[1];
  std::memset(fds, 0 , sizeof(fds));
  fds[0].fd = socket_;
  fds[0].events = POLLOUT;
  ret = poll(fds, 1, connTimeout_);

  if (ret > 0) {
    // Ensure the socket is connected and that there are no errors set
    int val;
    socklen_t lon;
    lon = sizeof(int);
    int ret2 = getsockopt(socket_, SOL_SOCKET, SO_ERROR, (void *)&val, &lon);
    if (ret2 == -1) {
      int errno_copy = errno;
      GlobalOutput.perror("TSocket::open() getsockopt() " + getSocketInfo(), errno_copy);
      throw TTransportException(TTransportException::NOT_OPEN, "getsockopt()", errno_copy);
    }
    // no errors on socket, go to town
    if (val == 0) {
      goto done;
    }
    GlobalOutput.perror("TSocket::open() error on socket (after poll) " + getSocketInfo(), val);
    throw TTransportException(TTransportException::NOT_OPEN, "socket open() error", val);
  } else if (ret == 0) {
    // socket timed out
    string errStr = "TSocket::open() timed out " + getSocketInfo();
    GlobalOutput(errStr.c_str());
    throw TTransportException(TTransportException::NOT_OPEN, "open() timed out");
  } else {
    // error on poll()
    int errno_copy = errno;
    GlobalOutput.perror("TSocket::open() poll() " + getSocketInfo(), errno_copy);
    throw TTransportException(TTransportException::NOT_OPEN, "poll() failed", errno_copy);
  }

 done:
  // Set socket back to normal mode (blocking)
  fcntl(socket_, F_SETFL, flags);
}

void TSocket::open() {
  if (isOpen()) {
    throw TTransportException(TTransportException::ALREADY_OPEN);
  }

  // Validate port number
  if (port_ < 0 || port_ > 65536) {
    throw TTransportException(TTransportException::NOT_OPEN, "Specified port is invalid");
  }

  struct addrinfo hints, *res, *res0;
  res = NULL;
  res0 = NULL;
  int error;
  char port[sizeof("65536")];
  std::memset(&hints, 0, sizeof(hints));
  hints.ai_family = PF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_flags = AI_PASSIVE | AI_ADDRCONFIG;
  sprintf(port, "%d", port_);

  error = getaddrinfo(host_.c_str(), port, &hints, &res0);

  if (error) {
    string errStr = "TSocket::open() getaddrinfo() " + getSocketInfo() + string(gai_strerror(error));
    GlobalOutput(errStr.c_str());
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not resolve host for client socket.");
  }

  // Cycle through all the returned addresses until one
  // connects or push the exception up.
  for (res = res0; res; res = res->ai_next) {
    try {
      openConnection(res);
      break;
    } catch (TTransportException& ttx) {
      if (res->ai_next) {
        close();
      } else {
        close();
        freeaddrinfo(res0); // cleanup on failure
        throw;
      }
    }
  }

  // Free address structure memory
  freeaddrinfo(res0);
}

void TSocket::close() {
  if (socket_ >= 0) {
    shutdown(socket_, SHUT_RDWR);
    ::close(socket_);
  }
  socket_ = -1;
}

uint32_t TSocket::read(uint8_t* buf, uint32_t len) {
  if (socket_ < 0) {
    throw TTransportException(TTransportException::NOT_OPEN, "Called read on non-open socket");
  }

  int32_t retries = 0;

  // EAGAIN can be signalled both when a timeout has occurred and when
  // the system is out of resources (an awesome undocumented feature).
  // The following is an approximation of the time interval under which
  // EAGAIN is taken to indicate an out of resources error.
  uint32_t eagainThresholdMicros = 0;
  if (recvTimeout_) {
    // if a readTimeout is specified along with a max number of recv retries, then
    // the threshold will ensure that the read timeout is not exceeded even in the
    // case of resource errors
    eagainThresholdMicros = (recvTimeout_*1000)/ ((maxRecvRetries_>0) ? maxRecvRetries_ : 2);
  }

 try_again:
  // Read from the socket
  struct timeval begin;
  gettimeofday(&begin, NULL);
  int got = recv(socket_, buf, len, 0);
  int errno_copy = errno; //gettimeofday can change errno
  struct timeval end;
  gettimeofday(&end, NULL);
  uint32_t readElapsedMicros =  (((end.tv_sec - begin.tv_sec) * 1000 * 1000)
                                 + (((uint64_t)(end.tv_usec - begin.tv_usec))));
  ++g_socket_syscalls;

  // Check for error on read
  if (got < 0) {
    if (errno_copy == EAGAIN) {
      // check if this is the lack of resources or timeout case
      if (!eagainThresholdMicros || (readElapsedMicros < eagainThresholdMicros)) {
        if (retries++ < maxRecvRetries_) {
          usleep(50);
          goto try_again;
        } else {
          throw TTransportException(TTransportException::TIMED_OUT,
                                    "EAGAIN (unavailable resources)");
        }
      } else {
        // infer that timeout has been hit
        throw TTransportException(TTransportException::TIMED_OUT,
                                  "EAGAIN (timed out)");
      }
    }

    // If interrupted, try again
    if (errno_copy == EINTR && retries++ < maxRecvRetries_) {
      goto try_again;
    }

    #if defined __FreeBSD__ || defined __MACH__
    if (errno_copy == ECONNRESET) {
      /* shigin: freebsd doesn't follow POSIX semantic of recv and fails with
       * ECONNRESET if peer performed shutdown 
       */
      close();
      return 0;
    }
    #endif

    // Now it's not a try again case, but a real probblez
    GlobalOutput.perror("TSocket::read() recv() " + getSocketInfo(), errno_copy);

    // If we disconnect with no linger time
    if (errno_copy == ECONNRESET) {
      throw TTransportException(TTransportException::NOT_OPEN, "ECONNRESET");
    }

    // This ish isn't open
    if (errno_copy == ENOTCONN) {
      throw TTransportException(TTransportException::NOT_OPEN, "ENOTCONN");
    }

    // Timed out!
    if (errno_copy == ETIMEDOUT) {
      throw TTransportException(TTransportException::TIMED_OUT, "ETIMEDOUT");
    }

    // Some other error, whatevz
    throw TTransportException(TTransportException::UNKNOWN, "Unknown", errno_copy);
  }

  // The remote host has closed the socket
  if (got == 0) {
    close();
    return 0;
  }

  // Pack data into string
  return got;
}

void TSocket::write(const uint8_t* buf, uint32_t len) {
  if (socket_ < 0) {
    throw TTransportException(TTransportException::NOT_OPEN, "Called write on non-open socket");
  }

  uint32_t sent = 0;

  while (sent < len) {

    int flags = 0;
    #ifdef MSG_NOSIGNAL
    // Note the use of MSG_NOSIGNAL to suppress SIGPIPE errors, instead we
    // check for the EPIPE return condition and close the socket in that case
    flags |= MSG_NOSIGNAL;
    #endif // ifdef MSG_NOSIGNAL

    int b = send(socket_, buf + sent, len - sent, flags);
    ++g_socket_syscalls;

    // Fail on a send error
    if (b < 0) {
      int errno_copy = errno;
      GlobalOutput.perror("TSocket::write() send() " + getSocketInfo(), errno_copy);

      if (errno == EPIPE || errno == ECONNRESET || errno == ENOTCONN) {
        close();
        throw TTransportException(TTransportException::NOT_OPEN, "write() send()", errno_copy);
      }

      throw TTransportException(TTransportException::UNKNOWN, "write() send()", errno_copy);
    }

    // Fail on blocked send
    if (b == 0) {
      throw TTransportException(TTransportException::NOT_OPEN, "Socket send returned 0.");
    }
    sent += b;
  }
}

std::string TSocket::getHost() {
  return host_;
}

int TSocket::getPort() {
  return port_;
}

void TSocket::setHost(string host) {
  host_ = host;
}

void TSocket::setPort(int port) {
  port_ = port;
}

void TSocket::setLinger(bool on, int linger) {
  lingerOn_ = on;
  lingerVal_ = linger;
  if (socket_ < 0) {
    return;
  }

  struct linger l = {(lingerOn_ ? 1 : 0), lingerVal_};
  int ret = setsockopt(socket_, SOL_SOCKET, SO_LINGER, &l, sizeof(l));
  if (ret == -1) {
    int errno_copy = errno;  // Copy errno because we're allocating memory.
    GlobalOutput.perror("TSocket::setLinger() setsockopt() " + getSocketInfo(), errno_copy);
  }
}

void TSocket::setNoDelay(bool noDelay) {
  noDelay_ = noDelay;
  if (socket_ < 0) {
    return;
  }

  // Set socket to NODELAY
  int v = noDelay_ ? 1 : 0;
  int ret = setsockopt(socket_, IPPROTO_TCP, TCP_NODELAY, &v, sizeof(v));
  if (ret == -1) {
    int errno_copy = errno;  // Copy errno because we're allocating memory.
    GlobalOutput.perror("TSocket::setNoDelay() setsockopt() " + getSocketInfo(), errno_copy);
  }
}

void TSocket::setConnTimeout(int ms) {
  connTimeout_ = ms;
}

void TSocket::setRecvTimeout(int ms) {
  if (ms < 0) {
    char errBuf[512];
    sprintf(errBuf, "TSocket::setRecvTimeout with negative input: %d\n", ms);
    GlobalOutput(errBuf);
    return;
  }
  recvTimeout_ = ms;

  if (socket_ < 0) {
    return;
  }

  recvTimeval_.tv_sec = (int)(recvTimeout_/1000);
  recvTimeval_.tv_usec = (int)((recvTimeout_%1000)*1000);

  // Copy because poll may modify
  struct timeval r = recvTimeval_;
  int ret = setsockopt(socket_, SOL_SOCKET, SO_RCVTIMEO, &r, sizeof(r));
  if (ret == -1) {
    int errno_copy = errno;  // Copy errno because we're allocating memory.
    GlobalOutput.perror("TSocket::setRecvTimeout() setsockopt() " + getSocketInfo(), errno_copy);
  }
}

void TSocket::setSendTimeout(int ms) {
  if (ms < 0) {
    char errBuf[512];
    sprintf(errBuf, "TSocket::setSendTimeout with negative input: %d\n", ms);
    GlobalOutput(errBuf);
    return;
  }
  sendTimeout_ = ms;

  if (socket_ < 0) {
    return;
  }

  struct timeval s = {(int)(sendTimeout_/1000),
                      (int)((sendTimeout_%1000)*1000)};
  int ret = setsockopt(socket_, SOL_SOCKET, SO_SNDTIMEO, &s, sizeof(s));
  if (ret == -1) {
    int errno_copy = errno;  // Copy errno because we're allocating memory.
    GlobalOutput.perror("TSocket::setSendTimeout() setsockopt() " + getSocketInfo(), errno_copy);
  }
}

void TSocket::setMaxRecvRetries(int maxRecvRetries) {
  maxRecvRetries_ = maxRecvRetries;
}

string TSocket::getSocketInfo() {
  std::ostringstream oss;
  oss << "<Host: " << host_ << " Port: " << port_ << ">";
  return oss.str();
}

std::string TSocket::getPeerHost() {
  if (peerHost_.empty()) {
    struct sockaddr_storage addr;
    socklen_t addrLen = sizeof(addr);

    if (socket_ < 0) {
      return host_;
    }

    int rv = getpeername(socket_, (sockaddr*) &addr, &addrLen);

    if (rv != 0) {
      return peerHost_;
    }

    char clienthost[NI_MAXHOST];
    char clientservice[NI_MAXSERV];

    getnameinfo((sockaddr*) &addr, addrLen,
                clienthost, sizeof(clienthost),
                clientservice, sizeof(clientservice), 0);

    peerHost_ = clienthost;
  }
  return peerHost_;
}

std::string TSocket::getPeerAddress() {
  if (peerAddress_.empty()) {
    struct sockaddr_storage addr;
    socklen_t addrLen = sizeof(addr);

    if (socket_ < 0) {
      return peerAddress_;
    }

    int rv = getpeername(socket_, (sockaddr*) &addr, &addrLen);

    if (rv != 0) {
      return peerAddress_;
    }

    char clienthost[NI_MAXHOST];
    char clientservice[NI_MAXSERV];

    getnameinfo((sockaddr*) &addr, addrLen,
                clienthost, sizeof(clienthost),
                clientservice, sizeof(clientservice),
                NI_NUMERICHOST|NI_NUMERICSERV);

    peerAddress_ = clienthost;
    peerPort_ = std::atoi(clientservice);
  }
  return peerAddress_;
}

int TSocket::getPeerPort() {
  getPeerAddress();
  return peerPort_;
}

}}} // apache::thrift::transport
