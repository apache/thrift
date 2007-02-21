#include <config.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/select.h>

#include "concurrency/Monitor.h"
#include "TSocket.h"
#include "TTransportException.h"

namespace facebook { namespace thrift { namespace transport { 

using namespace std;
using namespace facebook::thrift::concurrency;

// Global var to track total socket sys calls
uint32_t g_socket_syscalls = 0;

/**
 * TSocket implementation.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */

// Mutex to protect syscalls to netdb
static Monitor s_netdb_monitor;

// TODO(mcslee): Make this an option to the socket class
#define MAX_RECV_RETRIES 20
  
TSocket::TSocket(string host, int port) : 
  host_(host),
  port_(port),
  socket_(-1),
  connTimeout_(0),
  sendTimeout_(0),
  recvTimeout_(0),
  lingerOn_(1),
  lingerVal_(0),
  noDelay_(1) {
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
  noDelay_(1) {
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
  noDelay_(1) {
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
    perror("TSocket::peek()");
    close();
    throw TTransportException(TTransportException::UNKNOWN, "recv() ERROR:" + errno);
  }
  return (r > 0);
}

void TSocket::open() {
  if (isOpen()) {
    throw TTransportException(TTransportException::ALREADY_OPEN);
  }

  // Create socket
  socket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_ == -1) {
    perror("TSocket::open() socket");
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "socket() ERROR:" + errno);
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

  // Lookup the hostname
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port_);

  {
    // Scope lock on host entry lookup
    Synchronized s(s_netdb_monitor);
    struct hostent *host_entry = gethostbyname(host_.c_str());
    
    if (host_entry == NULL) {
      perror("TSocket: dns error: failed call to gethostbyname.");
      close();
      throw TTransportException(TTransportException::NOT_OPEN, "gethostbyname() failed");
    }
    
    addr.sin_port = htons(port_);
    memcpy(&addr.sin_addr.s_addr,
           host_entry->h_addr_list[0],
           host_entry->h_length);
  }

  // Set the socket to be non blocking for connect if a timeout exists
  int flags = fcntl(socket_, F_GETFL, 0); 
  if (connTimeout_ > 0) {
    fcntl(socket_, F_SETFL, flags | O_NONBLOCK);
  } else {
    fcntl(socket_, F_SETFL, flags | ~O_NONBLOCK);
  }

  // Conn timeout
  struct timeval c = {(int)(connTimeout_/1000),
                      (int)((connTimeout_%1000)*1000)};
   
  // Connect the socket
  int ret = connect(socket_, (struct sockaddr *)&addr, sizeof(addr));
  
  if (ret == 0) {
    goto done;
  }

  if (errno != EINPROGRESS) {
    close();
    char buff[1024];
    sprintf(buff, "TSocket::open() connect %s %d", host_.c_str(), port_);
    perror(buff);
    throw TTransportException(TTransportException::NOT_OPEN, "open() ERROR: " + errno);
  }

  fd_set fds;
  FD_ZERO(&fds);
  FD_SET(socket_, &fds);
  ret = select(socket_+1, NULL, &fds, NULL, &c);

  if (ret > 0) {
    // Ensure connected
    int val;
    socklen_t lon;
    lon = sizeof(int);
    int ret2 = getsockopt(socket_, SOL_SOCKET, SO_ERROR, (void *)&val, &lon);
    if (ret2 == -1) {
      close();
      perror("TSocket::open() getsockopt SO_ERROR");
      throw TTransportException(TTransportException::NOT_OPEN, "open() ERROR: " + errno);
    }
    if (val == 0) {
      goto done;
    }
    close();
    perror("TSocket::open() SO_ERROR was set");
    throw TTransportException(TTransportException::NOT_OPEN, "open() ERROR: " + errno);
  } else if (ret == 0) {
    close();
    perror("TSocket::open() timeed out");
    throw TTransportException(TTransportException::NOT_OPEN, "open() ERROR: " + errno);   
  } else {
    close();
    perror("TSocket::open() select error");
    throw TTransportException(TTransportException::NOT_OPEN, "open() ERROR: " + errno);
  }

 done:
  // Set socket back to normal mode (blocking)
  fcntl(socket_, F_SETFL, flags);
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

  uint32_t retries = 0;
  
 try_again:
  // Read from the socket
  int got = recv(socket_, buf, len, 0);
  ++g_socket_syscalls;
  
  // Check for error on read
  if (got < 0) {   
    // If temporarily out of resources, sleep a bit and try again
    if (errno == EAGAIN && retries++ < MAX_RECV_RETRIES) {
      usleep(50);
      goto try_again;
    }
    
    // If interrupted, try again
    if (errno == EINTR && retries++ < MAX_RECV_RETRIES) {
      goto try_again;
    }
    
    // Now it's not a try again case, but a real probblez
    perror("TSocket::read()");

    // If we disconnect with no linger time
    if (errno == ECONNRESET) {
      throw TTransportException(TTransportException::NOT_OPEN, "ECONNRESET");
    }
    
    // This ish isn't open
    if (errno == ENOTCONN) {
      throw TTransportException(TTransportException::NOT_OPEN, "ENOTCONN");
    }
    
    // Timed out!
    if (errno == ETIMEDOUT) {
      throw TTransportException(TTransportException::TIMED_OUT, "ETIMEDOUT");
    }
    
    // Some other error, whatevz
    throw TTransportException(TTransportException::UNKNOWN, "ERROR:" + errno);
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
      if (errno == EPIPE) {
        close();
        throw TTransportException(TTransportException::NOT_OPEN, "EPIPE");
      }

      if (errno == ECONNRESET) {
        close();
        throw TTransportException(TTransportException::NOT_OPEN, "ECONNRESET");
      }

      if (errno == ENOTCONN) {
        close();
        throw TTransportException(TTransportException::NOT_OPEN, "ENOTCONN");
      }

      perror("TSocket::write() send < 0");
      throw TTransportException(TTransportException::UNKNOWN, "ERROR:" + errno);
    }
    
    // Fail on blocked send
    if (b == 0) {
      throw TTransportException(TTransportException::NOT_OPEN, "Socket send returned 0.");
    }
    sent += b;
  }
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
    perror("TSocket::setLinger()");
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
    perror("TSocket::setNoDelay()");
  }
}

void TSocket::setConnTimeout(int ms) {
  connTimeout_ = ms;
}

void TSocket::setRecvTimeout(int ms) {
  recvTimeout_ = ms;
  recvTimeval_.tv_sec = (int)(recvTimeout_/1000);
  recvTimeval_.tv_usec = (int)((recvTimeout_%1000)*1000);
  if (socket_ < 0) {
    return;
  }

  // Copy because select may modify
  struct timeval r = recvTimeval_;
  int ret = setsockopt(socket_, SOL_SOCKET, SO_RCVTIMEO, &r, sizeof(r));
  if (ret == -1) {
    perror("TSocket::setRecvTimeout()");
  }
}

void TSocket::setSendTimeout(int ms) {
  sendTimeout_ = ms;
  if (socket_ < 0) {
    return;
  }
   
  struct timeval s = {(int)(sendTimeout_/1000),
                      (int)((sendTimeout_%1000)*1000)};
  int ret = setsockopt(socket_, SOL_SOCKET, SO_SNDTIMEO, &s, sizeof(s));
  if (ret == -1) {
    perror("TSocket::setSendTimeout()");
  }
}

}}} // facebook::thrift::transport
