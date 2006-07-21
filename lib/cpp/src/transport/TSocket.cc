#include <config.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>

#include "transport/TSocket.h"
#include "transport/TTransportException.h"

using namespace std;

uint32_t g_socket_syscalls = 0;

/**
 * TSocket implementation.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */

// Mutex to protect syscalls to netdb
pthread_mutex_t g_netdb_mutex = PTHREAD_MUTEX_INITIALIZER;

// TODO(mcslee): Make this an option to the socket class
#define MAX_RECV_RETRIES 20

TSocket::TSocket(string host, int port) :
  host_(host), port_(port), socket_(0) {}

TSocket::TSocket(int socket) {
  socket_ = socket;
}

TSocket::~TSocket() {
  close();
}

bool TSocket::isOpen() {
  return (socket_ > 0); 
}

void TSocket::open() {
  // Create socket
  socket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_ == -1) {
    perror("TSocket::open() socket");
    close();
    throw TTransportException(TTX_NOT_OPEN, "socket() ERROR:" + errno);
  }
  
  // Lookup the hostname
  struct sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port_);

  /*
  if (inet_pton(AF_INET, host_.c_str(), &addr.sin_addr) < 0) {
    perror("TSocket::open() inet_pton");
  }
  */

  {
    // TODO(mcslee): Fix scope-locking here to protect hostname lookups
    // scopelock sl(&netdb_mutex);
    struct hostent *host_entry = gethostbyname(host_.c_str());
    
    if (host_entry == NULL) {
      // perror("dns error: failed call to gethostbyname.\n");
      close();
      throw TTransportException(TTX_NOT_OPEN, "gethostbyname() failed");
    }
    
    addr.sin_port = htons(port_);
    memcpy(&addr.sin_addr.s_addr,
           host_entry->h_addr_list[0],
           host_entry->h_length);
  }
   
  // Connect the socket
  int ret = connect(socket_, (struct sockaddr *)&addr, sizeof(addr));
  
  // Connect failed
  if (ret < 0) {
    perror("TSocket::open() connect");
    close();
    throw TTransportException(TTX_NOT_OPEN, "open() ERROR: " + errno);
  }

  // Connection was successful
}

void TSocket::close() {
  if (socket_ > 0) {
    shutdown(socket_, SHUT_RDWR);
    ::close(socket_);
  }
  socket_ = 0;
}

uint32_t TSocket::read(uint8_t* buf, uint32_t len) {
  if (socket_ <= 0) {
    throw TTransportException(TTX_NOT_OPEN, "Called read on non-open socket");
  }

  uint32_t retries = 0;
  
 try_again:
  // Read from the socket
  int got = recv(socket_, buf, len, 0);
  ++g_socket_syscalls;
  
  // Check for error on read
  if (got < 0) {
    perror("TSocket::read()");
    
    // If temporarily out of resources, sleep a bit and try again
    if (errno == EAGAIN && retries++ < MAX_RECV_RETRIES) {
      usleep(50);
      goto try_again;
    }
    
    // If interrupted, try again
    if (errno == EINTR && retries++ < MAX_RECV_RETRIES) {
      goto try_again;
    }
    
    // If we disconnect with no linger time
    if (errno == ECONNRESET) {
      throw TTransportException(TTX_NOT_OPEN, "ECONNRESET");
    }
    
    // This ish isn't open
    if (errno == ENOTCONN) {
      throw TTransportException(TTX_NOT_OPEN, "ENOTCONN");
    }
    
    // Timed out!
    if (errno == ETIMEDOUT) {
      throw TTransportException(TTX_TIMED_OUT, "ETIMEDOUT");
    }
    
    // Some other error, whatevz
    throw TTransportException(TTX_UNKNOWN, "ERROR:" + errno);
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
  if (socket_ <= 0) {
    throw TTransportException(TTX_NOT_OPEN, "Called write on non-open socket");
  }

  uint32_t sent = 0;
    
  while (sent < len) {

    int flags = 0;

    #if defined(MSG_NOSIGNAL)
    // Note the use of MSG_NOSIGNAL to suppress SIGPIPE errors, instead we
    // check for the EPIPE return condition and close the socket in that case
    flags |= MSG_NOSIGNAL;
    #endif // defined(MSG_NOSIGNAL)

    int b = send(socket_, buf + sent, len - sent, flags);
    ++g_socket_syscalls;

    // Fail on a send error
    if (b < 0) {
      if (errno == EPIPE) {
        close();
        throw TTransportException(TTX_NOT_OPEN, "EPIPE");
      }

      if (errno == ECONNRESET) {
        close();
        throw TTransportException(TTX_NOT_OPEN, "ECONNRESET");
      }

      if (errno == ENOTCONN) {
        close();
        throw TTransportException(TTX_NOT_OPEN, "ENOTCONN");
      }

      perror("TSocket::write() send < 0");
      throw TTransportException(TTX_UNKNOWN, "ERROR:" + errno);
    }
    
    // Fail on blocked send
    if (b == 0) {
      throw TTransportException(TTX_NOT_OPEN, "Socket send returned 0.");
    }
    sent += b;
  }
}

void TSocket::setLinger(bool on, int linger) {
  // TODO(mcslee): Store these options so they can be set pre-connect
  if (socket_ <= 0) {
    return;
  }

  struct linger ling = {(on ? 1 : 0), linger};
  if (-1 == setsockopt(socket_, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling))) {
    close();
    perror("TSocket::setLinger()");
  }
}

void TSocket::setNoDelay(bool noDelay) {
  // TODO(mcslee): Store these options so they can be set pre-connect
  if (socket_ <= 0) {
    return;
  }

  // Set socket to NODELAY
  int val = (noDelay ? 1 : 0);
  if (-1 == setsockopt(socket_, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val))) {
    close();
    perror("TSocket::setNoDelay()");
  }
}
