#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <unistd.h>
#include <errno.h>

#include "transport/TSocket.h"

using namespace std;

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

bool TSocket::open() {
  // Create socket
  socket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_ == -1) {
    socket_ = 0;
    return false;
  }
  
  // Lookup the host
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
      return false;
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
    return false;
  }

  return true;
}

void TSocket::close() {
  if (socket_ > 0) {
    shutdown(socket_, SHUT_RDWR);
    ::close(socket_);
  }
  socket_ = 0;
}

int TSocket::read(string& s, uint32_t len) {
  char buff[len];
  s = "";

  uint32_t have = 0;
  uint32_t retries = 0;

  while (have < len) {
  try_again:
    // Read from the socket
    int got = recv(socket_, buff+have, len-have, 0);

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
        return 0;
      }

      return 0;
    }
    
    // Check for empty read
    if (got == 0) {
      return 0;
    }
    
    // Update the count
    have += (uint32_t) got;
  }
  
  // Pack data into string
  s = string(buff, have);
  return have;
}

void TSocket::write(const string& s) {
  uint32_t sent = 0;
    
  while (sent < s.size()) {
    int b = send(socket_, s.data() + sent, s.size() - sent, 0);
    
    // Fail on a send error
    if (b < 0) {
      // TODO(mcslee): Make the function return how many bytes it wrote or
      // throw an exception
      // throw_perror("send");
      return;
    }
    
    // Fail on blocked send
    if (b == 0) {
      // TODO(mcslee): Make the function return how many bytes it wrote or
      // throw string("couldn't send data.\n");
      return;
    }

    sent += b;
  }
}

bool TSocket::setLinger(bool on, int linger) {
  struct linger ling = {(on ? 1 : 0), linger};
  if (-1 == setsockopt(socket_, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling))) {
    close();
    perror("TSocket::setLinger()");
    return false;
  }
  return true; 
}

bool TSocket::setNoDelay(bool noDelay) {
  // Set socket to NODELAY
  int val = (noDelay ? 1 : 0);
  if (-1 == setsockopt(socket_, IPPROTO_TCP, TCP_NODELAY, &val, sizeof(val))) {
    close();
    perror("TSocket::setNoDelay()");
    return false;
  }
  return true;
}
