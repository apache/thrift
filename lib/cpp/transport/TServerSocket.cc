#include <sys/socket.h>
#include <netinet/in.h>

#include "transport/TSocket.h"
#include "transport/TServerSocket.h"

TServerSocket::TServerSocket(int port) :
  port_(port), serverSocket_(0), acceptBacklog_(1024) {}

TServerSocket::~TServerSocket() {
  close();
}

bool TServerSocket::listen() {
  serverSocket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (serverSocket_ == -1) {
    close();
    return false;
  }

  // Set reusaddress to prevent 2MSL delay on accept
  int one = 1;
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_REUSEADDR,
                       &one, sizeof(one))) {
    perror("TServerSocket::listen() SO_REUSEADDR");
    close();
    return false;
  }

  // Turn linger off, don't want to block on calls to close
  struct linger ling = {0, 0};
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_LINGER,
                       &ling, sizeof(ling))) {
    perror("TServerSocket::listen() SO_LINGER");
    close();
    return false;
  }

  // Bind to a port
  struct sockaddr_in addr;
  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port_);
  addr.sin_addr.s_addr = INADDR_ANY;
  if (-1 == bind(serverSocket_, (struct sockaddr *)&addr, sizeof(addr))) {
    char errbuf[1024];
    sprintf(errbuf, "TServerSocket::listen() BIND %d", port_);
    perror(errbuf);
    close();
    return false;
  }

  // Call listen
  if (-1 == ::listen(serverSocket_, acceptBacklog_)) {
    perror("TServerSocket::listen() LISTEN");
    close();
    return false;
  }

  // The socket is now listening!
  return true;
}

TTransport* TServerSocket::accept() {
  if (serverSocket_ <= 0) {
    // TODO(mcslee): Log error with common logging tool
    return NULL;
  }

  struct sockaddr_in clientAddress;
  int size = sizeof(clientAddress);
  int clientSocket = ::accept(serverSocket_,
                              (struct sockaddr *) &clientAddress,
                              (socklen_t *) &size);
    
  if (clientSocket <= 0) {
    perror("TServerSocket::accept()");
    return NULL;
  }

  return new TSocket(clientSocket);
}

void TServerSocket::close() {
  if (serverSocket_ > 0) {
    shutdown(serverSocket_, SHUT_RDWR);
    ::close(serverSocket_);
  }
  serverSocket_ = 0;
}
