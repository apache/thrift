#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <errno.h>

#include "TSocket.h"
#include "TServerSocket.h"
#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace transport { 

using namespace boost;

TServerSocket::TServerSocket(int port) :
  port_(port),
  serverSocket_(-1),
  acceptBacklog_(1024),
  sendTimeout_(0),
  recvTimeout_(0) {}

TServerSocket::TServerSocket(int port, int sendTimeout, int recvTimeout) :
  port_(port),
  serverSocket_(-1),
  acceptBacklog_(1024),
  sendTimeout_(sendTimeout),
  recvTimeout_(recvTimeout) {}

TServerSocket::~TServerSocket() {
  close();
}

void TServerSocket::setSendTimeout(int sendTimeout) {
  sendTimeout_ = sendTimeout;
}

void TServerSocket::setRecvTimeout(int recvTimeout) {
  recvTimeout_ = recvTimeout;
}

void TServerSocket::listen() {
  serverSocket_ = socket(AF_INET, SOCK_STREAM, 0);
  if (serverSocket_ == -1) {
    perror("TServerSocket::listen() socket");
    close();
    throw TTransportException(TTX_NOT_OPEN, "Could not create server socket.");
  }

  // Set reusaddress to prevent 2MSL delay on accept
  int one = 1;
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_REUSEADDR,
                       &one, sizeof(one))) {
    perror("TServerSocket::listen() SO_REUSEADDR");
    close();
    throw TTransportException(TTX_NOT_OPEN, "Could not set SO_REUSEADDR");
  }

  // Defer accept
  #ifdef TCP_DEFER_ACCEPT
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, TCP_DEFER_ACCEPT,
                       &one, sizeof(one))) {
    perror("TServerSocket::listen() TCP_DEFER_ACCEPT");
    close();
    throw TTransportException(TTX_NOT_OPEN, "Could not set TCP_DEFER_ACCEPT");
  }
  #endif // #ifdef TCP_DEFER_ACCEPT

  // Turn linger off, don't want to block on calls to close
  struct linger ling = {0, 0};
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_LINGER,
                       &ling, sizeof(ling))) {
    close();
    perror("TServerSocket::listen() SO_LINGER");
    throw TTransportException(TTX_NOT_OPEN, "Could not set SO_LINGER");
  }

  // TCP Nodelay, speed over bandwidth
  if (-1 == setsockopt(serverSocket_, IPPROTO_TCP, TCP_NODELAY,
                       &one, sizeof(one))) {
    close();
    perror("setsockopt TCP_NODELAY");
    throw TTransportException(TTX_NOT_OPEN, "Could not set TCP_NODELAY");
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
    throw TTransportException(TTX_NOT_OPEN, "Could not bind");
  }

  // Call listen
  if (-1 == ::listen(serverSocket_, acceptBacklog_)) {
    perror("TServerSocket::listen() LISTEN");
    close();
    throw TTransportException(TTX_NOT_OPEN, "Could not listen");
  }

  // The socket is now listening!
}

shared_ptr<TTransport> TServerSocket::acceptImpl() {
  if (serverSocket_ < 0) {
    throw TTransportException(TTX_NOT_OPEN, "TServerSocket not listening");
  }

  struct sockaddr_in clientAddress;
  int size = sizeof(clientAddress);
  int clientSocket = ::accept(serverSocket_,
                              (struct sockaddr *) &clientAddress,
                              (socklen_t *) &size);
    
  if (clientSocket < 0) {
    perror("TServerSocket::accept()");
    throw TTransportException(TTX_UNKNOWN, "ERROR:" + errno);
  }
  
  shared_ptr<TSocket> client(new TSocket(clientSocket));
  if (sendTimeout_ > 0) {
    client->setSendTimeout(sendTimeout_);
  }
  if (recvTimeout_ > 0) {
    client->setRecvTimeout(recvTimeout_);
  }                          
  return client;
}

void TServerSocket::close() {
  if (serverSocket_ >= 0) {
    shutdown(serverSocket_, SHUT_RDWR);
    ::close(serverSocket_);
  }
  serverSocket_ = -1;
}

}}} // facebook::thrift::transport
