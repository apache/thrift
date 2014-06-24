/*
 * TServerEpollSocket with epoll and multi process
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <cstring>
#include <sys/types.h>
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
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
#include <errno.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "TSocket.h"
#include "TServerEpollSocket.h"
#include <boost/shared_ptr.hpp>

#ifndef AF_LOCAL
#define AF_LOCAL AF_UNIX
#endif

#ifndef SOCKOPT_CAST_T
#   ifndef _WIN32
#       define SOCKOPT_CAST_T void
#   else
#       define SOCKOPT_CAST_T char
#   endif // _WIN32
#endif

template<class T>
inline const SOCKOPT_CAST_T* const_cast_sockopt(const T* v) {
    return reinterpret_cast<const SOCKOPT_CAST_T*>(v);
}

template<class T>
inline SOCKOPT_CAST_T* cast_sockopt(T* v) {
    return reinterpret_cast<SOCKOPT_CAST_T*>(v);
}

namespace apache { namespace thrift { namespace transport {

using namespace std;
using boost::shared_ptr;

TServerEpollSocket::TServerEpollSocket(int port) :
  port_(port),
  serverSocket_(-1),
  acceptBacklog_(1024),
  sendTimeout_(0),
  recvTimeout_(0),
  accTimeout_(-1),
  retryLimit_(0),
  retryDelay_(0),
  tcpSendBuffer_(0),
  tcpRecvBuffer_(0)
   {}

TServerEpollSocket::TServerEpollSocket(int port, int sendTimeout, int recvTimeout) :
  port_(port),
  serverSocket_(-1),
  acceptBacklog_(1024),
  sendTimeout_(sendTimeout),
  recvTimeout_(recvTimeout),
  accTimeout_(-1),
  retryLimit_(0),
  retryDelay_(0),
  tcpSendBuffer_(0),
  tcpRecvBuffer_(0)
   {}

TServerEpollSocket::~TServerEpollSocket() {
  close();
}

void TServerEpollSocket::setSendTimeout(int sendTimeout) {
  sendTimeout_ = sendTimeout;
}

void TServerEpollSocket::setRecvTimeout(int recvTimeout) {
  recvTimeout_ = recvTimeout;
}

void TServerEpollSocket::setAcceptTimeout(int accTimeout) {
  accTimeout_ = accTimeout;
}

void TServerEpollSocket::setRetryLimit(int retryLimit) {
  retryLimit_ = retryLimit;
}

void TServerEpollSocket::setRetryDelay(int retryDelay) {
  retryDelay_ = retryDelay;
}

void TServerEpollSocket::setTcpSendBuffer(int tcpSendBuffer) {
  tcpSendBuffer_ = tcpSendBuffer;
}

void TServerEpollSocket::setTcpRecvBuffer(int tcpRecvBuffer) {
  tcpRecvBuffer_ = tcpRecvBuffer;
}

void TServerEpollSocket::listen() {
  struct sockaddr_in localAddr;
  localAddr.sin_addr.s_addr = INADDR_ANY;
  localAddr.sin_family = AF_INET;
  localAddr.sin_port = htons(port_);

  /*create socket fd */
  serverSocket_ = socket(AF_INET, SOCK_STREAM, 0);

  if (serverSocket_ == -1) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() socket() ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not create server socket.", errno_copy);
  }

  // Set reusaddress to prevent 2MSL delay on accept
  int one = 1;
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_REUSEADDR,
                       cast_sockopt(&one), sizeof(one))) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() setsockopt() SO_REUSEADDR ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not set SO_REUSEADDR", errno_copy);
  }

  // Set TCP buffer sizes
  if (tcpSendBuffer_ > 0) {
    if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_SNDBUF,
                         cast_sockopt(&tcpSendBuffer_), sizeof(tcpSendBuffer_))) {
      int errno_copy = errno;
      GlobalOutput.perror("TServerEpollSocket::listen() setsockopt() SO_SNDBUF ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN, "Could not set SO_SNDBUF", errno_copy);
    }
  }

  if (tcpRecvBuffer_ > 0) {
    if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_RCVBUF,
                         cast_sockopt(&tcpRecvBuffer_), sizeof(tcpRecvBuffer_))) {
      int errno_copy = errno;
      GlobalOutput.perror("TServerEpollSocket::listen() setsockopt() SO_RCVBUF ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN, "Could not set SO_RCVBUF", errno_copy);
    }
  }

  // Defer accept
  #ifdef TCP_DEFER_ACCEPT
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, TCP_DEFER_ACCEPT,
                       &one, sizeof(one))) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() setsockopt() TCP_DEFER_ACCEPT ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not set TCP_DEFER_ACCEPT", errno_copy);
  }
  #endif // #ifdef TCP_DEFER_ACCEPT

  
  // Turn linger off, don't want to block on calls to close
  struct linger ling = {0, 0};
  if (-1 == setsockopt(serverSocket_, SOL_SOCKET, SO_LINGER,
                       cast_sockopt(&ling), sizeof(ling))) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() setsockopt() SO_LINGER ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not set SO_LINGER", errno_copy);
  }

  // Set NONBLOCK on the accept socket
  int flags = fcntl(serverSocket_, F_GETFL, 0);
  if (flags == -1) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() fcntl() F_GETFL ", errno_copy);
    throw TTransportException(TTransportException::NOT_OPEN, "fcntl() failed", errno_copy);
  }

  if (-1 == fcntl(serverSocket_, F_SETFL, flags | O_NONBLOCK)) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() fcntl() O_NONBLOCK ", errno_copy);
    throw TTransportException(TTransportException::NOT_OPEN, "fcntl() failed", errno_copy);
  }

  int retries = 0;

  if (-1 == ::bind(serverSocket_, (struct sockaddr *)(&localAddr), sizeof(struct sockaddr))) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::bind() bind() ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not bind", errno_copy);

  }

  // Call listen
  if (-1 == ::listen(serverSocket_, acceptBacklog_)) {
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::listen() listen() ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not listen", errno_copy);
  }

  // The socket is now listening!

  // Now create epoll and then add watch
  FaddWatch(false);

}

void TServerEpollSocket::FaddWatch(bool bAdd){
  epoll_fd = epoll_create(MAX_CLIENTS);
  if(-1 == epoll_fd){
    int errno_copy = errno;
    GlobalOutput.perror("TServerEpollSocket::FaddWatch() FaddWatch() ", errno_copy);
    close();
    throw TTransportException(TTransportException::NOT_OPEN, "Could not epoll_create", errno_copy);
  }

  if(bAdd){
    struct epoll_event ev;
    ev.events = EPOLLIN;
    ev.data.fd = serverSocket_;

    if(-1 == epoll_ctl(epoll_fd, EPOLL_CTL_ADD, serverSocket_, &ev)){
      int errno_copy = errno;
      GlobalOutput.perror("TServerEpollSocket::FaddWatch() epoll_ctl() ", errno_copy);
      close();
      throw TTransportException(TTransportException::NOT_OPEN, "Could not epoll_ctl", errno_copy);
    }
  }
}

shared_ptr<TTransport> TServerEpollSocket::acceptImpl() {
  if (serverSocket_ == -1) {
    throw TTransportException(TTransportException::NOT_OPEN, "TServerEpollSocket not listening");
  }

  struct epoll_event tmp_events[1];
  int fds = epoll_wait(epoll_fd, tmp_events, 1 /* > 1 fds will return */, 
          1000/* 10 million seconds will timemout */);


  for(int i = 0; i < fds; ++i){
      if(tmp_events[i].data.fd == serverSocket_){
          //accept events
          struct sockaddr_storage clientAddress;
          int size = sizeof(clientAddress);
          SOCKET clientSocket = ::accept(serverSocket_,
                                  (struct sockaddr *) &clientAddress,
                              (socklen_t *) &size);
          if(clientSocket <= 0){
              fprintf(stderr, "accept return :%d and errmsg:%s\n",
                        clientSocket, strerror(errno));
          }
          // Make sure client socket is blocking
          int flags = fcntl(clientSocket, F_GETFL, 0);
          if (flags == -1) {
            int errno_copy = errno;
            fprintf(stderr, "ssssssss:msg:%s\n", strerror(errno));
            GlobalOutput.perror("TServerEpollSocket::acceptImpl() fcntl() F_GETFL ", errno_copy);
            throw TTransportException(TTransportException::UNKNOWN, "fcntl(F_GETFL)", errno_copy);
          }

          if (-1 == fcntl(clientSocket, F_SETFL, flags | O_NONBLOCK)) {
            int errno_copy = errno;
            GlobalOutput.perror("TServerEpollSocket::acceptImpl() fcntl() F_SETFL ~O_NONBLOCK ", errno_copy);
            throw TTransportException(TTransportException::UNKNOWN, "fcntl(F_SETFL)", errno_copy);
          }

          shared_ptr<TSocket> client = createSocket(clientSocket);
          if (sendTimeout_ > 0) {
            client->setSendTimeout(sendTimeout_);
          }
          if (recvTimeout_ > 0) {
            client->setRecvTimeout(recvTimeout_);
          }
          client->setCachedAddress((sockaddr*) &clientAddress, size);

          if(clients[clientSocket])
              clients[clientSocket].reset();

          clients[clientSocket] = client;
          events[clientSocket].data.fd = clientSocket;
          events[clientSocket].events = EPOLLIN;

          epoll_ctl(epoll_fd, EPOLL_CTL_ADD, clientSocket, &events[clientSocket]);

          shared_ptr<TTransport> null_ptr;
          
          return null_ptr;  //no need to deal with. just ignore

      }else if(tmp_events[i].events & EPOLLIN){
          //read events
          //get socket client from array
          int fd = tmp_events[i].data.fd;
          if(fd > 0){

            shared_ptr<TSocket> client = clients[fd];
            if(client != NULL){

              //change the events
              return client;
            }
          }
      }else if(tmp_events[i].events & EPOLLOUT){
          //write events
          //get socket client from array
          int fd = tmp_events[i].data.fd;
          if(fd > 0){
            shared_ptr<TSocket> client = clients[fd];
            if(client != NULL){
                return client;
            }
          }
      }
  }
  shared_ptr<TTransport> null_ptr;
  return null_ptr;
}

shared_ptr<TSocket> TServerEpollSocket::createSocket(SOCKET clientSocket) {
  return shared_ptr<TSocket>(new TSocket(clientSocket));
}

void TServerEpollSocket::FmodWatch(shared_ptr<TTransport> client) {
    int fd = boost::static_pointer_cast<TSocket>(client)->getSocketFD();

    events[fd].data.fd = fd;
    events[fd].events = EPOLLIN;
    epoll_ctl(epoll_fd, EPOLL_CTL_MOD, fd, &events[fd]);
}

void TServerEpollSocket::clearClient(shared_ptr<TTransport> client) {
    int fd = boost::static_pointer_cast<TSocket>(client)->getSocketFD();
    epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, &(events[fd]));
    events[fd].data.fd = -1;
    client->close();
    clients[fd].reset();
}

void TServerEpollSocket::close() {
  if (serverSocket_ != -1) {

      shutdown(serverSocket_, SHUT_RDWR);
      ::close(serverSocket_);

  }
  serverSocket_ = -1;
}

}}} // apache::thrift::transport
