/*
 * server with epoll, mutiprocess 
 */

#ifndef _THRIFT_TRANSPORT_TSERVEREPOLLSOCKET_H_
#define _THRIFT_TRANSPORT_TSERVEREPOLLSOCKET_H_ 1

#include "TServerTransport.h"
#include <boost/shared_ptr.hpp>
#include <sys/epoll.h>

namespace apache { namespace thrift { namespace transport {

#define MAX_CLIENTS 65535
typedef int SOCKET;

class TSocket;

/**
 * Server socket implementation of TServerTransport. Wrapper around a unix
 * socket listen and accept calls.
 *
 */
class TServerEpollSocket : public TServerTransport {
 public:
  TServerEpollSocket(int port);
  TServerEpollSocket(int port, int sendTimeout, int recvTimeout);

  ~TServerEpollSocket();

  void setSendTimeout(int sendTimeout);
  void setRecvTimeout(int recvTimeout);

  void setAcceptTimeout(int accTimeout);

  void setRetryLimit(int retryLimit);
  void setRetryDelay(int retryDelay);

  void setTcpSendBuffer(int tcpSendBuffer);
  void setTcpRecvBuffer(int tcpRecvBuffer);

  void listen();
  void close();

  void FaddWatch(bool bAdd = true);

  /* modify the epoll event */
  void FmodWatch(boost::shared_ptr<TTransport> client);
  
  void clearClient(boost::shared_ptr<TTransport> client);

 protected:
  boost::shared_ptr<TTransport> acceptImpl();
  virtual boost::shared_ptr<TSocket> createSocket(SOCKET client);

 private:
  int port_;
  SOCKET serverSocket_;
  int acceptBacklog_;
  int sendTimeout_;
  int recvTimeout_;
  int accTimeout_;
  int retryLimit_;
  int retryDelay_;
  int tcpSendBuffer_;
  int tcpRecvBuffer_;

  /* epoll fd and events one to one */
  int epoll_fd;
  struct epoll_event events[MAX_CLIENTS];
  boost::shared_ptr<TSocket> clients[MAX_CLIENTS];
};

}}} // apache::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TServerEpollSocket_H_
