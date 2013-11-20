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

#ifndef _THRIFT_TRANSPORT_TSERVERSOCKET_H_
#define _THRIFT_TRANSPORT_TSERVERSOCKET_H_ 1

#include <thrift/transport/TServerTransport.h>
#include <thrift/transport/PlatformSocket.h>
#include <thrift/cxxfunctional.h>
#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift { namespace transport {

class TSocket;

/**
 * Server socket implementation of TServerTransport. Wrapper around a unix
 * socket listen and accept calls.
 *
 */
class TServerSocket : public TServerTransport {
 public:
  typedef apache::thrift::stdcxx::function<void(THRIFT_SOCKET fd)> socket_func_t;

  const static int DEFAULT_BACKLOG = 1024;

  TServerSocket(int port);
  TServerSocket(int port, int sendTimeout, int recvTimeout);
  TServerSocket(std::string path);

  ~TServerSocket();

  void setSendTimeout(int sendTimeout);
  void setRecvTimeout(int recvTimeout);

  void setAcceptTimeout(int accTimeout);
  void setAcceptBacklog(int accBacklog);

  void setRetryLimit(int retryLimit);
  void setRetryDelay(int retryDelay);

  void setKeepAlive(bool keepAlive) {keepAlive_ = keepAlive;}

  void setTcpSendBuffer(int tcpSendBuffer);
  void setTcpRecvBuffer(int tcpRecvBuffer);

  // listenCallback gets called just before listen, and after all Thrift
  // setsockopt calls have been made.  If you have custom setsockopt
  // things that need to happen on the listening socket, this is the place to do it.
  void setListenCallback(const socket_func_t &listenCallback) { listenCallback_ = listenCallback; }

  // acceptCallback gets called after each accept call, on the newly created socket.
  // It is called after all Thrift setsockopt calls have been made.  If you have
  // custom setsockopt things that need to happen on the accepted
  // socket, this is the place to do it.
  void setAcceptCallback(const socket_func_t &acceptCallback) { acceptCallback_ = acceptCallback; }

  void listen();
  void close();

  void interrupt();
  int getPort();

 protected:
  boost::shared_ptr<TTransport> acceptImpl();
  virtual boost::shared_ptr<TSocket> createSocket(THRIFT_SOCKET client);

 private:
  int port_;
  std::string path_;
  THRIFT_SOCKET serverSocket_;
  int acceptBacklog_;
  int sendTimeout_;
  int recvTimeout_;
  int accTimeout_;
  int retryLimit_;
  int retryDelay_;
  int tcpSendBuffer_;
  int tcpRecvBuffer_;
  bool keepAlive_;

  THRIFT_SOCKET intSock1_;
  THRIFT_SOCKET intSock2_;

  socket_func_t listenCallback_;
  socket_func_t acceptCallback_;
};

}}} // apache::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TSERVERSOCKET_H_
