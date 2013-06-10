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

#ifndef _THRIFT_TRANSPORT_TSERVERWINPIPES_H_
#define _THRIFT_TRANSPORT_TSERVERWINPIPES_H_ 1

#include <thrift/transport/TServerTransport.h>
#include <boost/shared_ptr.hpp>
#ifndef _WIN32
#  include "TServerSocket.h"
#endif

#define TPIPE_SERVER_MAX_CONNS_DEFAULT 10

namespace apache { namespace thrift { namespace transport {

/**
 * Windows Pipes implementation of TServerTransport.
 */
#ifdef _WIN32
class TPipeServer : public TServerTransport {
 public:
  //Constructors
  // Named Pipe -
  TPipeServer(const std::string &pipename, uint32_t bufsize);
  TPipeServer(const std::string &pipename, uint32_t bufsize, uint32_t maxconnections);
  TPipeServer(const std::string &pipename);
  // Anonymous pipe -
  TPipeServer(int bufsize);
  TPipeServer();

  //Destructor
  ~TPipeServer();

  //Standard transport callbacks
  void interrupt();
  void close();
 protected:
  boost::shared_ptr<TTransport> acceptImpl();

  bool TCreateNamedPipe();
  bool TCreateAnonPipe();
  void createWakeupEvent();

 public:
  //Accessors
  std::string getPipename();
  void setPipename(const std::string &pipename);
  int  getBufferSize();
  void setBufferSize(int bufsize);
  HANDLE getPipeHandle();  //Named Pipe R/W -or- Anonymous pipe Read handle
  HANDLE getWrtPipeHandle();
  HANDLE getClientRdPipeHandle();
  HANDLE getClientWrtPipeHandle();
  bool getAnonymous();
  void setAnonymous(bool anon);

 private:
  std::string pipename_;
  uint32_t bufsize_;
  HANDLE Pipe_;  //Named Pipe (R/W) or Anonymous Pipe (R)
  uint32_t maxconns_;
  HANDLE PipeW_; //Anonymous Pipe (W)
  HANDLE ClientAnonRead, ClientAnonWrite; //Client side anonymous pipe handles
  HANDLE wakeup;  // wake up event
  //? Do we need duplicates to send to client?
  bool isAnonymous;
  bool stop_; // stop flag
};
#else //_WIN32
//*NIX named pipe implementation uses domain socket
typedef TServerSocket TPipeServer;
#endif

}}} // apache::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TSERVERWINPIPES_H_
