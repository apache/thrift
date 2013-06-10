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

#ifndef _THRIFT_TRANSPORT_TPIPE_H_
#define _THRIFT_TRANSPORT_TPIPE_H_ 1

#include <thrift/transport/TTransport.h>
#include <thrift/transport/TVirtualTransport.h>
#ifndef _WIN32
#  include <thrift/transport/TSocket.h>
#endif

namespace apache { namespace thrift { namespace transport {

/**
 * Windows Pipes implementation of the TTransport interface.
 *
 */
#ifdef _WIN32
class TPipe : public TVirtualTransport<TPipe> {
 public:

  // Constructs a new pipe object.
  TPipe();
  // Named pipe constructors -
  explicit TPipe(HANDLE Pipe); //HANDLE is a void*
  //need a const char * overload so string literals don't go to the HANDLE overload
  explicit TPipe(const char *pipename);
  explicit TPipe(const std::string &pipename);
  // Anonymous pipe -
  TPipe(HANDLE PipeRd, HANDLE PipeWrt);

  // Destroys the pipe object, closing it if necessary.
  virtual ~TPipe();

  // Returns whether the pipe is open & valid.
  virtual bool isOpen();

  // Checks whether more data is available in the pipe.
  virtual bool peek();

  // Creates and opens the named/anonymous pipe.
  virtual void open();

  // Shuts down communications on the pipe.
  virtual void close();

  // Reads from the pipe.
  virtual uint32_t read(uint8_t* buf, uint32_t len);

  // Writes to the pipe.
  virtual void write(const uint8_t* buf, uint32_t len);


  //Accessors
  std::string getPipename();
  void setPipename(const std::string &pipename);
  HANDLE getPipeHandle(); //doubles as the read handle for anon pipe
  void setPipeHandle(HANDLE pipehandle);
  HANDLE getWrtPipeHandle();
  void setWrtPipeHandle(HANDLE pipehandle);
  long getConnectTimeout();
  void setConnectTimeout(long seconds);

 private:
  std::string pipename_;

  //Named pipe handles are R/W, while anonymous pipes are one or the other (half duplex).
  HANDLE Pipe_, PipeWrt_;
  long TimeoutSeconds_;
  bool isAnonymous;
};
#else
typedef TSocket TPipe;
#endif

}}} // apache::thrift::transport

#endif // #ifndef _THRIFT_TRANSPORT_TPIPE_H_

