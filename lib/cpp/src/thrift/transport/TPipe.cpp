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

#include "TTransportException.h"
#include "TPipe.h"

namespace apache { namespace thrift { namespace transport {

using namespace std;

/**
* TPipe implementation.
*/

//---- Constructors ----
TPipe::TPipe(int Pipe) :
  pipename_(""),
  Pipe_(Pipe),
  TimeoutSeconds_(3),
  isAnonymous(false)
{
#ifndef _WIN32
  GlobalOutput.perror("TPipe: constructor using a pipe handle is not supported under *NIX", -99);
  throw TTransportException(TTransportException::NOT_OPEN, " constructor using a pipe handle is not supported under *NIX");
#endif
}

TPipe::TPipe(string pipename) :
  pipename_(pipename),
  Pipe_(-1),
  TimeoutSeconds_(3),
  isAnonymous(false)
{
#ifdef _WIN32
    if(pipename_.find("\\\\") == -1) {
      pipename_ = "\\\\.\\pipe\\" + pipename_;
    }
#else
  dsocket.reset(new TSocket(pipename));
#endif
}

TPipe::TPipe(int PipeRd, int PipeWrt) :
  pipename_(""),
  Pipe_(PipeRd),
  PipeWrt_(PipeWrt),
  TimeoutSeconds_(3),
  isAnonymous(true)
{
#ifndef _WIN32
  GlobalOutput.perror("TPipe: Anonymous pipes not yet supported under *NIX", -99);
  throw TTransportException(TTransportException::NOT_OPEN, " Anonymous pipes not yet supported under *NIX");
#endif
}

  TPipe::TPipe() :
  pipename_(""),
  Pipe_(-1),
  TimeoutSeconds_(3)
{
#ifndef _WIN32
  GlobalOutput.perror("TPipe: Anonymous pipes not yet supported under *NIX", -99);
  throw TTransportException(TTransportException::NOT_OPEN, " Anonymous pipes not yet supported under *NIX");
#endif
}

//---- Destructor ----
TPipe::~TPipe() {
  close();
}


bool TPipe::isOpen() {
  return (Pipe_ != -1);
}

//---------------------------------------------------------
// Transport callbacks
//---------------------------------------------------------

#ifdef _WIN32 //Windows callbacks

bool TPipe::peek() {
  if (!isOpen()) {
    return false;
  }
  DWORD bytesavail = 0;
  int  PeekRet = 0;
  PeekRet = PeekNamedPipe((HANDLE)Pipe_, NULL, 0, NULL, &bytesavail, NULL); 
  return (PeekRet != 0 && bytesavail > 0);
}

void TPipe::open() {
  if (isOpen()) {
    return;
  }

  int SleepInterval = 500; //ms
  int retries = TimeoutSeconds_ * 1000 / SleepInterval;
  HANDLE hPipe_;
  for(int i=0; i<retries; i++)
  {
    hPipe_ = CreateFile( 
              pipename_.c_str(),
              GENERIC_READ | GENERIC_WRITE, 
              0,              // no sharing 
              NULL,           // default security attributes
              OPEN_EXISTING,  // opens existing pipe 
              0,              // default attributes 
              NULL);          // no template file 

    if ((int)hPipe_ == -1) 
      sleep(SleepInterval);
    else
      break;
  }
  if ((int)hPipe_ == -1) 
    throw TTransportException(TTransportException::NOT_OPEN, "Unable to open pipe");

  // The pipe connected; change to message-read mode. 
  DWORD dwMode = PIPE_READMODE_MESSAGE; 
  int fSuccess = SetNamedPipeHandleState( 
              hPipe_, // pipe handle 
              &dwMode,  // new pipe mode 
              NULL,     // don't set maximum bytes 
              NULL);    // don't set maximum time 
  if (fSuccess == 0)
  {
    throw TTransportException(TTransportException::NOT_OPEN, "SetNamedPipeHandleState failed");
    close();
  }
  Pipe_ = (int)hPipe_;
}


void TPipe::close() {
  if (isOpen())
  {
    CloseHandle((HANDLE)Pipe_);
    Pipe_ = -1;
  }
}

uint32_t TPipe::read(uint8_t* buf, uint32_t len) {
  if (!isOpen())
    throw TTransportException(TTransportException::NOT_OPEN, "Called read on non-open pipe");

  DWORD  cbRead; 
  int fSuccess = ReadFile( 
              (HANDLE)Pipe_, // pipe handle 
              buf,      // buffer to receive reply 
              len,      // size of buffer 
              &cbRead,  // number of bytes read 
              NULL);    // not overlapped 

  if ( !fSuccess && GetLastError() != ERROR_MORE_DATA )
    return 0; // No more data, possibly because client disconnected.

  return cbRead;
}

void TPipe::write(const uint8_t* buf, uint32_t len) {
  if (!isOpen())
    throw TTransportException(TTransportException::NOT_OPEN, "Called write on non-open pipe");

  int WritePipe = isAnonymous? PipeWrt_: Pipe_;
  DWORD  cbWritten; 
  int fSuccess = WriteFile( 
              (HANDLE)WritePipe, // pipe handle 
              buf,        // message 
              len,        // message length 
              &cbWritten, // bytes written 
              NULL);      // not overlapped 

  if ( !fSuccess) 
    throw TTransportException(TTransportException::NOT_OPEN, "Write to pipe failed");
}

#else //*NIX callbacks implemented via Unix Domain Sockets.
bool TPipe::peek() {
  return dsocket->peek();
}

void TPipe::open() {
  dsocket->open();
}

void TPipe::close() {
  dsocket->close();
}

uint32_t TPipe::read(uint8_t* buf, uint32_t len) {
  return dsocket->read(buf, len);
}

void TPipe::write(const uint8_t* buf, uint32_t len) {
  dsocket->write(buf, len);
}
#endif //callbacks


//---------------------------------------------------------
// Accessors
//---------------------------------------------------------

string TPipe::getPipename() {
  return pipename_;
}

void TPipe::setPipename(std::string pipename) {
  pipename_ = pipename;
}

int TPipe::getPipeHandle() {
  return Pipe_;
}

void TPipe::setPipeHandle(int pipehandle) {
  Pipe_ = pipehandle;
}

int TPipe::getWrtPipeHandle() {
  return PipeWrt_;
}

void TPipe::setWrtPipeHandle(int pipehandle) {
  PipeWrt_ = pipehandle;
}

long TPipe::getConnectTimeout() {
  return TimeoutSeconds_;
}

void TPipe::setConnectTimeout(long seconds) {
  TimeoutSeconds_ = seconds;
}

}}} // apache::thrift::transport
