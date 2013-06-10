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

#include <thrift/transport/TTransportException.h>
#include <thrift/transport/TPipe.h>

namespace apache { namespace thrift { namespace transport {

using namespace std;

/**
* TPipe implementation.
*/

#ifdef _WIN32
//---- Constructors ----
TPipe::TPipe(HANDLE Pipe) :
  Pipe_(Pipe),
  TimeoutSeconds_(3),
  isAnonymous(false)
{}

TPipe::TPipe(const char *pipename) :
  Pipe_(INVALID_HANDLE_VALUE),
  TimeoutSeconds_(3),
  isAnonymous(false)
{
  setPipename(pipename);
}

TPipe::TPipe(const std::string &pipename) :
  Pipe_(INVALID_HANDLE_VALUE),
  TimeoutSeconds_(3),
  isAnonymous(false)
{
  setPipename(pipename);
}

TPipe::TPipe(HANDLE PipeRd, HANDLE PipeWrt) :
  Pipe_(PipeRd),
  PipeWrt_(PipeWrt),
  TimeoutSeconds_(3),
  isAnonymous(true)
{}

TPipe::TPipe() :
  Pipe_(INVALID_HANDLE_VALUE),
  TimeoutSeconds_(3)
{}

//---- Destructor ----
TPipe::~TPipe() {
  close();
}


//---------------------------------------------------------
// Transport callbacks
//---------------------------------------------------------

bool TPipe::isOpen() {
  return (Pipe_ != INVALID_HANDLE_VALUE);
}

bool TPipe::peek() {
  if (!isOpen()) {
    return false;
  }
  DWORD bytesavail = 0;
  int  PeekRet = 0;
  PeekRet = PeekNamedPipe(Pipe_, NULL, 0, NULL, &bytesavail, NULL);
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

    if (hPipe_ == INVALID_HANDLE_VALUE)
      ::Sleep(SleepInterval);
    else
      break;
  }
  if (hPipe_ == INVALID_HANDLE_VALUE)
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
  Pipe_ = hPipe_;
}


void TPipe::close() {
  if (isOpen())
  {
    CloseHandle(Pipe_);
    Pipe_ = INVALID_HANDLE_VALUE;
  }
}

uint32_t TPipe::read(uint8_t* buf, uint32_t len) {
  if (!isOpen())
    throw TTransportException(TTransportException::NOT_OPEN, "Called read on non-open pipe");

  DWORD  cbRead;
  int fSuccess = ReadFile(
              Pipe_, // pipe handle
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

  HANDLE WritePipe = isAnonymous? PipeWrt_: Pipe_;
  DWORD  cbWritten;
  int fSuccess = WriteFile(
              WritePipe, // pipe handle
              buf,        // message
              len,        // message length
              &cbWritten, // bytes written
              NULL);      // not overlapped

  if ( !fSuccess)
    throw TTransportException(TTransportException::NOT_OPEN, "Write to pipe failed");
}

//---------------------------------------------------------
// Accessors
//---------------------------------------------------------

string TPipe::getPipename() {
  return pipename_;
}

void TPipe::setPipename(const std::string &pipename) {
  if(pipename.find("\\\\") == -1)
    pipename_ = "\\\\.\\pipe\\" + pipename;
  else
    pipename_ = pipename;
}

HANDLE TPipe::getPipeHandle() {
  return Pipe_;
}

void TPipe::setPipeHandle(HANDLE pipehandle) {
  Pipe_ = pipehandle;
}

HANDLE TPipe::getWrtPipeHandle() {
  return PipeWrt_;
}

void TPipe::setWrtPipeHandle(HANDLE pipehandle) {
  PipeWrt_ = pipehandle;
}

long TPipe::getConnectTimeout() {
  return TimeoutSeconds_;
}

void TPipe::setConnectTimeout(long seconds) {
  TimeoutSeconds_ = seconds;
}
#endif //_WIN32

}}} // apache::thrift::transport
