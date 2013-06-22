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

#include <thrift/thrift-config.h>
#include <cstring>

#include <thrift/transport/TPipe.h>
#include <thrift/transport/TPipeServer.h>
#include <boost/shared_ptr.hpp>
#ifdef _WIN32
#  include <AccCtrl.h>
#  include <Aclapi.h>
#endif //_WIN32

namespace apache { namespace thrift { namespace transport {

#ifdef _WIN32

using namespace std;
using boost::shared_ptr;

//---- Constructors ----
TPipeServer::TPipeServer(const std::string &pipename, uint32_t bufsize) :
  pipename_(pipename),
  bufsize_(bufsize),
  Pipe_(INVALID_HANDLE_VALUE),
  wakeup(INVALID_HANDLE_VALUE),
  maxconns_(TPIPE_SERVER_MAX_CONNS_DEFAULT),
  isAnonymous(false),
  stop_(false)
 {
    setPipename(pipename);
    createWakeupEvent();
 }

TPipeServer::TPipeServer(const std::string &pipename, uint32_t bufsize, uint32_t maxconnections) :
  pipename_(pipename),
  bufsize_(bufsize),
  Pipe_(INVALID_HANDLE_VALUE),
  wakeup(INVALID_HANDLE_VALUE),
  isAnonymous(false),
  stop_(false)
 {  //Restrict maxconns_ to 1-PIPE_UNLIMITED_INSTANCES
    if(maxconnections == 0)
      maxconns_ = 1;
    else if (maxconnections > PIPE_UNLIMITED_INSTANCES)
      maxconns_ = PIPE_UNLIMITED_INSTANCES;
	else
      maxconns_ = maxconnections;

    setPipename(pipename);
    createWakeupEvent();
 }

TPipeServer::TPipeServer(const std::string &pipename) :
  pipename_(pipename),
  bufsize_(1024),
  Pipe_(INVALID_HANDLE_VALUE),
  wakeup(INVALID_HANDLE_VALUE),
  maxconns_(TPIPE_SERVER_MAX_CONNS_DEFAULT),
  isAnonymous(false),
  stop_(false)
 {
    setPipename(pipename);
    createWakeupEvent();
 }

TPipeServer::TPipeServer(int bufsize) :
  pipename_(""),
  bufsize_(bufsize),
  Pipe_(INVALID_HANDLE_VALUE),
  wakeup(INVALID_HANDLE_VALUE),
  maxconns_(1),
  isAnonymous(true),
  stop_(false)
 {
  //The anonymous pipe needs to be created first so that the server can
  //pass the handles on to the client before the serve (acceptImpl)
  //blocking call.
  if (!TCreateAnonPipe()) {
    GlobalOutput.perror("TPipeServer Create(Anon)Pipe failed, GLE=", GetLastError());
    throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer Create(Anon)Pipe failed");
  }
  createWakeupEvent();
}

TPipeServer::TPipeServer() :
  pipename_(""),
  bufsize_(1024),
  Pipe_(INVALID_HANDLE_VALUE),
  wakeup(INVALID_HANDLE_VALUE),
  maxconns_(1),
  isAnonymous(true),
  stop_(false)
{
  if (!TCreateAnonPipe()) {
    GlobalOutput.perror("TPipeServer Create(Anon)Pipe failed, GLE=", GetLastError());
    throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer Create(Anon)Pipe failed");
  }
  createWakeupEvent();
}

//---- Destructor ----
TPipeServer::~TPipeServer() {
  close();
  CloseHandle( wakeup);
  wakeup = INVALID_HANDLE_VALUE;
}

//---------------------------------------------------------
// Transport callbacks
//---------------------------------------------------------

shared_ptr<TTransport> TPipeServer::acceptImpl() {
  shared_ptr<TPipe> client;

  stop_ = FALSE;

  if(isAnonymous)
  { //Anonymous Pipe
    //This 0-byte read serves merely as a blocking call.
    byte buf;
    DWORD br;
    int fSuccess = ReadFile(
          Pipe_, // pipe handle
          &buf,   // buffer to receive reply
          0,      // size of buffer
          &br,    // number of bytes read
          NULL);  // not overlapped

    if ( !fSuccess && GetLastError() != ERROR_MORE_DATA ) {
      GlobalOutput.perror("TPipeServer unable to initiate pipe comms, GLE=", GetLastError());
      throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer unable to initiate pipe comms");
    }
    client.reset(new TPipe(Pipe_, PipeW_));
  }
  else
  { //Named Pipe
    if (!TCreateNamedPipe()) {
      GlobalOutput.perror("TPipeServer CreateNamedPipe failed, GLE=", GetLastError());
      throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer CreateNamedPipe failed");
    }

    struct TEventCleaner {
      HANDLE hEvent;
      ~TEventCleaner() {CloseHandle(hEvent);}
    };

    OVERLAPPED overlapped;
    memset( &overlapped, 0, sizeof(overlapped));
    overlapped.hEvent = CreateEvent( NULL, TRUE, FALSE, NULL);
    {
      TEventCleaner cleaner = {overlapped.hEvent};
      while( ! stop_)
      {
        // Wait for the client to connect; if it succeeds, the
        // function returns a nonzero value. If the function returns
        // zero, GetLastError should return ERROR_PIPE_CONNECTED.
        if( ConnectNamedPipe(Pipe_, &overlapped))
        {
          GlobalOutput.printf("Client connected.");
          client.reset(new TPipe(Pipe_));
          return client;
        }

        DWORD dwErr = GetLastError();
        HANDLE events[2] = {overlapped.hEvent, wakeup};
        switch( dwErr)
        {
        case ERROR_PIPE_CONNECTED:
          GlobalOutput.printf("Client connected.");
          client.reset(new TPipe(Pipe_));
          return client;

        case ERROR_IO_PENDING:
          DWORD dwWait, dwDummy;
          dwWait = WaitForMultipleObjects( 2, events, FALSE, 3000);
          switch(dwWait)
          {
          case WAIT_OBJECT_0:
            if(GetOverlappedResult(Pipe_, &overlapped, &dwDummy, TRUE))
            {
              GlobalOutput.printf("Client connected.");
              client.reset(new TPipe(Pipe_));
              return client;
            }
            break;
          case WAIT_OBJECT_0 + 1:
            stop_ = TRUE;
            break;
          default:
            break;
          }
          break;

        default:
          break;
        }

        CancelIo(Pipe_);
        DisconnectNamedPipe(Pipe_);
      }

      close();
      GlobalOutput.perror("TPipeServer ConnectNamedPipe GLE=", GetLastError());
      throw TTransportException(TTransportException::NOT_OPEN, "TPipeServer: client connection failed");
    }
  }

  return client;
}

void TPipeServer::interrupt() {
  if(Pipe_ != INVALID_HANDLE_VALUE) {
    stop_ = TRUE;
    CancelIo(Pipe_);
    SetEvent(wakeup);
  }
}

void TPipeServer::close() {
  if(!isAnonymous)
  {
    if(Pipe_ != INVALID_HANDLE_VALUE) {
      DisconnectNamedPipe(Pipe_);
      CloseHandle(Pipe_);
      Pipe_ = INVALID_HANDLE_VALUE;
    }
  }
  else
  {
    try {
      CloseHandle(Pipe_);
      CloseHandle(PipeW_);
      CloseHandle(ClientAnonRead);
      CloseHandle(ClientAnonWrite);
    }
    catch(...) {
        GlobalOutput.perror("TPipeServer anon close GLE=", GetLastError());
    }
  }
}


bool TPipeServer::TCreateNamedPipe() {

  //Windows - set security to allow non-elevated apps
  //to access pipes created by elevated apps.
  SID_IDENTIFIER_AUTHORITY SIDAuthWorld = SECURITY_WORLD_SID_AUTHORITY;
  PSID everyone_sid = NULL;
  AllocateAndInitializeSid(&SIDAuthWorld, 1, SECURITY_WORLD_RID, 0, 0, 0, 0, 0, 0, 0, &everyone_sid);

  EXPLICIT_ACCESS ea;
  ZeroMemory(&ea, sizeof(EXPLICIT_ACCESS));
  ea.grfAccessPermissions = SPECIFIC_RIGHTS_ALL | STANDARD_RIGHTS_ALL;
  ea.grfAccessMode = SET_ACCESS;
  ea.grfInheritance = NO_INHERITANCE;
  ea.Trustee.TrusteeForm = TRUSTEE_IS_SID;
  ea.Trustee.TrusteeType = TRUSTEE_IS_WELL_KNOWN_GROUP;
  ea.Trustee.ptstrName  = (LPSTR)everyone_sid;

  PACL acl = NULL;
  SetEntriesInAcl(1, &ea, NULL, &acl);

  PSECURITY_DESCRIPTOR sd = (PSECURITY_DESCRIPTOR)LocalAlloc(LPTR,SECURITY_DESCRIPTOR_MIN_LENGTH);
  InitializeSecurityDescriptor(sd, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(sd, TRUE, acl, FALSE);

  SECURITY_ATTRIBUTES sa;
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.lpSecurityDescriptor = sd;
  sa.bInheritHandle = FALSE;

  // Create an instance of the named pipe
  HANDLE hPipe_ = CreateNamedPipe(
        pipename_.c_str(),        // pipe name
        PIPE_ACCESS_DUPLEX |      // read/write access
        FILE_FLAG_OVERLAPPED,     // async mode
        PIPE_TYPE_MESSAGE |       // message type pipe
        PIPE_READMODE_MESSAGE,    // message-read mode
        maxconns_,                // max. instances
        bufsize_,                 // output buffer size
        bufsize_,                 // input buffer size
        0,                        // client time-out
        &sa);                     // default security attribute

  if(hPipe_ == INVALID_HANDLE_VALUE)
  {
    Pipe_ = INVALID_HANDLE_VALUE;
    GlobalOutput.perror("TPipeServer::TCreateNamedPipe() GLE=", GetLastError());
    throw TTransportException(TTransportException::NOT_OPEN, "TCreateNamedPipe() failed", GetLastError());
    return false;
  }

  Pipe_ = hPipe_;
  return true;
}

bool TPipeServer::TCreateAnonPipe() {
  SECURITY_ATTRIBUTES sa;
  SECURITY_DESCRIPTOR sd; //security information for pipes

  InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(&sd, true, NULL, false);
  sa.lpSecurityDescriptor = &sd;
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle = true; //allow passing handle to child

  HANDLE ClientAnonReadH, PipeW_H, ClientAnonWriteH, Pipe_H;
  if (!CreatePipe(&ClientAnonReadH,&PipeW_H,&sa,0))   //create stdin pipe
  {
    GlobalOutput.perror("TPipeServer CreatePipe (anon) failed, GLE=", GetLastError());
    return false;
  }
  if (!CreatePipe(&Pipe_H,&ClientAnonWriteH,&sa,0))  //create stdout pipe
  {
    GlobalOutput.perror("TPipeServer CreatePipe (anon) failed, GLE=", GetLastError());
    CloseHandle(ClientAnonReadH);
    CloseHandle(PipeW_H);
    return false;
  }
  ClientAnonRead  = ClientAnonReadH;
  ClientAnonWrite = ClientAnonWriteH;
  Pipe_  = Pipe_H;
  PipeW_ = PipeW_H;

  return true;
}

void TPipeServer::createWakeupEvent() {
  wakeup = CreateEvent( NULL, TRUE, FALSE, NULL);
}


//---------------------------------------------------------
// Accessors
//---------------------------------------------------------

string TPipeServer::getPipename() {
  return pipename_;
}

void TPipeServer::setPipename(const std::string &pipename) {
  if(pipename.find("\\\\") == -1)
    pipename_ = "\\\\.\\pipe\\" + pipename;
  else
    pipename_ = pipename;
}

int  TPipeServer::getBufferSize() {
  return bufsize_;
}

void TPipeServer::setBufferSize(int bufsize) {
  bufsize_ = bufsize;
}

HANDLE TPipeServer::getPipeHandle() {
  return Pipe_;
}

HANDLE TPipeServer::getWrtPipeHandle()
{
  return PipeW_;
}

HANDLE TPipeServer::getClientRdPipeHandle()
{
  return ClientAnonRead;
}

HANDLE TPipeServer::getClientWrtPipeHandle()
{
  return ClientAnonWrite;
}

bool TPipeServer::getAnonymous() {
  return isAnonymous;
}

void TPipeServer::setAnonymous(bool anon) {
  isAnonymous = anon;
}
#endif //_WIN32

}}} // apache::thrift::transport
