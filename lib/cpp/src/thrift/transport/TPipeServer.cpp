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

#ifdef _WIN32

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <cstring>

#include "TPipe.h"
#include "TPipeServer.h"
#include <boost/shared_ptr.hpp>
#include <AccCtrl.h>
#include <Aclapi.h>

namespace apache { namespace thrift { namespace transport {

using namespace std;
using boost::shared_ptr;

//---- Constructors ----
TPipeServer::TPipeServer(string pipename, uint32_t bufsize) :
  pipename_(pipename),
  bufsize_(bufsize),
  hPipe_(INVALID_HANDLE_VALUE),
  isAnonymous(false),
  maxconns_(TPIPE_SERVER_MAX_CONNS_DEFAULT)
 {}

TPipeServer::TPipeServer(string pipename, uint32_t bufsize, uint32_t maxconnections) :
  pipename_(pipename),
  bufsize_(bufsize),
  hPipe_(INVALID_HANDLE_VALUE),
  isAnonymous(false)
 {  //Restrict maxconns_ to 1-255
    if(maxconnections == 0)
      maxconns_ = 1;
    else if (maxconnections > 255)
      maxconns_ = 255;
	else
      maxconns_ = maxconnections;
 }

TPipeServer::TPipeServer(string pipename) :
  pipename_(pipename),
  bufsize_(1024),
  hPipe_(INVALID_HANDLE_VALUE),
  isAnonymous(false),
  maxconns_(TPIPE_SERVER_MAX_CONNS_DEFAULT)
 {}

TPipeServer::TPipeServer(int bufsize) : 
  pipename_(""),
  bufsize_(bufsize),
  hPipe_(INVALID_HANDLE_VALUE),
  isAnonymous(true),
  maxconns_(1)
 {
  //The anonymous pipe needs to be created first so that the server can
  //pass the handles on to the client before the serve (acceptImpl)
  //blocking call.
  if (!TCreateAnonPipe()) {
    GlobalOutput.perror("TPipeServer Create(Anon)Pipe failed, GLE=", GetLastError());
    throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer Create(Anon)Pipe failed");
  }
}

TPipeServer::TPipeServer() : 
  pipename_(""),
  bufsize_(1024),
  hPipe_(INVALID_HANDLE_VALUE),
  isAnonymous(true),
  maxconns_(1)
{
  if (!TCreateAnonPipe()) {
    GlobalOutput.perror("TPipeServer Create(Anon)Pipe failed, GLE=", GetLastError());
    throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer Create(Anon)Pipe failed");
  }
}

//---- Destructor ----
TPipeServer::~TPipeServer() {
  close();
}

//---------------------------------------------------------
// Transport callbacks
//---------------------------------------------------------

shared_ptr<TTransport> TPipeServer::acceptImpl() {
  shared_ptr<TPipe> client;

  if(isAnonymous)
  { //Anonymous Pipe
    //This 0-byte read serves merely as a blocking call.
    byte buf;
    DWORD br;
    int fSuccess = ReadFile( 
          hPipe_, // pipe handle 
          &buf,   // buffer to receive reply 
          0,      // size of buffer 
          &br,    // number of bytes read 
          NULL);  // not overlapped

    if ( !fSuccess && GetLastError() != ERROR_MORE_DATA ) {
      GlobalOutput.perror("TPipeServer unable to initiate pipe comms, GLE=", GetLastError());
      throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer unable to initiate pipe comms");
    }
	client.reset(new TPipe(hPipe_, hPipeW_));
  }
  else
  { //Named Pipe
    int ConnectRet;
    while (true)
    {
      if (!TCreateNamedPipe()) {
        GlobalOutput.perror("TPipeServer CreateNamedPipe failed, GLE=", GetLastError());
        throw TTransportException(TTransportException::NOT_OPEN, " TPipeServer CreateNamedPipe failed");
      }

      // Wait for the client to connect; if it succeeds, the
      // function returns a nonzero value. If the function returns 
      // zero, GetLastError should return ERROR_PIPE_CONNECTED. 
      ConnectRet = ConnectNamedPipe(hPipe_, NULL) ? 
                    TRUE : (GetLastError() == ERROR_PIPE_CONNECTED);

      if (ConnectRet == TRUE)
      {
        GlobalOutput.printf("Client connected.");
        break;
      }
      else
      {
        close();
        GlobalOutput.perror("TPipeServer ConnectNamedPipe GLE=", GetLastError());
        throw TTransportException(TTransportException::NOT_OPEN, "TPipeServer: client connection failed");
      }
    }
	client.reset(new TPipe(hPipe_));
  }

  return client;
}

void TPipeServer::interrupt() {
  if(hPipe_ != INVALID_HANDLE_VALUE) {
    CancelIo(hPipe_);
  }
}

void TPipeServer::close() {
  if(!isAnonymous)
  {
    if(hPipe_ != INVALID_HANDLE_VALUE) {
      DisconnectNamedPipe(hPipe_);
      CloseHandle(hPipe_);
      hPipe_ = INVALID_HANDLE_VALUE;
    }
  }
  else
  {
    try {
      CloseHandle(hPipe_);
      CloseHandle(hPipeW_);
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
  hPipe_ = CreateNamedPipe( 
        pipename_.c_str(),        // pipe name 
        PIPE_ACCESS_DUPLEX,       // read/write access 
        PIPE_TYPE_MESSAGE |       // message type pipe 
        PIPE_READMODE_MESSAGE,    // message-read mode 
        maxconns_,                // max. instances  
        bufsize_,                 // output buffer size 
        bufsize_,                 // input buffer size 
        0,                        // client time-out 
        &sa);                     // default security attribute 

  return (hPipe_ != INVALID_HANDLE_VALUE);
}

bool TPipeServer::TCreateAnonPipe() {
  SECURITY_ATTRIBUTES sa;
  SECURITY_DESCRIPTOR sd; //security information for pipes

  InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(&sd, true, NULL, false);
  sa.lpSecurityDescriptor = &sd;
  sa.lpSecurityDescriptor = NULL;
  sa.nLength = sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle = true; //allow passing handle to child

  if (!CreatePipe(&ClientAnonRead,&hPipeW_,&sa,0))   //create stdin pipe
  {
    GlobalOutput.perror("TPipeServer CreatePipe (anon) failed, GLE=", GetLastError());
    return false;
  }
  if (!CreatePipe(&hPipe_,&ClientAnonWrite,&sa,0))  //create stdout pipe
  {
    GlobalOutput.perror("TPipeServer CreatePipe (anon) failed, GLE=", GetLastError());
    CloseHandle(ClientAnonRead);
    CloseHandle(hPipeW_);
    return false;
  }

  return true;
}


//---------------------------------------------------------
// Accessors
//---------------------------------------------------------

string TPipeServer::getPipename() {
  return pipename_;
}

void TPipeServer::setPipename(std::string pipename) {
  pipename_ = pipename;
}

int  TPipeServer::getBufferSize() {
  return bufsize_;
}

void TPipeServer::setBufferSize(int bufsize) {
  bufsize_ = bufsize;
}

HANDLE TPipeServer::getPipeHandle() {
  return hPipe_;
}

HANDLE TPipeServer::getWrtPipeHandle()
{
  return hPipeW_;
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

}}} // apache::thrift::transport

#endif //_WIN32
