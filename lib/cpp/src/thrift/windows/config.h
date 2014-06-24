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

#ifndef _THRIFT_WINDOWS_CONFIG_H_
#define _THRIFT_WINDOWS_CONFIG_H_ 1

#if defined(_MSC_VER) && (_MSC_VER > 1200)
#pragma once
#endif // _MSC_VER

#ifndef _WIN32
#error This is a MSVC header only.
#endif

#pragma warning(disable: 4996) // Depreciated posix name.
#pragma warning(disable: 4250) // Inherits via dominance.

#define VERSION "0.9.0"
#define HAVE_GETTIMEOFDAY 1
#define HAVE_SYS_STAT_H 1

#include "TargetVersion.h"
#include "GetTimeOfDay.h"
#include "Operators.h"
#include "TWinsockSingleton.h"
#include "WinFcntl.h"
#include "SocketPair.h"

// boost
#include <boost/cstdint.hpp>

typedef boost::int64_t  int64_t;
typedef boost::uint32_t uint32_t;
typedef boost::uint8_t  uint8_t;

// windows
#include <Winsock2.h>
#include <ws2tcpip.h>
#pragma comment(lib, "Ws2_32.lib")
#pragma comment(lib, "advapi32.lib") //For security APIs in TPipeServer

// pthreads
#if 0
#	include <pthread.h>
#else
struct timespec {
  int64_t tv_sec;
  int64_t tv_nsec;
};
#	define USE_BOOST_THREAD 1
#	define ctime_r( _clock, _buf ) \
        ( strcpy( (_buf), ctime( (_clock) ) ),  \
          (_buf) )
#endif

typedef ptrdiff_t ssize_t;

// Missing functions.
#define usleep(ms) Sleep(ms)
inline int sleep(DWORD ms)
{
    Sleep(ms);
    return 0;
}

#if WINVER <= 0x0502 //XP, Server2003
#define POLLIN  0x0300
#define POLLOUT 0x0010
#define poll(fds, nfds, timeout) \
    poll_win32(fds, nfds, timeout)

typedef struct pollfd {
  SOCKET  fd;
  SHORT   events;
  SHORT   revents;
} WSAPOLLFD, *PWSAPOLLFD, FAR *LPWSAPOLLFD;

inline int poll_win32(LPWSAPOLLFD fdArray, ULONG nfds, INT timeout)
{
  fd_set read_fds, write_fds;
  fd_set* read_fds_ptr  = NULL;
  fd_set* write_fds_ptr = NULL;

  FD_ZERO(&read_fds);
  FD_ZERO(&write_fds);

  for(ULONG i=0; i<nfds; i++) {
    //Read (in) socket
    if((fdArray[i].events & POLLIN) == POLLIN) {
      read_fds_ptr = &read_fds;
      FD_SET(fdArray[i].fd, &read_fds);
    }
    //Write (out) socket
    else if((fdArray[i].events & POLLOUT) == POLLOUT) {
      write_fds_ptr = &write_fds;
      FD_SET(fdArray[i].fd, &write_fds);
    }
  }

  timeval time_out;
  timeval* time_out_ptr = NULL;
  if(timeout >= 0) {
    timeval time_out = {timeout / 1000, timeout * 1000};
    time_out_ptr = &time_out;
  }
  else { //to avoid compiler warnings
    (void)time_out;
    (void)timeout;
  }

  int sktready = select(1, read_fds_ptr, write_fds_ptr, NULL, time_out_ptr);
  if(sktready > 0) {
    for(ULONG i=0; i<nfds; i++) {
      fdArray[i].revents = 0;
      if(FD_ISSET(fdArray[i].fd, &read_fds))
        fdArray[i].revents |= POLLIN;
      if(FD_ISSET(fdArray[i].fd, &write_fds))
        fdArray[i].revents |= POLLOUT;
    }
  }
  return sktready;
}
#else //Vista, Win7...
  inline int poll(struct pollfd* fdArray, ULONG fds, INT timeout) {
    return WSAPoll(fdArray, fds, timeout);
  }
#endif // WINVER

inline int close(SOCKET socket)
{
    return ::closesocket(socket);
}

#endif // _THRIFT_WINDOWS_CONFIG_H_
