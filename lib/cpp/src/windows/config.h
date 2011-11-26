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

#define VERSION "0.8.0"
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

#if WINVER <= 0x0502
#define poll(fds, nfds, timeout) \
    poll_win32(fds, nfds, timeout)

inline int poll_win32(LPWSAPOLLFD fdArray, ULONG fds, INT timeout)
{
    fd_set read_fds;
    fd_set write_fds;
    fd_set except_fds;

    FD_ZERO(&read_fds);
    FD_ZERO(&write_fds);
    FD_ZERO(&except_fds);

    FD_SET(fdArray[0].fd, &read_fds);
    FD_SET(fdArray[0].fd, &write_fds);
    FD_SET(fdArray[0].fd, &except_fds);

    timeval time_out = {timeout * 0.001, timeout * 1000};
    return select(1, &read_fds, &write_fds, &except_fds, &time_out);
}
#else
	inline int poll(struct pollfd* fdArray, ULONG fds, INT timeout) {
		return WSAPoll(fdArray, fds, timeout);
	}
#endif // WINVER

inline void close(SOCKET socket)
{
    ::closesocket(socket);
}

#endif // _THRIFT_WINDOWS_CONFIG_H_
