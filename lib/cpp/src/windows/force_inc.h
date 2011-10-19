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

#ifndef _THRIFT_WINDOWS_FORCEINC_H_
#define _THRIFT_WINDOWS_FORCEINC_H_

#if defined(_MSC_VER) && (_MSC_VER > 1200)
#pragma once
#endif // _MSC_VER

#ifndef _WIN32
#error This is a MSVC header only.
#endif

#define NOMINMAX
#define BOOST_ALL_NO_LIB 1
#define BOOST_THREAD_NO_LIB 1

#include "windows/config.h"

#undef gai_strerror
#define gai_strerror gai_strerrorA

#undef errno
#undef EINTR
#undef EINPROGRESS
#undef ECONNRESET
#undef ENOTCONN
#undef ETIMEDOUT
#undef EWOULDBLOCK
#undef EAGAIN
#undef EPIPE
#define errno ::WSAGetLastError()
#define EINPROGRESS WSAEINPROGRESS
#define EAGAIN WSAEWOULDBLOCK
#define EINTR WSAEINTR
#define ECONNRESET WSAECONNRESET
#define ENOTCONN WSAENOTCONN
#define ETIMEDOUT WSAETIMEDOUT
#define EWOULDBLOCK WSAEWOULDBLOCK
#define EPIPE WSAECONNRESET

#endif // _THRIFT_WINDOWS_FORCEINC_H_
