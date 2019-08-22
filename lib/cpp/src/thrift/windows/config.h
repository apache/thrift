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
#error "This is a Windows header only"
#endif

// Something that defines PRId64 is required to build
#define HAVE_INTTYPES_H 1

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif

#if defined(_M_IX86) || defined(_M_X64)
#define ARITHMETIC_RIGHT_SHIFT 1
#define SIGNED_RIGHT_SHIFT_IS 1
#endif

#ifndef __MINGW32__
#pragma warning(disable : 4996) // Deprecated posix name.
#endif

#define HAVE_GETTIMEOFDAY 1
#define HAVE_SYS_STAT_H 1

#include <stdint.h>

#include <thrift/transport/PlatformSocket.h>
#include <thrift/windows/GetTimeOfDay.h>
#include <thrift/windows/Operators.h>
#include <thrift/windows/TWinsockSingleton.h>
#include <thrift/windows/WinFcntl.h>
#include <thrift/windows/SocketPair.h>

// windows
#include <Winsock2.h>
#include <ws2tcpip.h>
#ifndef __MINGW32__
  #ifdef _WIN32_WCE
  #pragma comment(lib, "Ws2.lib")
  #else
  #pragma comment(lib, "Ws2_32.lib")
  #pragma comment(lib, "gdi32.lib") // For static OpenSSL
  #pragma comment(lib, "crypt32.lib") // For static OpenSSL
  #pragma comment(lib, "user32.lib") // For static OpenSSL
  #pragma comment(lib, "advapi32.lib") // For security APIs in TPipeServer
  #pragma comment(lib, "Shlwapi.lib")  // For StrStrIA in TPipeServer
  #endif
#endif // __MINGW32__

#endif // _THRIFT_WINDOWS_CONFIG_H_
