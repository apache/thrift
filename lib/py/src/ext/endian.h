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

#ifndef THRIFT_PY_ENDIAN_H
#define THRIFT_PY_ENDIAN_H

#include <Python.h>

#ifdef _WIN32
#include <winsock2.h>
#pragma comment(lib, "ws2_32.lib")
#define inline __inline
#else
#include <netinet/in.h>

static inline unsigned long long ntohll(unsigned long long n) {
  union {
    unsigned long long f;
    unsigned char t[8];
  } u;
  u.f = n;
  return static_cast<unsigned long long>(u.t[0]) << 56
         | static_cast<unsigned long long>(u.t[1]) << 48
         | static_cast<unsigned long long>(u.t[2]) << 40
         | static_cast<unsigned long long>(u.t[3]) << 32
         | static_cast<unsigned long long>(u.t[4]) << 24
         | static_cast<unsigned long long>(u.t[5]) << 16
         | static_cast<unsigned long long>(u.t[6]) << 8 | static_cast<unsigned long long>(u.t[7]);
}

#define htonll(n) ntohll(n)

#endif // !_WIN32

static inline unsigned long long letohll(unsigned long long n) {
  union {
    unsigned long long f;
    unsigned char t[8];
  } u;
  u.f = n;
  return static_cast<unsigned long long>(u.t[0]) | static_cast<unsigned long long>(u.t[1]) << 8
         | static_cast<unsigned long long>(u.t[2]) << 16
         | static_cast<unsigned long long>(u.t[3]) << 24
         | static_cast<unsigned long long>(u.t[4]) << 32
         | static_cast<unsigned long long>(u.t[5]) << 40
         | static_cast<unsigned long long>(u.t[6]) << 48
         | static_cast<unsigned long long>(u.t[7]) << 56;
}

#define htolell(n) letohll(n)

#endif // THRIFT_PY_ENDIAN_H
