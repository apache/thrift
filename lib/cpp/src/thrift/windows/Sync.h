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

#ifndef _THRIFT_WINDOWS_Sync_H_
#define _THRIFT_WINDOWS_Sync_H_ 1

#ifndef _WIN32
#error "windows/Sync.h is only usable on Windows"
#endif

#include <thrift/concurrency/Exception.h>
#include <thrift/TNonCopyable.h>

// Including Windows.h can conflict with Winsock2 usage, and also
// adds problematic macros like min() and max(). Try to work around:
#ifndef NOMINMAX
#define NOMINMAX
#define _THRIFT_UNDEF_NOMINMAX
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#define _THRIFT_UNDEF_WIN32_LEAN_AND_MEAN
#endif
#include <Windows.h>
#ifdef _THRIFT_UNDEF_NOMINMAX
#undef NOMINMAX
#undef _THRIFT_UNDEF_NOMINMAX
#endif
#ifdef _THRIFT_UNDEF_WIN32_LEAN_AND_MEAN
#undef WIN32_LEAN_AND_MEAN
#undef _THRIFT_UNDEF_WIN32_LEAN_AND_MEAN
#endif

/*
  Lightweight synchronization objects that only make sense on Windows.  For cross-platform
  code, use the classes found in the concurrency namespace
*/

namespace apache {
namespace thrift {

struct TCriticalSection : apache::thrift::TNonCopyable {
  CRITICAL_SECTION cs;
  TCriticalSection() { InitializeCriticalSection(&cs); }
  ~TCriticalSection() { DeleteCriticalSection(&cs); }
};

class TAutoCrit : apache::thrift::TNonCopyable {
private:
  CRITICAL_SECTION* cs_;

public:
  explicit TAutoCrit(TCriticalSection& cs) : cs_(&cs.cs) { EnterCriticalSection(cs_); }
  ~TAutoCrit() { LeaveCriticalSection(cs_); }
};

struct TAutoResetEvent : apache::thrift::TNonCopyable {
  HANDLE h;

  TAutoResetEvent() {
    h = CreateEvent(nullptr, FALSE, FALSE, nullptr);
    if (h == nullptr) {
      GlobalOutput.perror("TAutoResetEvent unable to create event, GLE=", GetLastError());
      throw apache::thrift::concurrency::SystemResourceException("CreateEvent failed");
    }
  }
  ~TAutoResetEvent() { CloseHandle(h); }
};

struct TManualResetEvent : apache::thrift::TNonCopyable {
  HANDLE h;

  TManualResetEvent() {
    h = CreateEvent(nullptr, TRUE, FALSE, nullptr);
    if (h == nullptr) {
      GlobalOutput.perror("TManualResetEvent unable to create event, GLE=", GetLastError());
      throw apache::thrift::concurrency::SystemResourceException("CreateEvent failed");
    }
  }
  ~TManualResetEvent() { CloseHandle(h); }
};

struct TAutoHandle : apache::thrift::TNonCopyable {
  HANDLE h;
  explicit TAutoHandle(HANDLE h_ = INVALID_HANDLE_VALUE) : h(h_) {}
  ~TAutoHandle() {
    if (h != INVALID_HANDLE_VALUE)
      CloseHandle(h);
  }

  HANDLE release() {
    HANDLE retval = h;
    h = INVALID_HANDLE_VALUE;
    return retval;
  }
  void reset(HANDLE h_ = INVALID_HANDLE_VALUE) {
    if (h_ == h)
      return;
    if (h != INVALID_HANDLE_VALUE)
      CloseHandle(h);
    h = h_;
  }
};
}
} // apache::thrift

#endif
