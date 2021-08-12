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

#include <thrift/windows/WinFcntl.h>

int thrift_fcntl(THRIFT_SOCKET fd, int cmd, int flags) {
  if (cmd != THRIFT_F_GETFL && cmd != THRIFT_F_SETFL) {
    return -1;
  }

  if (flags != THRIFT_O_NONBLOCK && flags != 0) {
    return -1;
  }

  if (cmd == THRIFT_F_GETFL) {
    return 0;
  }

  int res;
  if (flags) {
    res = ioctlsocket(fd, FIONBIO, reinterpret_cast<u_long*>(&(flags = 1)));
  } else {
    res = ioctlsocket(fd, FIONBIO, reinterpret_cast<u_long*>(&(flags = 0)));
  }

  return res;
}

#ifdef _WIN32_WCE
std::string thrift_wstr2str(std::wstring ws) {
  std::string s(ws.begin(), ws.end());
  return s;
}
#endif
