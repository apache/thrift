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

#ifndef THRIFT_TEST_CPP_TESTKEYSDIR_H
#define THRIFT_TEST_CPP_TESTKEYSDIR_H

#include <fstream>
#include <string>

#ifdef _WIN32
#include <direct.h>
#define THRIFT_TEST_GETCWD _getcwd
#else
#include <unistd.h>
#define THRIFT_TEST_GETCWD getcwd
#endif

// Returns the directory that holds keys/<file>, searched upward from the
// directory of the executable named by argv0, or an empty string.
inline std::string findTestKeysDir(const char* argv0, const char* file) {
  std::string dir = argv0;
  const bool absolute
      = !dir.empty() && (dir[0] == '/' || dir[0] == '\\' || (dir.size() > 1 && dir[1] == ':'));
  if (!absolute) {
    char cwd[4096];
    if (THRIFT_TEST_GETCWD(cwd, sizeof(cwd)) == nullptr) {
      return std::string();
    }
    dir = std::string(cwd) + "/" + dir;
  }
  for (;;) {
    std::string::size_type sep = dir.find_last_of("/\\");
    if (sep == std::string::npos) {
      return std::string();
    }
    dir.erase(sep);
    if (std::ifstream(dir + "/keys/" + file).good()) {
      return dir.empty() ? "/" : dir;
    }
  }
}

#undef THRIFT_TEST_GETCWD

#endif // THRIFT_TEST_CPP_TESTKEYSDIR_H
