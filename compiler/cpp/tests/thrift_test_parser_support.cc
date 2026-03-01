// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements.See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

// Minimal parser support for compiler unit tests.
//
// The Bison grammar (thrifty.yy) references a set of functions and globals that
// are normally provided by src/thrift/main.cc. The compiler unit tests build
// does not compile main.cc (it would conflict with the Catch2 main), but some
// tests still need to parse .thrift files.
//
// This file provides lightweight implementations sufficient for unit tests.


#include "thrift/globals.h"
#include "thrift/main.h"

#include <algorithm>
#include <cstdarg>
#include <cstdio>
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <vector>

// Provided by compiler/cpp/tests/thrift_test_globals.cc (not declared in public headers)
extern std::string g_curdir;
extern std::string g_curpath;
extern std::vector<std::string> g_incl_searchpath;

// Error reporting used by the parser.
void yyerror(const char* fmt, ...) {
  std::fprintf(stderr, "[ERROR:%s:%d] ", g_curpath.c_str(), yylineno);
  va_list args;
  va_start(args, fmt);
  std::vfprintf(stderr, fmt, args);
  va_end(args);
  std::fprintf(stderr, "\n");

  throw std::runtime_error("thrift parser error");
}

// Simplified helpers referenced by the grammar.
std::string program_name(std::string filename) {
  filename.erase(std::remove(filename.begin(), filename.end(), '\\'), filename.end());
  std::string::size_type slash = filename.rfind('/');
  if (slash != std::string::npos) {
    filename = filename.substr(slash + 1);
  }
  std::string::size_type dot = filename.rfind('.');
  if (dot != std::string::npos) {
    filename = filename.substr(0, dot);
  }
  return filename;
}

std::string directory_name(std::string filename) {
  std::replace(filename.begin(), filename.end(), '\\', '/');
  std::string::size_type slash = filename.rfind('/');
  if (slash == std::string::npos) {
    return ".";
  }
  return filename.substr(0, slash);
}

std::string include_file(std::string filename) {
  // Unit tests only parse fixtures without includes. Provide a best-effort
  // resolution to satisfy the linker and any unexpected includes.
  if (!filename.empty() && (filename[0] == '/' || filename.find(":/") != std::string::npos)) {
    return filename;
  }
  // Search current dir first.
  if (!g_curdir.empty()) {
    return g_curdir + "/" + filename;
  }
  return filename;
}

void clear_doctext() {
  if (g_doctext != nullptr) {
    std::free(g_doctext);
    g_doctext = nullptr;
  }
}

char* clean_up_doctext(char* doctext) {
  // Keep behavior minimal for unit tests.
  return doctext;
}

void declare_valid_program_doctext() {
  if ((g_program_doctext_candidate != nullptr) && (g_program_doctext_status == STILL_CANDIDATE)) {
    g_program_doctext_status = ABSOLUTELY_SURE;
  } else {
    g_program_doctext_status = NO_PROGRAM_DOCTEXT;
  }
}

void validate_simple_identifier(const char* identifier) {
  if (identifier == nullptr) {
    return;
  }
  const std::string name(identifier);
  if (name.find('.') != std::string::npos) {
    yyerror("Identifier %s can't have a dot.", identifier);
  }
}

void validate_const_type(t_const* /*c*/) {
  // Not needed for current unit tests.
}

void validate_field_value(t_field* /*field*/, t_const_value* /*cv*/) {
  // Not needed for current unit tests.
}

bool validate_throws(t_struct* /*throws*/) {
  return true;
}

void check_for_list_of_bytes(t_type* /*list_elem_type*/) {
  // Not needed for current unit tests.
}

void emit_byte_type_warning() {
  // Not needed for current unit tests.
}

void error_unsupported_namespace_decl(const char* old_form, const char* new_form) {
  // Treat as fatal in unit tests.
  if (new_form == nullptr) {
    yyerror("Unsupported declaration '%s_namespace'", old_form);
  } else {
    yyerror("Unsupported declaration '%s'", old_form);
  }
}
