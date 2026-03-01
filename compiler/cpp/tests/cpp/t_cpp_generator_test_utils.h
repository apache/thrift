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

#ifndef T_CPP_GENERATOR_TEST_UTILS_H
#define T_CPP_GENERATOR_TEST_UTILS_H

#include "../catch/catch.hpp"
#include <thrift/parse/t_program.h>
#include <thrift/generate/t_generator.h>
#include <thrift/generate/t_generator_registry.h>
#include <thrift/globals.h>
#include <thrift/main.h>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <algorithm>

// Provided by compiler/cpp/tests/thrift_test_parser_support.cc
extern std::string g_curdir;
extern std::string g_curpath;

namespace cpp_generator_test_utils {

// Helper function to read file content
inline std::string read_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        return "";
    }
    std::ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

// Get the source directory of the current test file
inline std::string source_dir() {
    std::string file = __FILE__;
    std::replace(file.begin(), file.end(), '\\', '/');
    size_t slash = file.rfind('/');
    return (slash == std::string::npos) ? std::string(".") : file.substr(0, slash);
}

// Join two path components
inline std::string join_path(const std::string& a, const std::string& b) {
    if (a.empty()) {
        return b;
    }
    if (a.back() == '/' || a.back() == '\\') {
        return a + b;
    }
    return a + "/" + b;
}

// Normalize generated code for comparison by removing comments and extra whitespace
inline std::string normalize_for_compare(std::string s) {
    s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());

    std::istringstream in(s);
    std::ostringstream out;
    std::string line;
    bool in_block_comment = false;
    bool first = true;
    while (std::getline(in, line)) {
        while (!line.empty() && (line.back() == ' ' || line.back() == '\t')) {
            line.pop_back();
        }

        const auto first_non_ws = line.find_first_not_of(" \t");
        if (first_non_ws == std::string::npos) {
            continue;
        }

        const std::string trimmed = line.substr(first_non_ws);

        if (in_block_comment) {
            if (trimmed.find("*/") != std::string::npos) {
                in_block_comment = false;
            }
            continue;
        }

        if (trimmed.size() >= 2 && trimmed.compare(0, 2, "//") == 0) {
            continue;
        }

        if (trimmed.size() >= 2 && trimmed.compare(0, 2, "/*") == 0) {
            if (trimmed.find("*/") == std::string::npos) {
                in_block_comment = true;
            }
            continue;
        }

        if (!first) {
            out << '\n';
        }
        first = false;
        out << line;
    }

    return out.str();
}

// Parse a thrift program for testing
inline void parse_thrift_for_test(t_program* program) {
    REQUIRE(program != nullptr);

    // These globals are used by the parser; see thrift/globals.h.
    g_program = program;
    g_scope = program->scope();
    g_parent_scope = nullptr;
    g_parent_prefix = program->get_name() + ".";

    g_curpath = program->get_path();
    g_curdir = directory_name(g_curpath);

    // Pass 1: scan includes (even if none) to match the compiler behavior.
    g_parse_mode = INCLUDES;
    yylineno = 1;
    yyin = std::fopen(g_curpath.c_str(), "r");
    REQUIRE(yyin != nullptr);
    REQUIRE(yyparse() == 0);
    std::fclose(yyin);
    yyin = nullptr;

    // Pass 2: parse program.
    g_parse_mode = PROGRAM;
    yylineno = 1;
    yyin = std::fopen(g_curpath.c_str(), "r");
    REQUIRE(yyin != nullptr);
    REQUIRE(yyparse() == 0);
    std::fclose(yyin);
    yyin = nullptr;
}

} // namespace cpp_generator_test_utils

#endif // T_CPP_GENERATOR_TEST_UTILS_H
