// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
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

#include "t_cpp_generator_test_utils.h"

#include <memory>
#include <string>
#include <utility>
#include <vector>

struct yy_buffer_state;
extern yy_buffer_state* yy_scan_bytes(const char*, int);
extern int yylex_destroy();

TEST_CASE("lexer diagnoses an unterminated comment at end of file", "[parser]")
{
    yylex_destroy();
    const std::vector<std::pair<std::string, std::string>> cases = {
        {"/* unterminated", "Unexpected end of file in multiline comment at 1\n"},
        {"/** unterminated", "Unexpected end of file in doc-comment at 1\n"}
    };
    for (const auto& test : cases) {
        INFO(test.first);
        auto cleanup = [](yy_buffer_state*) { yylex_destroy(); };
        std::unique_ptr<yy_buffer_state, decltype(cleanup)> buffer(
            yy_scan_bytes(test.first.data(), static_cast<int>(test.first.size())), cleanup);
        REQUIRE_THROWS_WITH(yylex(), test.second);
    }
}

TEST_CASE("lexer keeps reading a comment after a literal NUL", "[parser]")
{
    yylex_destroy();
    for (const std::string& opener : {"/*", "/**"}) {
        const std::string source = opener + std::string(" a\0b ", 5) + "*/";
        INFO(opener);
        auto cleanup = [](yy_buffer_state*) { yylex_destroy(); };
        std::unique_ptr<yy_buffer_state, decltype(cleanup)> buffer(
            yy_scan_bytes(source.data(), static_cast<int>(source.size())), cleanup);
        REQUIRE_NOTHROW(yylex());
    }
}
