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

#include <cstdio>
#include <memory>
#include <string>
#include <utility>
#include <vector>

struct yy_buffer_state;
extern yy_buffer_state* yy_scan_string(const char*);
extern yy_buffer_state* yy_scan_bytes(const char*, int);
extern yy_buffer_state* yy_create_buffer(FILE*, int);
extern void yy_switch_to_buffer(yy_buffer_state*);
extern int yylex_destroy();

using cpp_generator_test_utils::join_path;
using cpp_generator_test_utils::parse_thrift_for_test;
using cpp_generator_test_utils::source_dir;

TEST_CASE("parser preserves supported string escapes", "[parser]")
{
    const std::string path = join_path(source_dir(), "test_string_escapes.thrift");
    std::unique_ptr<t_program> program(new t_program(path, "test_string_escapes"));
    parse_thrift_for_test(program.get());

    const std::vector<std::pair<std::string, std::string>> expected = {
        {"DOUBLE_QUOTED", "Y-m-d\\TH:i:s.uP"},
        {"SINGLE_QUOTED", "Y-m-d\\TH:i:s.uP"},
        {"ALL_DOUBLE", "\r\n\t\"'\\"},
        {"ALL_SINGLE", "\r\n\t\"'\\"}
    };
    REQUIRE(program->get_consts().size() == expected.size());
    const t_scope& scope = *program->scope();
    for (const auto& entry : expected) {
        INFO(entry.first);
        const t_const* constant = scope.get_constant(entry.first);
        REQUIRE(constant != nullptr);
        REQUIRE(constant->get_value()->get_type() == t_const_value::CV_STRING);
        REQUIRE(constant->get_value()->get_string() == entry.second);
    }
}

TEST_CASE("lexer explains how to escape a literal backslash", "[parser]")
{
    // Clear scanner state left by earlier parser tests.
    yylex_destroy();
    for (const std::string& escape : {"T", "x41", "0", "u00e9"}) {
        for (const char quote : {'"', '\''}) {
            const std::string source = std::string(1, quote) + "\\" + escape + quote;
            INFO(source);
            // Reset scanner state even when the expected parser exception is thrown.
            auto cleanup = [](yy_buffer_state*) { yylex_destroy(); };
            std::unique_ptr<yy_buffer_state, decltype(cleanup)> buffer(
                yy_scan_string(source.c_str()), cleanup);
            const std::string expected = "Invalid escape sequence '\\" + escape.substr(0, 1)
                + "'. Use \\\\ for a literal backslash.\n";
            REQUIRE_THROWS_WITH(yylex(), expected);
        }
    }
}

TEST_CASE("lexer diagnoses incomplete and non-printable escapes", "[parser]")
{
    yylex_destroy();
    const std::vector<std::pair<std::string, std::string>> cases = {
        {"", "End of file while reading string at 1\n"},
        {"\n", "End of line while reading string at 1\n"},
        {"\r\n", "Invalid escape byte 0x0D. Use \\\\ for a literal backslash.\n"},
        {"\t", "Invalid escape byte 0x09. Use \\\\ for a literal backslash.\n"},
        {"\x01", "Invalid escape byte 0x01. Use \\\\ for a literal backslash.\n"},
        {"\x7f", "Invalid escape byte 0x7F. Use \\\\ for a literal backslash.\n"},
        {"\xc3\xa9", "Invalid escape byte 0xC3. Use \\\\ for a literal backslash.\n"},
        {"\xff", "Invalid escape byte 0xFF. Use \\\\ for a literal backslash.\n"},
        {std::string(1, '\0'), "Invalid escape byte 0x00. Use \\\\ for a literal backslash.\n"},
        {std::string("\0tail", 5), "Invalid escape byte 0x00. Use \\\\ for a literal backslash.\n"}
    };
    for (const auto& test : cases) {
        for (const char quote : {'"', '\''}) {
            const std::string source = std::string(1, quote) + "abc\\" + test.first;
            INFO(test.second);
            for (const bool from_file : {false, true}) {
                INFO(from_file);
                auto close_file = [](FILE* handle) { std::fclose(handle); };
                std::unique_ptr<FILE, decltype(close_file)> file(nullptr, close_file);
                auto cleanup = [](yy_buffer_state*) { yylex_destroy(); };
                std::unique_ptr<yy_buffer_state, decltype(cleanup)> buffer(nullptr, cleanup);
                if (from_file) {
                    file.reset(std::tmpfile());
                    REQUIRE(file != nullptr);
                    REQUIRE(std::fwrite(source.data(), 1, source.size(), file.get()) == source.size());
                    std::rewind(file.get());
                    buffer.reset(yy_create_buffer(file.get(), 8));
                    yy_switch_to_buffer(buffer.get());
                } else {
                    buffer.reset(yy_scan_bytes(source.data(), static_cast<int>(source.size())));
                }
                REQUIRE_THROWS_WITH(yylex(), test.second);
            }
        }
    }
}

TEST_CASE("lexer diagnoses an unterminated string at end of file", "[parser]")
{
    yylex_destroy();
    for (const char quote : {'"', '\''}) {
        const std::string source = std::string(1, quote) + "abc";
        INFO(source);
        auto cleanup = [](yy_buffer_state*) { yylex_destroy(); };
        std::unique_ptr<yy_buffer_state, decltype(cleanup)> buffer(
            yy_scan_bytes(source.data(), static_cast<int>(source.size())), cleanup);
        REQUIRE_THROWS_WITH(yylex(), "End of file while reading string at 1\n");
    }
}

TEST_CASE("lexer keeps reading a string after a literal NUL", "[parser]")
{
    yylex_destroy();
    for (const char quote : {'"', '\''}) {
        const std::string source = std::string(1, quote) + std::string("a\0b", 3) + quote;
        INFO(quote);
        auto cleanup = [](yy_buffer_state*) { yylex_destroy(); };
        std::unique_ptr<yy_buffer_state, decltype(cleanup)> buffer(
            yy_scan_bytes(source.data(), static_cast<int>(source.size())), cleanup);
        REQUIRE_NOTHROW(yylex());
    }
}
