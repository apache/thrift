// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

#include "../cpp/t_cpp_generator_test_utils.h"

#include <algorithm>
#include <memory>
#include <string>

using std::map;
using std::string;
using cpp_generator_test_utils::join_path;
using cpp_generator_test_utils::parse_thrift_for_test;
using cpp_generator_test_utils::read_file;

namespace {

string zig_test_source_dir() {
    string file = __FILE__;
    std::replace(file.begin(), file.end(), '\\', '/');
    const size_t slash = file.rfind('/');
    return (slash == string::npos) ? string(".") : file.substr(0, slash);
}

} // namespace

TEST_CASE("t_zig_generator escapes reserved words in declarations and references", "[functional]")
{
    const string thrift_path = join_path(zig_test_source_dir(), "test_zig_reserved_word.thrift");
    const string program_name = "test_zig_reserved_word";

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, program_name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "zig", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("gen-zig/test_zig_reserved_word.zig");
    REQUIRE(!generated.empty());

    REQUIRE(generated.find("pub const KWEnum = enum(i32)") != string::npos);
    REQUIRE(generated.find("error_ = 1") != string::npos);
    REQUIRE(generated.find("pub const error_ = struct") != string::npos);
    REQUIRE(generated.find("return_: i32") != string::npos);
    REQUIRE(generated.find("init_: i32") != string::npos);
    REQUIRE(generated.find("test_: KWEnum") != string::npos);
    REQUIRE(generated.find("body: error_") != string::npos);
    REQUIRE(generated.find("pub fn fn_(") != string::npos);
    REQUIRE(generated.find("pub fn return_(") != string::npos);
    REQUIRE(generated.find("pub const TestHandler") != string::npos);
    REQUIRE(generated.find("pub const TTestClient") != string::npos);

    // Unescaped identifiers must not appear as Zig definitions.
    REQUIRE(generated.find("return: i32") == string::npos);
    REQUIRE(generated.find("init: i32") == string::npos);
    REQUIRE(generated.find("body: error,") == string::npos);
    REQUIRE(generated.find("pub const error = enum") == string::npos);
}