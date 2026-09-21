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

#include "../cpp/t_cpp_generator_test_utils.h"

#include <cstdio>
#include <fstream>
#include <memory>

using std::map;
using std::string;
using cpp_generator_test_utils::parse_thrift_for_test;
using cpp_generator_test_utils::read_file;

// THRIFT-6344: the reserved word list had "macro " with a trailing space, so a
// field named macro was written unescaped, unlike the other reserved words.
TEST_CASE("t_d_generator escapes a field named macro", "[functional]")
{
    const string thrift_path = "test_d_reserved_word.thrift";
    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << "struct S { 1: i32 macro, 2: i32 mixin }\n";
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_d_reserved_word"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "d", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("gen-d/test_d_reserved_word_types.d");
    REQUIRE(!generated.empty());
    REQUIRE(generated.find("int mixin_;") != string::npos);
    REQUIRE(generated.find("int macro_;") != string::npos);
    REQUIRE(generated.find("int macro;") == string::npos);

    std::remove(thrift_path.c_str());
}
