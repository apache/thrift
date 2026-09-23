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

// THRIFT-6342: render_const_value wrote (byte)N and (short)N, which is not Haxe.
TEST_CASE("t_haxe_generator renders i8 and i16 constants as plain integers", "[functional]")
{
    const string thrift_path = "test_haxe_const.thrift";
    const string thrift_source =
        "const i8 SMALL = 7\n"
        "const i16 MEDIUM = 300\n"
        "const i32 LARGE = 70000\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_haxe_const"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "haxe", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("gen-haxe/Test_haxe_constConstants.hx");
    REQUIRE(!generated.empty());
    REQUIRE(generated.find("SMALL : haxe.Int32 = 7;") != string::npos);
    REQUIRE(generated.find("MEDIUM : haxe.Int32 = 300;") != string::npos);
    REQUIRE(generated.find("LARGE : haxe.Int32 = 70000;") != string::npos);
    REQUIRE(generated.find("(byte)") == string::npos);
    REQUIRE(generated.find("(short)") == string::npos);

    std::remove(thrift_path.c_str());
}
