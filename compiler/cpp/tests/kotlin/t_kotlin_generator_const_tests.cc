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

// THRIFT-6339: generate_consts tested the declared type, so a constant of a
// typedef type matched neither the base-type nor the enum branch and was
// written without a value.
TEST_CASE("t_kotlin_generator renders a constant declared through a typedef", "[functional]")
{
    const string thrift_path = "test_kotlin_const.thrift";
    const string thrift_source =
        "typedef i32 MyInt\n"
        "const MyInt ANSWER = 42\n"
        "enum E { A = 1 }\n"
        "typedef E MyE\n"
        "const MyE FIRST = E.A\n"
        "const i32 PLAIN = 7\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_kotlin_const"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "kotlin", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("test_kotlin_constConstants.kt");
    REQUIRE(!generated.empty());
    REQUIRE(generated.find("val ANSWER: kotlin.Int = 42\n") != string::npos);
    REQUIRE(generated.find("val FIRST: E = E.A\n") != string::npos);
    REQUIRE(generated.find("val PLAIN: kotlin.Int = 7\n") != string::npos);
    std::remove("test_kotlin_constConstants.kt");
    std::remove("E.kt");

    std::remove(thrift_path.c_str());
}
