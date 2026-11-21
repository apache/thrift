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

// THRIFT-6340: the backing property of a required field was declared under the
// raw field name and read through kotlin_safe_name, so a field named after a
// Kotlin keyword referenced _`name`.
TEST_CASE("t_kotlin_generator uses one backing name for a required field named after a keyword", "[functional]")
{
    const string thrift_path = "test_kotlin_keyword_field.thrift";
    const string thrift_source =
        "struct S { 1: required i32 val, 2: required string plain }\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_kotlin_keyword_field"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "kotlin", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("S.kt");
    REQUIRE(!generated.empty());
    REQUIRE(generated.find("private var _val: kotlin.Int? = null") != string::npos);
    REQUIRE(generated.find("val `val`: kotlin.Int get() = _val!!") != string::npos);
    REQUIRE(generated.find("_`val`") == string::npos);
    REQUIRE(generated.find("val plain: kotlin.String get() = _plain!!") != string::npos);
    std::remove("S.kt");

    std::remove(thrift_path.c_str());
}
