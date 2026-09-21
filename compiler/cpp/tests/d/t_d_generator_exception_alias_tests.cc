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

// THRIFT-6341: the exception types were collected in a std::set<t_type*>, so
// the alias lines came out in pointer order and differed between runs.
TEST_CASE("t_d_generator aliases a service's exceptions in the order they are first thrown", "[functional]")
{
    const string thrift_path = "test_d_exception_alias.thrift";
    const string thrift_source =
        "exception E1 { 1: string msg }\n"
        "exception E2 { 1: string msg }\n"
        "exception E3 { 1: string msg }\n"
        "service Svc {\n"
        "  void a() throws (1: E3 e),\n"
        "  void b() throws (1: E1 e, 2: E2 f),\n"
        "  void c() throws (1: E2 e, 2: E3 f)\n"
        "}\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_d_exception_alias"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "d", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("gen-d/Svc.d");
    REQUIRE(!generated.empty());
    const string::size_type e3 = generated.find("alias test_d_exception_alias_types.E3 E3;");
    const string::size_type e1 = generated.find("alias test_d_exception_alias_types.E1 E1;");
    const string::size_type e2 = generated.find("alias test_d_exception_alias_types.E2 E2;");
    REQUIRE(e3 != string::npos);
    REQUIRE(e1 != string::npos);
    REQUIRE(e2 != string::npos);
    REQUIRE(e3 < e1);
    REQUIRE(e1 < e2);
    REQUIRE(generated.find("alias test_d_exception_alias_types.E3 E3;", e3 + 1) == string::npos);

    std::remove(thrift_path.c_str());
}
