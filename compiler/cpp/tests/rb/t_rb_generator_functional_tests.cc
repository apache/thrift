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

TEST_CASE("t_rb_generator emits bare marker for CRLF blank RDoc lines", "[functional]")
{
    const string thrift_path = "test_rdoc_crlf.thrift";
    const string thrift_source =
        "/**\r\n"
        " * first line\r\n"
        " *\r\n"
        " * second line\r\n"
        " */\r\n"
        "struct Example {\r\n"
        "  1: string value\r\n"
        "}\r\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_rdoc_crlf"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "rb", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated_content = read_file("gen-rb/test_rdoc_crlf_types.rb");
    REQUIRE(!generated_content.empty());
    REQUIRE(generated_content.find("\r") == string::npos);
    REQUIRE(generated_content.rfind("# frozen_string_literal: true\n", 0) == 0);
    REQUIRE(generated_content.find("#  * first line\n") != string::npos);
    REQUIRE(generated_content.find("#  *\n") != string::npos);

    std::remove(thrift_path.c_str());
}
