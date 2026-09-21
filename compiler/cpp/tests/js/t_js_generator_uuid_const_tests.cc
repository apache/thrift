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

// THRIFT-6336: render_const_value streamed the t_const_value pointer for a
// uuid constant, so the generated value was an address.
TEST_CASE("t_js_generator renders a uuid constant as its string value", "[functional]")
{
    const string thrift_path = "test_js_uuid_const.thrift";
    const string thrift_source =
        "const uuid GEN_UUID = \"00000000-1111-2222-3333-444444444444\"\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_js_uuid_const"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "js", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("gen-js/test_js_uuid_const_types.js");
    REQUIRE(!generated.empty());
    REQUIRE(generated.find("GEN_UUID = '00000000-1111-2222-3333-444444444444';") != string::npos);
    REQUIRE(generated.find("'0x") == string::npos);

    std::remove(thrift_path.c_str());
}
