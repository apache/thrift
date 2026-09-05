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
#include <fstream>
#include <memory>
#include <vector>

using cpp_generator_test_utils::parse_thrift_for_test;
using std::string;

TEST_CASE("parser accepts backslashes in string constants", "[parser]")
{
    const string thrift_path = "test_string_constant_backslash.thrift";
    const string thrift_source =
        "namespace php Example\n"
        "const string ISO8601U = \"Y-m-d\\TH:i:s.uP\"\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_string_constant_backslash"));
    parse_thrift_for_test(program.get());

    const std::vector<t_const*>& consts = program->get_consts();
    REQUIRE(consts.size() == 1);
    REQUIRE(consts.front()->get_value()->get_type() == t_const_value::CV_STRING);
    REQUIRE(consts.front()->get_value()->get_string() == "Y-m-d\\TH:i:s.uP");

    std::remove(thrift_path.c_str());
}
