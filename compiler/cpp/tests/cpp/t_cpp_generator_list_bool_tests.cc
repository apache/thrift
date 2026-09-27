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

#include "t_cpp_generator_test_utils.h"

using std::string;
using std::map;
using cpp_generator_test_utils::read_file;
using cpp_generator_test_utils::source_dir;
using cpp_generator_test_utils::join_path;
using cpp_generator_test_utils::parse_thrift_for_test;

// std::vector<bool> has emplace_back() only from C++14 on, and the generated code has to build as
// C++11, so a list of bool (directly or through a typedef) must be grown with push_back().
TEST_CASE("t_cpp_generator grows a list<bool> with push_back, not emplace_back", "[functional]")
{
    string path = join_path(source_dir(), "test_list_bool.thrift");
    string name = "test_list_bool";
    map<string, string> parsed_options;
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    string generated = read_file("gen-cpp/test_list_bool_types.cpp");
    REQUIRE(!generated.empty());

    REQUIRE(generated.find("this->flags.push_back(false);") != string::npos);
    REQUIRE(generated.find("this->typedef_flags.push_back(false);") != string::npos);
    REQUIRE(generated.find("this->flags.emplace_back();") == string::npos);
    REQUIRE(generated.find("this->typedef_flags.emplace_back();") == string::npos);

    // The outer list holds vectors, so it keeps emplace_back(); its elements are lists of bool.
    REQUIRE(generated.find("this->nested_flags.emplace_back();") != string::npos);
    REQUIRE(generated.find("this->nested_flags.back().push_back(false);") != string::npos);
    REQUIRE(generated.find("this->nested_flags.back().emplace_back();") == string::npos);

    // Every other element type is unchanged.
    REQUIRE(generated.find("this->numbers.emplace_back();") != string::npos);
}
