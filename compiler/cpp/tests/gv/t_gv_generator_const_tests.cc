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

// THRIFT-6332: print_const_value cast the declared type of a constant to a
// container type by the shape of the value, so a struct literal, a typedef
// of a container, or a container nested in either read out of bounds and
// crashed on an enum identifier or a nested container inside.
TEST_CASE("t_gv_generator renders struct, typedef and nested container constants", "[functional]")
{
    const string thrift_path = "test_gv_const.thrift";
    const string thrift_source =
        "enum E { A = 1, B = 2 }\n"
        "typedef map<string, E> EnumMap\n"
        "typedef list<E> EnumList\n"
        "struct Inner { 1: list<i32> l }\n"
        "struct S { 1: E e, 2: map<string, i32> m, 3: Inner inner }\n"
        "const S STRUCT_CONST = { \"e\": E.A, \"m\": { \"k\": 1 }, \"inner\": { \"l\": [1, 2] } }\n"
        "const EnumMap MAP_CONST = { \"a\": E.B }\n"
        "const EnumList LIST_CONST = [ E.A, E.B ]\n"
        "const list<S> LIST_OF_STRUCTS = [ { \"e\": E.B } ]\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_gv_const"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "gv", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated = read_file("gen-gv/test_gv_const.gv");
    REQUIRE(!generated.empty());
    REQUIRE(generated.find("STRUCT_CONST = \\{ \\\"e\\\" = E.A, \\\"inner\\\" = \\{ \\\"l\\\" = \\{ 1, 2 \\} \\}, \\\"m\\\" = \\{ \\\"k\\\" = 1 \\} \\} :: S")
            != string::npos);
    REQUIRE(generated.find("MAP_CONST = \\{ \\\"a\\\" = E.B \\} :: EnumMap") != string::npos);
    REQUIRE(generated.find("LIST_CONST = \\{ E.A, E.B \\} :: EnumList") != string::npos);
    REQUIRE(generated.find("LIST_OF_STRUCTS = \\{ \\{ \\\"e\\\" = E.B \\} \\} :: list\\<S\\>") != string::npos);

    std::remove(thrift_path.c_str());
}
