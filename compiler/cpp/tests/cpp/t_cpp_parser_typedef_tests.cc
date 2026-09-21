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

#include <cstdio>
#include <fstream>
#include <memory>
#include <string>

using std::string;
using cpp_generator_test_utils::parse_thrift_for_test;

namespace {

void write_file(const string& path, const string& source)
{
    std::ofstream file(path, std::ios::binary);
    REQUIRE(file.is_open());
    file << source;
}

} // namespace

// THRIFT-6333: a typedef target that is never declared used to be reported
// by t_typedef::get_type() with printf and exit(1), from inside whichever
// generator asked first. It now throws the message, which parse() in
// main.cc raises through t_program::resolve_types() before any generator runs.
TEST_CASE("undefined typedef target throws when resolved", "[parser]")
{
    const string thrift_path = "test_undefined_typedef.thrift";
    write_file(thrift_path,
               "typedef Missing T\n"
               "struct S { 1: T t }\n");

    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_undefined_typedef"));
    parse_thrift_for_test(program.get());

    REQUIRE(program->get_typedefs().size() == 1);
    t_typedef* td = program->get_typedefs()[0];
    // The target is a forward placeholder; resolving through it throws.
    REQUIRE_THROWS_WITH(td->get_true_type(), "Type \"Missing\" not defined");
    REQUIRE_THROWS_WITH(program->resolve_types(), "Type \"Missing\" not defined");

    std::remove(thrift_path.c_str());
}

TEST_CASE("typedef declared after its use resolves", "[parser]")
{
    const string thrift_path = "test_forward_typedef.thrift";
    write_file(thrift_path,
               "struct S { 1: T t }\n"
               "typedef i32 T\n");

    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_forward_typedef"));
    parse_thrift_for_test(program.get());

    REQUIRE(program->get_structs().size() == 1);
    t_type* field_type = program->get_structs()[0]->get_members()[0]->get_type();
    REQUIRE(field_type->is_typedef());
    REQUIRE_NOTHROW(field_type->get_true_type());
    REQUIRE(field_type->get_true_type()->is_base_type());
    REQUIRE_NOTHROW(program->resolve_types());

    std::remove(thrift_path.c_str());
}

// A typedef that reaches itself, directly or through a container, would send
// get_true_type() round forever; resolve_types() reports it instead.
TEST_CASE("typedef that refers to itself throws when resolved", "[parser]")
{
    const char* sources[] = {
        "typedef T T\n",
        "typedef list<T> T\n",
        "typedef B T\ntypedef T B\n",
    };
    const string thrift_path = "test_cyclic_typedef.thrift";
    for (const char* source : sources) {
        INFO(source);
        write_file(thrift_path, source);

        std::unique_ptr<t_program> program(new t_program(thrift_path, "test_cyclic_typedef"));
        parse_thrift_for_test(program.get());

        REQUIRE_THROWS_WITH(program->resolve_types(), "Type \"T\" refers to itself");
    }

    std::remove(thrift_path.c_str());
}

TEST_CASE("struct that contains itself resolves", "[parser]")
{
    const string thrift_path = "test_recursive_struct.thrift";
    write_file(thrift_path,
               "typedef list<S> L\n"
               "struct S { 1: L children }\n");

    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_recursive_struct"));
    parse_thrift_for_test(program.get());

    REQUIRE_NOTHROW(program->resolve_types());

    std::remove(thrift_path.c_str());
}
