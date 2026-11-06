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

namespace {

// Generates "exception E { 1: <type> message }" with the given generator options and
// returns what generate_program threw, or an empty string if it did not throw.
string generate_exception_with_message_field(const string& type,
                                             const map<string, string>& parsed_options = {}) {
    const string thrift_path = "test_java_message_field.thrift";
    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        thrift_file << "exception E { 1: " << type << " message }\n";
    }

    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_java_message_field"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "java", parsed_options, ""));
    REQUIRE(gen != nullptr);

    string error;
    try {
        gen->generate_program();
    } catch (const string& e) {
        error = e;
    }
    std::remove(thrift_path.c_str());
    return error;
}

void remove_generated_exception() {
    std::remove("gen-java/E.java");
    std::remove("gen-java");
}

} // namespace

// THRIFT-6522: a non-string field named "message" generates a getMessage() getter that
// cannot override Throwable.getMessage(), so the output never compiled.
TEST_CASE("t_java_generator rejects a non-string exception field named message", "[functional]")
{
    for (const char* type : {"i32", "list<string>", "binary"}) {
        INFO(type);
        const string error = generate_exception_with_message_field(type);
        REQUIRE(error.find("exception E") != string::npos);
        REQUIRE(error.find("field 'message'") != string::npos);
        remove_generated_exception();
    }
}

TEST_CASE("t_java_generator accepts a string or bool exception field named message", "[functional]")
{
    for (const char* type : {"string", "bool"}) {
        INFO(type);
        REQUIRE(generate_exception_with_message_field(type).empty());
        REQUIRE(!read_file("gen-java/E.java").empty());
        remove_generated_exception();
    }
}

// With option_type, the getter of an optional field returns Option<String> or
// Optional<String>, which cannot override Throwable.getMessage() either.
TEST_CASE("t_java_generator rejects an optional message field with option_type", "[functional]")
{
    for (const char* option_type : {"", "jdk8"}) {
        INFO(option_type);
        const map<string, string> options = {{"option_type", option_type}};
        const string error = generate_exception_with_message_field("optional string", options);
        REQUIRE(error.find("exception E") != string::npos);
        REQUIRE(error.find("optional field 'message'") != string::npos);
        remove_generated_exception();
    }
}

TEST_CASE("t_java_generator accepts a message field with option_type unless it is optional",
          "[functional]")
{
    const map<string, string> options = {{"option_type", "jdk8"}};
    for (const char* type : {"string", "required string", "optional bool"}) {
        INFO(type);
        REQUIRE(generate_exception_with_message_field(type, options).empty());
        REQUIRE(!read_file("gen-java/E.java").empty());
        remove_generated_exception();
    }
}
