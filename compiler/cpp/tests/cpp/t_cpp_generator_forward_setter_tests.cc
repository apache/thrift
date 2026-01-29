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
using cpp_generator_test_utils::normalize_for_compare;
using cpp_generator_test_utils::parse_thrift_for_test;

// Helper function declared in t_cpp_generator_private_optional_tests.cc
extern string extract_class_definition(const string& content, const string& class_name);

TEST_CASE("t_cpp_generator with moveable_types only generates move semantics", "[functional]")
{
    string path = join_path(source_dir(), "test_forward_setter.thrift");
    string name = "test_forward_setter";
    map<string, string> parsed_options = {{"moveable_types", ""}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_forward_setter_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract class definition
    string class_def = extract_class_definition(generated_content, "TestForwardSetter");
    REQUIRE(!class_def.empty());

    // Verify move constructor and move assignment are present
    REQUIRE(class_def.find("TestForwardSetter(TestForwardSetter&&)") != string::npos);
    REQUIRE(class_def.find("TestForwardSetter& operator=(TestForwardSetter&&)") != string::npos);
    
    // Verify setters are NOT templated (traditional setters)
    REQUIRE(class_def.find("void __set_complex_string(const std::string& val);") != string::npos);
    // Should not have template setters
    bool hasTemplateSetters = (class_def.find("template <typename T_>") != string::npos);
    REQUIRE(!hasTemplateSetters);
}

TEST_CASE("t_cpp_generator with moveable_types=forward_setter generates forwarding setters", "[functional]")
{
    string path = join_path(source_dir(), "test_forward_setter.thrift");
    string name = "test_forward_setter";
    map<string, string> parsed_options = {{"moveable_types", "forward_setter"}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated header
    string generated_file = "gen-cpp/test_forward_setter_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract class definition
    string class_def = extract_class_definition(generated_content, "TestForwardSetter");
    REQUIRE(!class_def.empty());

    // Verify move constructor and move assignment are present
    REQUIRE(class_def.find("TestForwardSetter(TestForwardSetter&&)") != string::npos);
    REQUIRE(class_def.find("TestForwardSetter& operator=(TestForwardSetter&&)") != string::npos);
    
    // Verify setters for complex types use templates with perfect forwarding (declarations in header)
    REQUIRE(class_def.find("template <typename T_>") != string::npos);
    REQUIRE(class_def.find("void __set_complex_string(T_&& val);") != string::npos);
    
    // Verify primitive setters are NOT templated
    REQUIRE(class_def.find("void __set_primitive_field(const int32_t val);") != string::npos);
    REQUIRE(class_def.find("void __set_primitive_bool(const bool val);") != string::npos);
    
    // Verify header includes .tcc file
    REQUIRE(generated_content.find("#include \"test_forward_setter_types.tcc\"") != string::npos);
    
    // Read generated .tcc file and verify implementations
    string tcc_file = "gen-cpp/test_forward_setter_types.tcc";
    string tcc_content = read_file(tcc_file);
    REQUIRE(!tcc_content.empty());
    
    // Verify template implementations are in .tcc file
    REQUIRE(tcc_content.find("::std::forward<T_>(val)") != string::npos);
    REQUIRE(tcc_content.find("void TestForwardSetter::__set_complex_string(T_&& val)") != string::npos);
    REQUIRE(tcc_content.find("void TestForwardSetter::__set_complex_struct(T_&& val)") != string::npos);
}
