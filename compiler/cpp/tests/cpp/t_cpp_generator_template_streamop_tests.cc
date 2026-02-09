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

using std::string;
using std::map;
using cpp_generator_test_utils::read_file;
using cpp_generator_test_utils::source_dir;
using cpp_generator_test_utils::join_path;
using cpp_generator_test_utils::normalize_for_compare;
using cpp_generator_test_utils::parse_thrift_for_test;

// Helper function declared in t_cpp_generator_private_optional_tests.cc
// Extracts the class definition from generated content for the given class name
extern string extract_class_definition(const string& content, const string& class_name);

TEST_CASE("t_cpp_generator without template_streamop generates standard operator<< and printTo", "[functional]")
{
    string path = join_path(source_dir(), "test_template_streamop.thrift");
    string name = "test_template_streamop";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_template_streamop_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Verify operator<< declaration uses std::ostream (not template)
    REQUIRE(generated_content.find("std::ostream& operator<<(std::ostream& out, const SimpleStruct& obj);") != string::npos);
    
    // Extract class definition to check printTo
    string class_def = extract_class_definition(generated_content, "SimpleStruct");
    REQUIRE(!class_def.empty());
    
    // Verify printTo method uses std::ostream (not template)
    REQUIRE(class_def.find("void printTo(std::ostream& out) const;") != string::npos);
    
    // Verify no template syntax for printTo or operator<<
    REQUIRE(class_def.find("template <typename OStream_>") == string::npos);
    
    // Read implementation file
    string impl_file = "gen-cpp/test_template_streamop_types.cpp";
    string impl_content = read_file(impl_file);
    REQUIRE(!impl_content.empty());
    
    // Verify implementation also uses std::ostream
    REQUIRE(impl_content.find("void SimpleStruct::printTo(std::ostream& out) const") != string::npos);
    REQUIRE(impl_content.find("std::ostream& operator<<(std::ostream& out, const SimpleStruct& obj)") != string::npos);
}

TEST_CASE("t_cpp_generator with template_streamop generates templated operator<< and printTo", "[functional]")
{
    string path = join_path(source_dir(), "test_template_streamop.thrift");
    string name = "test_template_streamop";
    map<string, string> parsed_options = {{"template_streamop", ""}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated header
    string generated_file = "gen-cpp/test_template_streamop_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Verify operator<< declaration uses template syntax
    REQUIRE(generated_content.find("template <typename OStream_>") != string::npos);
    REQUIRE(generated_content.find("OStream_& operator<<(OStream_& out, const SimpleStruct& obj);") != string::npos);
    
    // Extract class definition
    string class_def = extract_class_definition(generated_content, "SimpleStruct");
    REQUIRE(!class_def.empty());
    
    // Verify printTo method declaration uses template syntax
    REQUIRE(class_def.find("template <typename OStream_>") != string::npos);
    REQUIRE(class_def.find("void printTo(OStream_& out) const;") != string::npos);
    
    // Verify no hardcoded std::ostream in printTo declaration
    REQUIRE(class_def.find("void printTo(std::ostream& out) const;") == string::npos);
    
    // Read implementation file (.tcc for templates)
    string impl_file = "gen-cpp/test_template_streamop_types.tcc";
    string impl_content = read_file(impl_file);
    REQUIRE(!impl_content.empty());
    
    // Verify implementation uses template syntax
    REQUIRE(impl_content.find("template <typename OStream_>") != string::npos);
    REQUIRE(impl_content.find("void SimpleStruct::printTo(OStream_& out) const") != string::npos);
    REQUIRE(impl_content.find("OStream_& operator<<(OStream_& out, const SimpleStruct& obj)") != string::npos);
    
    // Verify both SimpleStruct and NestedStruct have template versions
    REQUIRE(impl_content.find("void NestedStruct::printTo(OStream_& out) const") != string::npos);
}

TEST_CASE("t_cpp_generator with template_streamop and private_optional generates correct friend declarations", "[functional]")
{
    string path = join_path(source_dir(), "test_template_streamop.thrift");
    string name = "test_template_streamop";
    map<string, string> parsed_options = {{"template_streamop", ""}, {"private_optional", ""}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated header
    string generated_file = "gen-cpp/test_template_streamop_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract class definition
    string class_def = extract_class_definition(generated_content, "SimpleStruct");
    REQUIRE(!class_def.empty());
    
    // Verify friend declaration comes after template keyword (correct C++ syntax)
    // Should be: template <typename OStream_>
    //            friend OStream_& operator<<(...)
    // NOT: friend template <typename OStream_> ...
    REQUIRE(class_def.find("template <typename OStream_>") != string::npos);
    REQUIRE(class_def.find("friend OStream_& operator<<(OStream_& out, const SimpleStruct& obj);") != string::npos);
    
    // Verify incorrect syntax is not present
    REQUIRE(class_def.find("friend template <typename OStream_>") == string::npos);
}

// Note: Enum printTo specialization functionality is tested in the runtime test (TemplateStreamOpTest)
// since the compiler test infrastructure doesn't support this testing pattern.
