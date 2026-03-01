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

// Helper function to extract enum definition from generated header
// Handles struct Color { enum type { ... }; }; or enum Color { ... }; or enum class Color { ... };
string extract_enum_definition(const string& content, const string& enum_name) {
    // First try to find struct wrapper (default behavior)
    size_t struct_start = content.find("struct " + enum_name + " {");
    if (struct_start != string::npos) {
        // Find the closing brace and semicolon for the struct
        size_t brace_count = 0;
        size_t pos = content.find("{", struct_start);
        if (pos == string::npos) return "";
        
        brace_count = 1;
        pos++;
        
        while (pos < content.length() && brace_count > 0) {
            if (content[pos] == '{') brace_count++;
            else if (content[pos] == '}') brace_count--;
            pos++;
        }
        
        if (brace_count == 0) {
            // Skip whitespace and semicolon
            while (pos < content.length() && (content[pos] == ' ' || content[pos] == '\n' || content[pos] == '\r')) pos++;
            if (pos < content.length() && content[pos] == ';') pos++;
            return content.substr(struct_start, pos - struct_start);
        }
        return "";
    }
    
    // Try to find enum class
    size_t enum_start = content.find("enum class " + enum_name + " {");
    if (enum_start == string::npos) {
        // Try to find plain enum
        enum_start = content.find("enum " + enum_name + " {");
        if (enum_start == string::npos) {
            return "";
        }
    }
    
    // Find the closing brace and semicolon
    size_t brace_end = content.find("};", enum_start);
    if (brace_end == string::npos) {
        return "";
    }
    
    return content.substr(enum_start, brace_end - enum_start + 2);
}

TEST_CASE("t_cpp_generator default behavior generates wrapper struct for enums", "[functional]")
{
    string path = join_path(source_dir(), "test_enum_class.thrift");
    string name = "test_enum_class";
    map<string, string> parsed_options = {}; // No options
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_enum_class_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract enum definition
    string enum_def = extract_enum_definition(generated_content, "Color");
    REQUIRE(!enum_def.empty());

    // Compare generated enum definition against the expected fixture.
    string expected_path = join_path(source_dir(), "expected_Color_default.txt");
    string expected_content = read_file(expected_path);
    REQUIRE(!expected_content.empty());

    REQUIRE(normalize_for_compare(enum_def) == normalize_for_compare(expected_content));
}

TEST_CASE("t_cpp_generator with pure_enums generates plain enum", "[functional]")
{
    string path = join_path(source_dir(), "test_enum_class.thrift");
    string name = "test_enum_class";
    map<string, string> parsed_options = {{"pure_enums", ""}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_enum_class_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract enum definition
    string enum_def = extract_enum_definition(generated_content, "Color");
    REQUIRE(!enum_def.empty());

    // Compare generated enum definition against the expected fixture.
    string expected_path = join_path(source_dir(), "expected_Color_pure_enums.txt");
    string expected_content = read_file(expected_path);
    REQUIRE(!expected_content.empty());

    REQUIRE(normalize_for_compare(enum_def) == normalize_for_compare(expected_content));
}

TEST_CASE("t_cpp_generator with pure_enums=enum_class generates C++ 11 enum class", "[functional]")
{
    string path = join_path(source_dir(), "test_enum_class.thrift");
    string name = "test_enum_class";
    map<string, string> parsed_options = {{"pure_enums", "enum_class"}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_enum_class_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract enum definition
    string enum_def = extract_enum_definition(generated_content, "Color");
    REQUIRE(!enum_def.empty());

    // Compare generated enum definition against the expected fixture.
    string expected_path = join_path(source_dir(), "expected_Color_enum_class.txt");
    string expected_content = read_file(expected_path);
    REQUIRE(!expected_content.empty());

    REQUIRE(normalize_for_compare(enum_def) == normalize_for_compare(expected_content));
}
