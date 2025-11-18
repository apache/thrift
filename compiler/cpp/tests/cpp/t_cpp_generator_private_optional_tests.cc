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

#include "../catch/catch.hpp"
#include <thrift/parse/t_program.h>
#include <thrift/generate/t_generator.h>
#include <thrift/generate/t_generator_registry.h>
#include <thrift/globals.h>
#include <thrift/main.h>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>

#include <algorithm>

using std::string;
using std::map;
using std::ifstream;
using std::ofstream;
using std::ostringstream;

// Provided by compiler/cpp/tests/thrift_test_parser_support.cc
extern std::string g_curdir;
extern std::string g_curpath;

// Helper function to read file content
string read_file(const string& filename) {
    ifstream file(filename);
    if (!file.is_open()) {
        return "";
    }
    ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

static string source_dir() {
    string file = __FILE__;
    std::replace(file.begin(), file.end(), '\\', '/');
    size_t slash = file.rfind('/');
    return (slash == string::npos) ? string(".") : file.substr(0, slash);
}

static string join_path(const string& a, const string& b) {
    if (a.empty()) {
        return b;
    }
    if (a.back() == '/' || a.back() == '\\') {
        return a + b;
    }
    return a + "/" + b;
}

static string normalize_for_compare(string s) {
    // Normalize line endings to '\n', trim trailing whitespace on each line,
    // and drop empty/whitespace-only lines.
    s.erase(std::remove(s.begin(), s.end(), '\r'), s.end());

    std::istringstream in(s);
    std::ostringstream out;
    string line;
    bool first = true;
    while (std::getline(in, line)) {
        while (!line.empty() && (line.back() == ' ' || line.back() == '\t')) {
            line.pop_back();
        }

        const bool is_blank = line.find_first_not_of(" \t") == string::npos;
        if (is_blank) {
            continue;
        }

        if (!first) {
            out << '\n';
        }
        first = false;
        out << line;
    }

    return out.str();
}

// Helper function to extract class definition from generated header
string extract_class_definition(const string& content, const string& class_name) {
    size_t class_start = content.find("class " + class_name + " :");
    if (class_start == string::npos) {
        return "";
    }
    
    size_t class_end = content.find("};", class_start);
    if (class_end == string::npos) {
        return "";
    }
    
    return content.substr(class_start, class_end - class_start + 2);
}

static void parse_thrift_for_test(t_program* program) {
    REQUIRE(program != nullptr);

    // These globals are used by the parser; see thrift/globals.h.
    g_program = program;
    g_scope = program->scope();
    g_parent_scope = nullptr;
    g_parent_prefix = program->get_name() + ".";

    g_curpath = program->get_path();
    g_curdir = directory_name(g_curpath);

    // Pass 1: scan includes (even if none) to match the compiler behavior.
    g_parse_mode = INCLUDES;
    yylineno = 1;
    yyin = std::fopen(g_curpath.c_str(), "r");
    REQUIRE(yyin != nullptr);
    REQUIRE(yyparse() == 0);
    std::fclose(yyin);
    yyin = nullptr;

    // Pass 2: parse program.
    g_parse_mode = PROGRAM;
    yylineno = 1;
    yyin = std::fopen(g_curpath.c_str(), "r");
    REQUIRE(yyin != nullptr);
    REQUIRE(yyparse() == 0);
    std::fclose(yyin);
    yyin = nullptr;
}

TEST_CASE("t_cpp_generator default behavior generates all public fields", "[functional]")
{
    string path = join_path(source_dir(), "test_private_optional.thrift");
    string name = "test_private_optional";
    map<string, string> parsed_options = {}; // No private_optional flag
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_private_optional_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Compare generated class definition against the expected fixture.
    string class_def = extract_class_definition(generated_content, "TestStruct");
    REQUIRE(!class_def.empty());

    string expected_path = join_path(source_dir(), "expected_TestStruct_default.txt");
    string expected_content = read_file(expected_path);
    REQUIRE(!expected_content.empty());

    REQUIRE(normalize_for_compare(class_def) == normalize_for_compare(expected_content));
    
}

TEST_CASE("t_cpp_generator with private_optional generates private optional fields", "[functional]")
{
    string path = join_path(source_dir(), "test_private_optional.thrift");
    string name = "test_private_optional";
    map<string, string> parsed_options = {{"private_optional", ""}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "cpp", parsed_options, option_string));
    REQUIRE(gen != nullptr);
    
    // Generate code
    REQUIRE_NOTHROW(gen->generate_program());

    // Read generated output
    string generated_file = "gen-cpp/test_private_optional_types.h";
    string generated_content = read_file(generated_file);
    REQUIRE(!generated_content.empty());

    // Extract class definition
    string class_def = extract_class_definition(generated_content, "TestStruct");
    REQUIRE(!class_def.empty());

    // Compare generated class definition against the expected fixture.
    string expected_path = join_path(source_dir(), "expected_TestStruct_private_optional.txt");
    string expected_content = read_file(expected_path);
    REQUIRE(!expected_content.empty());

    REQUIRE(normalize_for_compare(class_def) == normalize_for_compare(expected_content));
    
}
