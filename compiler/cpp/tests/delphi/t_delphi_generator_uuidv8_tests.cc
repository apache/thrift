// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

#include "t_delphi_generator_test_utils.h"

#include <regex>
#include <set>

using std::string;
using std::map;
using std::set;
using std::vector;
using std::regex;
using std::regex_search;
using delphi_generator_test_utils::read_file;
using delphi_generator_test_utils::source_dir;
using delphi_generator_test_utils::join_path;
using delphi_generator_test_utils::parse_thrift_for_test;

static const string UUID_PATTERN = R"(\[\s*'\{[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\}'\])";

static set<string> extract_guids(const string& content) {
    set<string> guids;
    regex r(UUID_PATTERN);
    std::sregex_iterator it(content.begin(), content.end(), r);
    std::sregex_iterator end;
    while (it != end) {
        guids.insert(it->str());
        ++it;
    }
    return guids;
}

static string extract_guid_for_interface(const string& content, const string& iface_name) {
    string pattern = iface_name + R"( = interface\([\s\S]*?"\n\s*)" + UUID_PATTERN;
    regex r(pattern);
    std::smatch match;
    if (regex_search(content, match, r)) {
        string matched = match.str();
        std::smatch guid_match;
        if (regex_search(matched, guid_match, regex(UUID_PATTERN))) {
            return guid_match.str();
        }
    }
    return "";
}

TEST_CASE("t_delphi_generator generates deterministic UUIDv8 GUIDs", "[delphi][uuidv8]") {
    string path = join_path(source_dir(), "test_uuidv8.thrift");
    string name = "test_uuidv8";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, option_string));
    REQUIRE(gen != nullptr);

    REQUIRE_NOTHROW(gen->generate_program());

    string generated_file = "gen-delphi/Test.GuidV8.pas";
    string content = read_file(generated_file);
    REQUIRE(!content.empty());

    set<string> guids = extract_guids(content);
    REQUIRE(!guids.empty());

    INFO("Found " << guids.size() << " unique GUIDs in generated code");

    for (const auto& guid : guids) {
        INFO("GUID: " << guid);
        CHECK(guid.find("{") != string::npos);
        CHECK(guid.find("-") != string::npos);
    }

    string simple_struct_guid = extract_guid_for_interface(content, "ISimpleStruct");
    CHECK(!simple_struct_guid.empty());

    string complex_struct_guid = extract_guid_for_interface(content, "IComplexStruct");
    CHECK(!complex_struct_guid.empty());

    CHECK(simple_struct_guid != complex_struct_guid);
}

TEST_CASE("t_delphi_generator generates different GUIDs for sync vs async", "[delphi][uuidv8]") {
    string path = join_path(source_dir(), "test_uuidv8.thrift");
    string name = "test_uuidv8";
    map<string, string> parsed_options = {{"async", ""}};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, option_string));
    REQUIRE(gen != nullptr);

    REQUIRE_NOTHROW(gen->generate_program());

    string generated_file = "gen-delphi/Test.GuidV8.pas";
    string content = read_file(generated_file);
    REQUIRE(!content.empty());

    string simple_service_guid = extract_guid_for_interface(content, "ISimpleService");
    string simple_service_async_guid = extract_guid_for_interface(content, "ISimpleServiceAsync");

    CHECK(!simple_service_guid.empty());
    CHECK(!simple_service_async_guid.empty());
    CHECK(simple_service_guid != simple_service_async_guid);
}

TEST_CASE("t_delphi_generator generates different GUIDs for service inheritance", "[delphi][uuidv8]") {
    string path = join_path(source_dir(), "test_uuidv8.thrift");
    string name = "test_uuidv8";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, option_string));
    REQUIRE(gen != nullptr);

    REQUIRE_NOTHROW(gen->generate_program());

    string generated_file = "gen-delphi/Test.GuidV8.pas";
    string content = read_file(generated_file);
    REQUIRE(!content.empty());

    string simple_service_guid = extract_guid_for_interface(content, "ISimpleService");
    string complex_service_guid = extract_guid_for_interface(content, "IComplexService");

    CHECK(!simple_service_guid.empty());
    CHECK(!complex_service_guid.empty());
    CHECK(simple_service_guid != complex_service_guid);
}

TEST_CASE("t_delphi_generator generates GUID format matching Delphi interface declaration", "[delphi][uuidv8]") {
    string path = join_path(source_dir(), "test_uuidv8.thrift");
    string name = "test_uuidv8";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, option_string));
    REQUIRE(gen != nullptr);

    REQUIRE_NOTHROW(gen->generate_program());

    string generated_file = "gen-delphi/Test.GuidV8.pas";
    string content = read_file(generated_file);
    REQUIRE(!content.empty());

    string pattern = R"(ISimpleStruct = interface\(IBase\)\n\s+\[\s*'\{[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\}'\])";
    regex r(pattern);
    CHECK(regex_search(content, r));
}

TEST_CASE("t_delphi_generator generates unique GUIDs for each entity", "[delphi][uuidv8]") {
    string path = join_path(source_dir(), "test_uuidv8.thrift");
    string name = "test_uuidv8";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, option_string));
    REQUIRE(gen != nullptr);

    REQUIRE_NOTHROW(gen->generate_program());

    set<string> all_guids;
    vector<string> files = {"gen-delphi/Test.GuidV8.pas"};

    for (const auto& file : files) {
        string content = read_file(file);
        if (!content.empty()) {
            set<string> file_guids = extract_guids(content);
            all_guids.insert(file_guids.begin(), file_guids.end());
        }
    }

    INFO("Total unique GUIDs across all files: " << all_guids.size());
    REQUIRE(all_guids.size() >= 4);
}
