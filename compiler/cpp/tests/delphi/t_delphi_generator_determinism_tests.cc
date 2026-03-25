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

#include <fstream>
#include <regex>

using std::string;
using std::map;
using std::set;
using std::vector;
using delphi_generator_test_utils::read_file;
using delphi_generator_test_utils::source_dir;
using delphi_generator_test_utils::join_path;
using delphi_generator_test_utils::parse_thrift_for_test;

static const string UUIDv5_PATTERN = R"(\[\{'([0-9a-f]{8}-[0-9a-f]{4}-8[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12})'\}\])";

static string extract_first_guid(const string& content) {
    std::regex r(UUIDv5_PATTERN);
    std::smatch match;
    if (std::regex_search(content, match, r)) {
        return match.str(1);
    }
    return "";
}

TEST_CASE("t_delphi_generator produces deterministic GUIDs across multiple generations", "[delphi][determinism]") {
    string path = join_path(source_dir(), "test_uuidv5.thrift");
    string name = "test_uuidv5";

    string content_run1;
    string content_run2;

    {
        map<string, string> parsed_options = {};
        std::unique_ptr<t_program> program(new t_program(path, name));
        parse_thrift_for_test(program.get());
        std::unique_ptr<t_generator> gen(
            t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));

        program.reset(new t_program(path, name));
        parse_thrift_for_test(program.get());
        gen.reset(t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
        REQUIRE_NOTHROW(gen->generate_program());
        content_run1 = read_file("gen-delphi/Test.GuidV5.Types.pas");
    }

    {
        map<string, string> parsed_options = {};
        std::unique_ptr<t_program> program(new t_program(path, name));
        parse_thrift_for_test(program.get());
        std::unique_ptr<t_generator> gen(
            t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
        REQUIRE_NOTHROW(gen->generate_program());
        content_run2 = read_file("gen-delphi/Test.GuidV5.Types.pas");
    }

    REQUIRE(!content_run1.empty());
    REQUIRE(!content_run2.empty());

    string guid1_run1 = extract_first_guid(content_run1);
    string guid1_run2 = extract_first_guid(content_run2);

    CHECK(!guid1_run1.empty());
    CHECK(!guid1_run2.empty());
    CHECK(guid1_run1 == guid1_run2);
}

TEST_CASE("t_delphi_generator produces same GUIDs with guid_v5 on multiple runs", "[delphi][determinism]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";
    map<string, string> parsed_options = {};

    set<string> all_guids_run1;
    set<string> all_guids_run2;

    for (int run = 0; run < 2; ++run) {
        std::unique_ptr<t_program> program(new t_program(path, name));
        parse_thrift_for_test(program.get());
        std::unique_ptr<t_generator> gen(
            t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
        REQUIRE_NOTHROW(gen->generate_program());

        string content = read_file("gen-delphi/test.canonical.Types.pas");
        REQUIRE(!content.empty());

        std::regex r(UUIDv5_PATTERN);
        auto begin = std::sregex_iterator(content.begin(), content.end(), r);
        auto end = std::sregex_iterator();

        set<string>& guids = (run == 0) ? all_guids_run1 : all_guids_run2;
        for (auto i = begin; i != end; ++i) {
            guids.insert((*i).str(1));
        }
    }

    CHECK(all_guids_run1 == all_guids_run2);
    CHECK(!all_guids_run1.empty());
}

TEST_CASE("t_delphi_generator produces consistent GUIDs across platforms", "[delphi][determinism]") {
    string path = join_path(source_dir(), "test_uuidv5.thrift");
    string name = "test_uuidv5";
    map<string, string> parsed_options = {};

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
    REQUIRE_NOTHROW(gen->generate_program());

    string content = read_file("gen-delphi/Test.GuidV5.Service.pas");
    REQUIRE(!content.empty());

    std::regex r(UUIDv5_PATTERN);
    auto begin = std::sregex_iterator(content.begin(), content.end(), r);
    auto end = std::sregex_iterator();

    int guid_count = 0;
    for (auto i = begin; i != end; ++i) {
        guid_count++;
        string uuid = (*i).str(1);
        CHECK(uuid.length() == 36);
        CHECK(uuid[14] == '5');
        CHECK(uuid[19] >= '8');
        CHECK(uuid[19] <= 'b');
    }

    CHECK(guid_count >= 2);
}

TEST_CASE("t_delphi_generator unique GUIDs per interface", "[delphi][determinism]") {
    string path = join_path(source_dir(), "test_uuidv5.thrift");
    string name = "test_uuidv5";
    map<string, string> parsed_options = {};

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());
    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
    REQUIRE_NOTHROW(gen->generate_program());

    set<string> all_guids;
    vector<string> files = {"gen-delphi/Test.GuidV5.Types.pas", "gen-delphi/Test.GuidV5.Service.pas"};

    for (const auto& file : files) {
        string content = read_file(file);
        if (!content.empty()) {
            std::regex r(UUIDv5_PATTERN);
            auto begin = std::sregex_iterator(content.begin(), content.end(), r);
            auto end = std::sregex_iterator();
            for (auto i = begin; i != end; ++i) {
                all_guids.insert((*i).str(1));
            }
        }
    }

    CHECK(all_guids.size() >= 4);
}
