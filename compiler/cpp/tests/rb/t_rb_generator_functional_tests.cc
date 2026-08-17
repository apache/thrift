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

TEST_CASE("t_rb_generator emits bare marker for CRLF blank RDoc lines", "[functional]")
{
    const string thrift_path = "test_rdoc_crlf.thrift";
    const string thrift_source =
        "/**\r\n"
        " * first line\r\n"
        " *\r\n"
        " * second line\r\n"
        " */\r\n"
        "struct Example {\r\n"
        "  1: string value\r\n"
        "}\r\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_rdoc_crlf"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "rb", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated_content = read_file("gen-rb/test_rdoc_crlf_types.rb");
    REQUIRE(!generated_content.empty());
    REQUIRE(generated_content.find("\r") == string::npos);
    REQUIRE(generated_content.rfind("# frozen_string_literal: true\n", 0) == 0);
    REQUIRE(generated_content.find("#  * first line\n") != string::npos);
    REQUIRE(generated_content.find("#  *\n") != string::npos);

    std::remove(thrift_path.c_str());
}

TEST_CASE("t_rb_generator uses suffixed field id constants to avoid FIELDS collisions", "[functional]")
{
    const string thrift_path = "test_field_id_conflict.thrift";
    const string thrift_source =
        "struct Example {\n"
        "  1: string fields\n"
        "}\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_field_id_conflict"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "rb", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string generated_content = read_file("gen-rb/test_field_id_conflict_types.rb");
    REQUIRE(!generated_content.empty());
    REQUIRE(generated_content.find("FIELDS_FIELD_ID = 1") != string::npos);
    REQUIRE(generated_content.find("FIELDS_FIELD_ID => {type: ::Thrift::Types::STRING, name: \"fields\"},")
            != string::npos);
    REQUIRE(generated_content.find("FIELDS = 1") == string::npos);
    REQUIRE(generated_content.find("FIELDS => {type: ::Thrift::Types::STRING, name: \"fields\"},")
            == string::npos);

    std::remove(thrift_path.c_str());
}

TEST_CASE("t_rb_generator formats service classes and positional arguments", "[functional]")
{
    const string thrift_path = "test_service_arguments.thrift";
    const string thrift_source =
        "service EmptyService {\n"
        "}\n"
        "service PingService {\n"
        "  oneway void ping(1: i32 n)\n"
        "  i32 pong()\n"
        "}\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_service_arguments"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "rb", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string service = read_file("gen-rb/ping_service.rb");
    const string expected_empty_result =
        "  class Ping_result\n"
        "    include ::Thrift::Struct, ::Thrift::Struct_Union\n"
        "\n"
        "    FIELDS";
    REQUIRE(service.find("send_oneway_message(\"ping\", Ping_args, {n: n})") != string::npos);
    REQUIRE(service.find("def pong()\n      send_pong()\n      recv_pong()\n    end")
            != string::npos);
    REQUIRE(service.find(expected_empty_result) != string::npos);
    REQUIRE(service.find("\n\n  end\n") == string::npos);

    const string empty_service = read_file("gen-rb/empty_service.rb");
    REQUIRE(!empty_service.empty());
    REQUIRE(empty_service.find("\n\n  end\n") == string::npos);

    std::remove(thrift_path.c_str());
}

TEST_CASE("t_rb_generator formats multiline Ruby literals, calls, and field metadata", "[functional]")
{
    const string thrift_path = "test_multiline_layout.thrift";
    const string thrift_source =
        "struct Item {\n"
        "  1: string value\n"
        "}\n"
        "union Choice {\n"
        "  1: string value\n"
        "}\n"
        "const Item ITEM = {\"value\": \"one\"}\n"
        "const set<i32> IDS = [1, 2]\n"
        "struct Defaults {\n"
        "  1: list<i32> values = [1, 2]\n"
        "}\n";

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_multiline_layout"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "rb", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    const string constants = read_file("gen-rb/test_multiline_layout_constants.rb");
    const string expected_item_constant =
        "ITEM = ::Item.new({\n"
        "  %q\"value\" => %q\"one\",\n"
        "})";
    const string expected_set_constant =
        "IDS = Set.new([\n"
        "  1,\n"
        "  2,\n"
        "])";
    REQUIRE(constants.find(expected_item_constant) != string::npos);
    REQUIRE(constants.find(expected_set_constant) != string::npos);

    const string types = read_file("gen-rb/test_multiline_layout_types.rb");
    const string expected_struct =
        "class Item\n"
        "  include ::Thrift::Struct, ::Thrift::Struct_Union\n"
        "\n";
    const string expected_union =
        "class Choice < ::Thrift::Union\n"
        "  include ::Thrift::Struct_Union\n"
        "\n";
    const string expected_field_metadata =
        "    VALUES_FIELD_ID => {\n"
        "      type: ::Thrift::Types::LIST,\n";
    const string expected_default =
        "      default: [\n"
        "        1,\n"
        "        2,\n"
        "      ],\n";
    REQUIRE(types.find(expected_struct) != string::npos);
    REQUIRE(types.find(expected_union) != string::npos);
    REQUIRE(types.find(expected_field_metadata) != string::npos);
    REQUIRE(types.find(expected_default) != string::npos);

    std::remove(thrift_path.c_str());
}
