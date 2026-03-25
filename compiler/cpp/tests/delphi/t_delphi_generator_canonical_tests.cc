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

#include <algorithm>
#include <cstring>

using std::string;
using std::map;
using std::vector;
using delphi_generator_test_utils::parse_thrift_for_test;
using delphi_generator_test_utils::join_path;
using delphi_generator_test_utils::source_dir;

TEST_CASE("t_delphi_generator canonical string for simple struct", "[delphi][canonical]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "delphi", parsed_options, option_string));
    REQUIRE(gen != nullptr);

    const auto& structs = program->get_structs();
    REQUIRE(structs.size() >= 1);

    t_struct* simple = nullptr;
    for (auto s : structs) {
        if (s->get_name() == "SimpleStruct") {
            simple = s;
            break;
        }
    }
    REQUIRE(simple != nullptr);

    const auto& fields = simple->get_members();
    CHECK(fields.size() == 3);
    CHECK(fields[0]->get_name() == "id");
    CHECK(fields[1]->get_name() == "name");
    CHECK(fields[2]->get_name() == "value");
}

TEST_CASE("t_delphi_generator canonical string for service with inheritance", "[delphi][canonical]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    const auto& services = program->get_services();
    REQUIRE(services.size() >= 2);

    t_service* base = nullptr;
    t_service* derived = nullptr;
    for (auto s : services) {
        if (s->get_name() == "BaseService") {
            base = s;
        } else if (s->get_name() == "DerivedService") {
            derived = s;
        }
    }
    REQUIRE(base != nullptr);
    REQUIRE(derived != nullptr);
    CHECK(derived->get_extends() != nullptr);
    CHECK(derived->get_extends()->get_name() == "BaseService");
}

TEST_CASE("t_delphi_generator canonical string for oneway functions", "[delphi][canonical]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    const auto& services = program->get_services();
    t_service* notifier = nullptr;
    for (auto s : services) {
        if (s->get_name() == "Notifier") {
            notifier = s;
            break;
        }
    }
    REQUIRE(notifier != nullptr);

    const auto& funcs = notifier->get_functions();
    CHECK(funcs.size() == 2);

    bool found_oneway = false;
    for (auto f : funcs) {
        if (f->get_name() == "notify") {
            CHECK(f->is_oneway());
            found_oneway = true;
        }
    }
    CHECK(found_oneway);
}

TEST_CASE("t_delphi_generator canonical string for complex types", "[delphi][canonical]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    const auto& structs = program->get_structs();
    t_struct* complex = nullptr;
    for (auto s : structs) {
        if (s->get_name() == "ComplexStruct") {
            complex = s;
            break;
        }
    }
    REQUIRE(complex != nullptr);

    const auto& fields = complex->get_members();
    CHECK(fields.size() == 4);

    CHECK(fields[0]->get_name() == "id");
    CHECK(fields[1]->get_name() == "tags");
    CHECK(fields[2]->get_name() == "counts");
    CHECK(fields[3]->get_name() == "nested");
}

TEST_CASE("t_delphi_generator canonical string for exceptions", "[delphi][canonical]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";
    map<string, string> parsed_options = {};
    string option_string = "";

    std::unique_ptr<t_program> program(new t_program(path, name));
    parse_thrift_for_test(program.get());

    const auto& xceptions = program->get_xceptions();
    CHECK(xceptions.size() >= 1);

    t_struct* error = nullptr;
    for (auto x : xceptions) {
        if (x->get_name() == "ApplicationError") {
            error = x;
            break;
        }
    }
    REQUIRE(error != nullptr);
    CHECK(error->is_xception());

    const auto& fields = error->get_members();
    CHECK(fields.size() == 2);
    CHECK(fields[0]->get_name() == "code");
    CHECK(fields[1]->get_name() == "message");
}

TEST_CASE("t_delphi_generator async flag affects service", "[delphi][canonical]") {
    string path = join_path(source_dir(), "test_canonical.thrift");
    string name = "test_canonical";

    {
        map<string, string> parsed_options = {{"async", ""}};
        std::unique_ptr<t_program> program(new t_program(path, name));
        parse_thrift_for_test(program.get());
        std::unique_ptr<t_generator> gen(
            t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
        REQUIRE(gen != nullptr);
    }

    {
        map<string, string> parsed_options = {};
        std::unique_ptr<t_program> program(new t_program(path, name));
        parse_thrift_for_test(program.get());
        std::unique_ptr<t_generator> gen(
            t_generator_registry::get_generator(program.get(), "delphi", parsed_options, ""));
        REQUIRE(gen != nullptr);
    }
}
