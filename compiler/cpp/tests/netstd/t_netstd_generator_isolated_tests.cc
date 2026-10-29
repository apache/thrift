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
#include <thrift/generate/t_netstd_generator.h>

// The isolated client generation is opt-in: without the option the generator must keep producing
// the shared-protocol clients, so the flag has to default to off.
TEST_CASE("t_netstd_generator keeps isolated client generation off by default", "[initialization]")
{
    t_program program("IsolatedTest.thrift", "netstd");
    map<string, string> parsed_options;
    string option_string = "";

    t_netstd_generator* gen = nullptr;
    REQUIRE_NOTHROW(gen = new t_netstd_generator(&program, parsed_options, option_string));
    REQUIRE_FALSE(gen->is_isolated_enabled());
    delete gen;
}

TEST_CASE("t_netstd_generator enables isolated client generation with the isolated option", "[initialization]")
{
    t_program program("IsolatedTest.thrift", "netstd");
    map<string, string> parsed_options = { { "isolated", "" }, { "net10", "" } };
    string option_string = "";

    t_netstd_generator* gen = nullptr;
    REQUIRE_NOTHROW(gen = new t_netstd_generator(&program, parsed_options, option_string));
    REQUIRE(gen->is_isolated_enabled());
    REQUIRE_FALSE(gen->is_wcf_enabled());
    delete gen;
}

TEST_CASE("t_netstd_generator still rejects unknown options next to isolated", "[initialization]")
{
    t_program program("IsolatedTest.thrift", "netstd");
    map<string, string> parsed_options = { { "isolated", "" }, { "bogus", "" } };
    string option_string = "";

    t_netstd_generator* gen = nullptr;
    REQUIRE_THROWS(gen = new t_netstd_generator(&program, parsed_options, option_string));
    delete gen;
}
