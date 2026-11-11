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

#include <memory>
#include <string>

using std::string;

// The options are passed the way "--gen netstd:<option>" passes them.
static t_generator* make_netstd_generator(t_program* program, const string& option) {
    return t_generator_registry::get_generator(program, "netstd:" + option);
}

// THRIFT-6383: .NET 8 and .NET 9 reached end of support, and the netstd generator dropped the
// options that targeted them
TEST_CASE("t_netstd_generator rejects the net8 and net9 options", "[initialization]")
{
    std::unique_ptr<t_program> program(new t_program("test_netstd_options.thrift", "test_netstd_options"));

    for (const string option : { "net8", "net9" }) {
        std::unique_ptr<t_generator> gen;
        REQUIRE_THROWS_WITH(gen.reset(make_netstd_generator(program.get(), option)),
                            "unknown option netstd:" + option);
    }
}

TEST_CASE("t_netstd_generator accepts the net10 and net11 options", "[initialization]")
{
    std::unique_ptr<t_program> program(new t_program("test_netstd_options.thrift", "test_netstd_options"));

    for (const string option : { "net10", "net11" }) {
        std::unique_ptr<t_generator> gen;
        REQUIRE_NOTHROW(gen.reset(make_netstd_generator(program.get(), option)));
        REQUIRE(gen != nullptr);
    }
}
