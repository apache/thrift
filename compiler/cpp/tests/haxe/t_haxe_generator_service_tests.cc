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

// THRIFT-6529: the argument and result structs of a method are classes of the package. They were named after the
// method alone, so two services of one package with a method of the same name wrote the same classes.
TEST_CASE("t_haxe_generator names argument and result structs after service and method", "[functional]")
{
    const string thrift_path = "test_haxe_service_helpers.thrift";
    const string thrift_source =
        "service A {\n"
        "  i32 get(1: i32 a)\n"
        "}\n"
        "\n"
        "service B {\n"
        "  string get()\n"
        "}\n"
        "\n"
        "service C extends A {\n"
        "  void put(1: i32 c)\n"
        "}\n";

    // files of an earlier run must not count
    const string checked[] = {
        "A_get_args.hx", "A_get_result.hx", "AImpl.hx", "AProcessor.hx",
        "B_get_args.hx", "B_get_result.hx", "BImpl.hx", "BProcessor.hx",
        "C_get_args.hx", "C_put_args.hx", "CImpl.hx"
    };
    for (const string& file : checked) {
        std::remove(("gen-haxe/" + file).c_str());
    }

    {
        std::ofstream thrift_file(thrift_path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << thrift_source;
    }

    map<string, string> parsed_options;
    std::unique_ptr<t_program> program(new t_program(thrift_path, "test_haxe_service_helpers"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "haxe", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    // each service has structs of its own
    const string a_args = read_file("gen-haxe/A_get_args.hx");
    REQUIRE(a_args.find("class A_get_args ") != string::npos);
    REQUIRE(a_args.find("public var a(get,set) : haxe.Int32;") != string::npos);
    const string b_args = read_file("gen-haxe/B_get_args.hx");
    REQUIRE(b_args.find("class B_get_args ") != string::npos);
    REQUIRE(b_args.find("public var a(") == string::npos);
    REQUIRE(read_file("gen-haxe/A_get_result.hx").find("public var success(get,set) : haxe.Int32;") != string::npos);
    REQUIRE(read_file("gen-haxe/B_get_result.hx").find("public var success(get,set) : String;") != string::npos);

    // and the client and the processor of each service use them
    for (const string service : { "A", "B" }) {
        const string impl = read_file("gen-haxe/" + service + "Impl.hx");
        REQUIRE(impl.find(" = new " + service + "_get_args();") != string::npos);
        REQUIRE(impl.find(" = new " + service + "_get_result();") != string::npos);
        const string processor = read_file("gen-haxe/" + service + "Processor.hx");
        REQUIRE(processor.find("var args : " + service + "_get_args = new " + service + "_get_args();")
                != string::npos);
        REQUIRE(processor.find("var result : " + service + "_get_result = new " + service + "_get_result();")
                != string::npos);
    }

    // an inherited method keeps the structs of the service that declares it
    REQUIRE(read_file("gen-haxe/C_put_args.hx").find("class C_put_args ") != string::npos);
    REQUIRE(read_file("gen-haxe/C_get_args.hx").empty());
    REQUIRE(read_file("gen-haxe/CImpl.hx").find("_get_args") == string::npos);

    std::remove(thrift_path.c_str());
}
