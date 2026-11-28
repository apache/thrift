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

using std::map;
using std::string;
using cpp_generator_test_utils::parse_thrift_for_test;
using cpp_generator_test_utils::read_file;

namespace {

// Returns the text from the first occurrence of begin up to the next "public static class ", or
// to the end of the file, so that an assertion can be limited to one nested class.
string section(const string& text, const string& begin) {
    size_t start = text.find(begin);
    REQUIRE(start != string::npos);
    size_t end = text.find("public static class ", start + begin.size());
    return text.substr(start, end == string::npos ? string::npos : end - start);
}

}

// THRIFT-4228: the generated code declared locals that nothing read.
TEST_CASE("t_java_generator emits no dead local stores", "[functional]")
{
    const string path = "test_java_dead_store.thrift";
    {
        std::ofstream thrift_file(path, std::ios::binary);
        REQUIRE(thrift_file.is_open());
        thrift_file << "namespace java dead.store\n"
                       "struct Empty {}\n"
                       "struct Fields {\n"
                       "  1: required i32 a\n"
                       "  2: optional i32 b\n"
                       "  3: i32 c\n"
                       "}\n"
                       "service DeadStore {\n"
                       "  void doNothing()\n"
                       "  oneway void fire()\n"
                       "  i32 value()\n"
                       "}\n";
    }
    map<string, string> parsed_options;

    std::unique_ptr<t_program> program(new t_program(path, "test_java_dead_store"));
    parse_thrift_for_test(program.get());

    std::unique_ptr<t_generator> gen(
        t_generator_registry::get_generator(program.get(), "java", parsed_options, ""));
    REQUIRE(gen != nullptr);
    REQUIRE_NOTHROW(gen->generate_program());

    string service = read_file("gen-java/dead/store/DeadStore.java");
    string empty = read_file("gen-java/dead/store/Empty.java");
    string fields = read_file("gen-java/dead/store/Fields.java");
    REQUIRE(!service.empty());
    REQUIRE(!empty.empty());
    REQUIRE(!fields.empty());

    // A oneway call never reads a reply, so its getResult() must not set up a protocol.
    string fire = section(service, "public static class fire_call ");
    REQUIRE(fire.find("getResult()") != string::npos);
    REQUIRE(fire.find("memoryTransport") == string::npos);
    REQUIRE(fire.find("TProtocol prot =") == string::npos);

    // Calls with a reply keep it.
    for (const char* call : {"public static class doNothing_call ", "public static class value_call "}) {
        string body = section(service, call);
        REQUIRE(body.find("TMemoryInputTransport memoryTransport") != string::npos);
        REQUIRE(body.find("new Client(prot)") != string::npos);
    }

    // A struct without fields has nothing to read through the tuple protocol.
    string empty_scheme = section(empty, "private static class EmptyTupleScheme ");
    REQUIRE(empty_scheme.find("TTupleProtocol") == string::npos);

    string fields_scheme = section(fields, "private static class FieldsTupleScheme ");
    REQUIRE(fields_scheme.find("TTupleProtocol iprot") != string::npos);
    REQUIRE(fields_scheme.find("TTupleProtocol oprot") != string::npos);

    std::remove("gen-java/dead/store/DeadStore.java");
    std::remove("gen-java/dead/store/Empty.java");
    std::remove("gen-java/dead/store/Fields.java");
    std::remove(path.c_str());
}
