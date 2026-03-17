/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

/**
 * Test file to verify that generated code compiles and works correctly when
 * both private_optional and template_streamop options are enabled together.
 */

#include <iostream>
#include <sstream>
#include <string>
#include <cassert>
#include <type_traits>
#include <cstring>
#include <cstdio>
#include <cstdint>

// Include generated thrift types with private_optional,template_streamop options
#include "ThriftTest_types.h"
#include <thrift/TToString.h>

using namespace thrift::test;

// SFINAE helpers to verify field accessibility at compile time.
// Each requires a separate template because C++ template parameters cannot be
// string literals, so each field name gets its own specialisation.
template<typename T, typename = void>
struct has_public_aa : std::false_type {};

template<typename T>
struct has_public_aa<T, decltype(void(std::declval<T>().aa))> : std::true_type {};

// SFINAE test to check if required field 'ab' is directly accessible
template<typename T, typename = void>
struct has_public_ab : std::false_type {};

template<typename T>
struct has_public_ab<T, decltype(void(std::declval<T>().ab))> : std::true_type {};

// Custom minimal stream - verifies that template_streamop works with non-std streams
class MinimalStream {
private:
    std::string buf_;
public:
    MinimalStream& operator<<(const std::string& s) { buf_ += s; return *this; }
    MinimalStream& operator<<(const char* s) { buf_ += s; return *this; }
    MinimalStream& operator<<(char c) { buf_ += c; return *this; }
    MinimalStream& operator<<(int32_t i) { buf_ += std::to_string(i); return *this; }
    MinimalStream& operator<<(int64_t i) { buf_ += std::to_string(i); return *this; }
    MinimalStream& operator<<(uint32_t i) { buf_ += std::to_string(i); return *this; }
    MinimalStream& operator<<(uint64_t i) { buf_ += std::to_string(i); return *this; }
    MinimalStream& operator<<(double d) {
        char tmp[64];
        std::snprintf(tmp, sizeof(tmp), "%g", d);
        buf_ += tmp;
        return *this;
    }
    MinimalStream& operator<<(bool b) { buf_ += (b ? "true" : "false"); return *this; }
    const std::string& str() const { return buf_; }
};

int main() {
    std::cout << "Testing private_optional + template_streamop combined..." << std::endl;

    // Compile-time: optional field 'aa' in StructB must be private
    static_assert(!has_public_aa<StructB>::value,
                  "Optional field 'aa' in StructB should be private with private_optional");
    std::cout << "  ✓ Compile-time: optional field 'aa' is private in StructB" << std::endl;

    // Compile-time: required field 'ab' in StructB must remain public
    static_assert(has_public_ab<StructB>::value,
                  "Required field 'ab' in StructB should remain public");
    std::cout << "  ✓ Compile-time: required field 'ab' is public in StructB" << std::endl;

    // Test 1: private_optional getters/setters work
    {
        Xtruct x;
        x.__set_string_thing("hello");
        x.__set_i32_thing(42);
        assert(x.__get_string_thing() == "hello");
        assert(x.__get_i32_thing() == 42);
        std::cout << "  ✓ private_optional getters/setters work on Xtruct" << std::endl;
    }

    // Test 2: template_streamop with std::ostringstream
    {
        Xtruct x;
        x.__set_string_thing("stream test");
        x.__set_i32_thing(99);

        std::ostringstream oss;
        oss << x;
        std::string result = oss.str();

        assert(!result.empty());
        assert(result.find("stream test") != std::string::npos);
        assert(result.find("99") != std::string::npos);
        std::cout << "  ✓ template_streamop works with std::ostringstream" << std::endl;
    }

    // Test 3: template_streamop with custom MinimalStream
    {
        Xtruct x;
        x.__set_string_thing("minimal stream");
        x.__set_i32_thing(7);

        MinimalStream ms;
        ms << x;
        std::string result = ms.str();

        assert(!result.empty());
        assert(result.find("minimal stream") != std::string::npos);
        assert(result.find("7") != std::string::npos);
        std::cout << "  ✓ template_streamop works with custom MinimalStream" << std::endl;
    }

    // Test 4: Stream a struct whose optional fields were set via setters (combines both options)
    {
        StructB sb;
        StructA sa;
        sa.__set_s("optional value");

        // private_optional: must use setter, not direct assignment
        sb.__set_aa(sa);
        // required field: direct access is still fine
        sb.ab = sa;

        // template_streamop: stream the struct to both stream types
        std::ostringstream oss;
        oss << sb;
        std::string oss_result = oss.str();

        MinimalStream ms;
        ms << sb;
        std::string ms_result = ms.str();

        assert(!oss_result.empty());
        assert(!ms_result.empty());
        assert(oss_result.find("optional value") != std::string::npos);
        assert(ms_result.find("optional value") != std::string::npos);
        std::cout << "  ✓ Combined: StructB with private optional fields streams correctly" << std::endl;
    }

    // Test 5: to_string works with combined options
    {
        Xtruct x;
        x.__set_string_thing("to_string test");
        x.__set_i32_thing(123);

        std::string s = apache::thrift::to_string(x);
        assert(!s.empty());
        assert(s.find("to_string test") != std::string::npos);
        std::cout << "  ✓ to_string works with combined options" << std::endl;
    }

    std::cout << "\n✅ All private_optional + template_streamop combined tests passed!" << std::endl;
    return 0;
}
