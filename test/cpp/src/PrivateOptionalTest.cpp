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
 * Test file to verify that private_optional generated code compiles and works correctly.
 * This exercises the private_optional option using ThriftTest types.
 */

#include <iostream>
#include <string>
#include <cassert>
#include <type_traits>

// Include generated thrift types with private_optional option
#include "ThriftTest_types.h"

using namespace thrift::test;

// SFINAE test to check if a field is directly accessible
template<typename T, typename = void>
struct has_public_string_thing : std::false_type {};

template<typename T>
struct has_public_string_thing<T, decltype(void(std::declval<T>().string_thing))> : std::true_type {};

// SFINAE test to check if optional field 'aa' is directly accessible
template<typename T, typename = void>
struct has_public_aa : std::false_type {};

template<typename T>
struct has_public_aa<T, decltype(void(std::declval<T>().aa))> : std::true_type {};

// SFINAE test to check if required field 'ab' is directly accessible
template<typename T, typename = void>
struct has_public_ab : std::false_type {};

template<typename T>
struct has_public_ab<T, decltype(void(std::declval<T>().ab))> : std::true_type {};

int main() {
    std::cout << "Testing private_optional with ThriftTest types..." << std::endl;
    
    // Compile-time verification: required fields should still be publicly accessible
    static_assert(has_public_string_thing<Xtruct>::value,
                  "Required fields (like string_thing in Xtruct) should remain public");
    std::cout << "  ✓ Compile-time verification: Required fields are public (Xtruct)" << std::endl;
    
    // Compile-time verification for StructB: optional field 'aa' should be private
    static_assert(!has_public_aa<StructB>::value,
                  "Optional field 'aa' in StructB should be private with private_optional");
    std::cout << "  ✓ Compile-time verification: Optional field 'aa' is private (StructB)" << std::endl;
    
    // Compile-time verification for StructB: required field 'ab' should be public
    static_assert(has_public_ab<StructB>::value,
                  "Required field 'ab' in StructB should remain public");
    std::cout << "  ✓ Compile-time verification: Required field 'ab' is public (StructB)" << std::endl;
    
    // Test 1: Verify getters work for accessing fields
    {
        Xtruct x;
        x.__set_string_thing("test");
        const std::string& str = x.__get_string_thing();
        assert(str == "test");
        std::cout << "  ✓ Getter for string field works" << std::endl;
    }
    
    // Test 2: Verify setters work
    {
        Xtruct x;
        x.__set_i32_thing(42);
        x.__set_i64_thing(1234567890);
        assert(x.__get_i32_thing() == 42);
        assert(x.__get_i64_thing() == 1234567890);
        std::cout << "  ✓ Setters for primitive fields work" << std::endl;
    }
    
    // Test 3: Verify getters/setters for complex types
    {
        Xtruct2 x2;
        Xtruct x;
        x.__set_string_thing("nested");
        x.__set_i32_thing(99);
        x2.__set_struct_thing(x);
        // With private_optional, use getters to access fields
        assert(x2.__get_struct_thing().__get_string_thing() == "nested");
        assert(x2.__get_struct_thing().__get_i32_thing() == 99);
        std::cout << "  ✓ Getters/setters for struct fields work" << std::endl;
    }
    
    // Test 4: Verify direct access to required fields still works
    {
        Xtruct x;
        x.string_thing = "direct access";
        x.i32_thing = 123;
        assert(x.string_thing == "direct access");
        assert(x.i32_thing == 123);
        std::cout << "  ✓ Direct access to required fields works" << std::endl;
    }
    
    // Test 5: Test StructB with optional and required fields
    {
        StructB sb;
        StructA sa;
        sa.__set_s("test struct");
        
        // Set optional field 'aa' using setter (cannot access directly)
        sb.__set_aa(sa);
        
        // Set and access required field 'ab' directly (it's public)
        sb.ab = sa;
        
        // Verify using getters
        assert(sb.__get_aa().__get_s() == "test struct");
        assert(sb.ab.__get_s() == "test struct");
        std::cout << "  ✓ StructB: Optional field private, required field public" << std::endl;
    }
    
    std::cout << "\n✅ All private_optional tests passed!" << std::endl;
    std::cout << "   Verified at compile-time: Optional fields are private, required fields public" << std::endl;
    return 0;
}
