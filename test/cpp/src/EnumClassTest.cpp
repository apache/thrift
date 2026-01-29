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
 * Test file to verify that pure_enums=enum_class generated code compiles and works correctly.
 * This exercises the enum_class option using ThriftTest types.
 */

#include <iostream>
#include <cassert>
#include <type_traits>

// Include generated thrift types with enum_class option
#include "ThriftTest_types.h"

using namespace thrift::test;

int main() {
    std::cout << "Testing pure_enums=enum_class with ThriftTest types..." << std::endl;
    
    // Compile-time verification that Numberz is an enum class
    static_assert(std::is_enum<Numberz>::value, "Numberz should be an enum type");
    // enum class doesn't implicitly convert to int, which is a key characteristic
    static_assert(!std::is_convertible<Numberz, int>::value, 
                  "Numberz should be enum class (not implicitly convertible to int)");
    std::cout << "  ✓ Compile-time verification: Numberz is enum class" << std::endl;
    
    // Test 1: Verify enum class can be used with scoped names
    {
        Numberz num = Numberz::ONE;
        assert(static_cast<int>(num) == 1);
        std::cout << "  ✓ Enum class scoped access works (Numberz::ONE)" << std::endl;
    }
    
    // Test 2: Verify different enum values
    {
        Numberz two = Numberz::TWO;
        Numberz five = Numberz::FIVE;
        assert(static_cast<int>(two) == 2);
        assert(static_cast<int>(five) == 5);
        std::cout << "  ✓ Multiple enum class values work" << std::endl;
    }
    
    // Test 3: Verify enum class comparison
    {
        Numberz a = Numberz::THREE;
        Numberz b = Numberz::THREE;
        Numberz c = Numberz::FIVE;
        assert(a == b);
        assert(a != c);
        std::cout << "  ✓ Enum class comparison works" << std::endl;
    }
    
    // Test 4: Verify enum class in switch statement
    {
        Numberz num = Numberz::EIGHT;
        bool found = false;
        switch(num) {
            case Numberz::ONE:
                break;
            case Numberz::TWO:
                break;
            case Numberz::EIGHT:
                found = true;
                break;
            default:
                break;
        }
        assert(found);
        std::cout << "  ✓ Enum class in switch statements works" << std::endl;
    }
    
    std::cout << "\n✅ All pure_enums=enum_class tests passed!" << std::endl;
    std::cout << "   Verified at compile-time: enum class properties enforced" << std::endl;
    return 0;
}
