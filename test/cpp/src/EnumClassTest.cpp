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
#include <sstream>
#include <cassert>
#include <type_traits>
#include <string>

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
    
    // Test 5: Verify to_string() works with enum class
    {
        Numberz one = Numberz::ONE;
        Numberz five = Numberz::FIVE;
        Numberz eight = Numberz::EIGHT;
        
        std::string str_one = to_string(one);
        std::string str_five = to_string(five);
        std::string str_eight = to_string(eight);
        
        assert(str_one == "ONE");
        assert(str_five == "FIVE");
        assert(str_eight == "EIGHT");
        std::cout << "  ✓ to_string() with enum class works (ONE, FIVE, EIGHT)" << std::endl;
    }
    
    // Test 6: Verify operator<< works with enum class
    {
        Numberz two = Numberz::TWO;
        Numberz three = Numberz::THREE;
        
        std::ostringstream oss;
        oss << two << " and " << three;
        
        std::string result = oss.str();
        assert(result == "TWO and THREE");
        std::cout << "  ✓ operator<< with enum class works (TWO and THREE)" << std::endl;
    }
    
    // Test 7: Verify to_string() for invalid/cast enum values
    {
        // Cast an invalid value to enum (edge case testing)
        Numberz invalid = static_cast<Numberz>(999);
        std::string str_invalid = to_string(invalid);
        
        // Should fall back to numeric representation
        assert(str_invalid == "999");
        std::cout << "  ✓ to_string() handles invalid enum values (999)" << std::endl;
    }
    
    // Test 8: Verify operator<< for invalid/cast enum values
    {
        Numberz invalid = static_cast<Numberz>(777);
        std::ostringstream oss;
        oss << invalid;
        
        std::string result = oss.str();
        assert(result == "777");
        std::cout << "  ✓ operator<< handles invalid enum values (777)" << std::endl;
    }
    
    // Test 9: Verify enum class with zero value
    {
        Numberz zero = static_cast<Numberz>(0);
        std::string str_zero = to_string(zero);
        
        std::ostringstream oss;
        oss << zero;
        
        // Both should output "0" since there's no named value
        assert(str_zero == "0");
        assert(oss.str() == "0");
        std::cout << "  ✓ to_string() and operator<< work with zero value" << std::endl;
    }
    
    // Test 10: Verify all Numberz enum values can be converted to string
    {
        std::ostringstream oss;
        oss << Numberz::ONE << ", "
            << Numberz::TWO << ", "
            << Numberz::THREE << ", "
            << Numberz::FIVE << ", "
            << Numberz::SIX << ", "
            << Numberz::EIGHT;
        
        std::string result = oss.str();
        assert(result == "ONE, TWO, THREE, FIVE, SIX, EIGHT");
        std::cout << "  ✓ All Numberz enum values stream correctly" << std::endl;
    }
    
    std::cout << "\n✅ All pure_enums=enum_class tests passed!" << std::endl;
    std::cout << "   Verified at compile-time: enum class properties enforced" << std::endl;
    std::cout << "   Verified at runtime: to_string(), operator<< work correctly" << std::endl;
    return 0;
}
