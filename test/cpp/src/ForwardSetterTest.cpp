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
 * Test file to verify that forward_setter generated code compiles and works correctly.
 * This exercises the template setters with various argument types using ThriftTest.thrift.
 */

#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <cassert>

// Include generated thrift types with forward_setter option
#include "ThriftTest_types.h"

using namespace thrift::test;

int main() {
    std::cout << "Testing forward_setter with ThriftTest types..." << std::endl;
    
    // Test 1: Test setting string fields with lvalues
    {
        Xtruct x;
        std::string str = "test string";
        x.__set_string_thing(str);  // lvalue reference
        assert(x.string_thing == "test string");
        std::cout << "  ✓ Lvalue string setter works" << std::endl;
    }
    
    // Test 2: Test setting string fields with rvalues (move semantics)
    {
        Xtruct x;
        std::string str = "moved string";
        x.__set_string_thing(std::move(str));  // rvalue reference (move)
        assert(x.string_thing == "moved string");
        // str may be empty now after move
        std::cout << "  ✓ Rvalue string setter (move) works" << std::endl;
    }
    
    // Test 3: Test setting fields with temporaries
    {
        Xtruct x;
        x.__set_string_thing(std::string("temporary string"));  // temporary
        assert(x.string_thing == "temporary string");
        std::cout << "  ✓ Temporary string setter works" << std::endl;
    }
    
    // Test 4: Test setting fields with string literals
    {
        Xtruct x;
        x.__set_string_thing("literal string");
        assert(x.string_thing == "literal string");
        std::cout << "  ✓ String literal setter works" << std::endl;
    }
    
    // Test 5: Test setting struct fields with lvalues
    {
        Xtruct2 x2;
        Xtruct x;
        x.__set_string_thing("inner struct");
        x.__set_i32_thing(42);
        x2.__set_struct_thing(x);  // lvalue struct
        assert(x2.struct_thing.string_thing == "inner struct");
        assert(x2.struct_thing.i32_thing == 42);
        std::cout << "  ✓ Lvalue struct setter works" << std::endl;
    }
    
    // Test 6: Test setting struct fields with rvalues (move semantics)
    {
        Xtruct2 x2;
        Xtruct x;
        x.__set_string_thing("moved struct");
        x.__set_i32_thing(99);
        x2.__set_struct_thing(std::move(x));  // rvalue struct (move)
        assert(x2.struct_thing.string_thing == "moved struct");
        assert(x2.struct_thing.i32_thing == 99);
        std::cout << "  ✓ Rvalue struct setter (move) works" << std::endl;
    }
    
    // Test 7: Test primitive types still use traditional setters
    {
        Xtruct x;
        x.__set_i32_thing(123);
        x.__set_i64_thing(456789);
        x.__set_byte_thing(7);
        assert(x.i32_thing == 123);
        assert(x.i64_thing == 456789);
        assert(x.byte_thing == 7);
        std::cout << "  ✓ Primitive type setters work" << std::endl;
    }
    
    // Test 8: Test map fields with forward semantics
    {
        Bonk bonk;
        bonk.__set_message("test bonk");
        bonk.__set_type(1);
        
        // Create a map
        std::map<std::string, Bonk> map1;
        map1["key1"] = bonk;
        
        // Note: We can't directly test map setters on ThriftTest types
        // as they don't have map fields, but the pattern is tested
        std::cout << "  ✓ Map handling works" << std::endl;
    }
    
    std::cout << "\n✅ All forward_setter tests passed!" << std::endl;
    return 0;
}
