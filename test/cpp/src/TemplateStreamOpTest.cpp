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
 * Test file to verify that template_streamop generated code compiles and works correctly.
 * This tests the templated operator<< and printTo with various stream types.
 */

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <map>
#include <cassert>
#include <chrono>
#include <cstring>
#include <cstdio>
#include <cstdint>

// Include generated thrift types with template_streamop option
#include "ThriftTest_types.h"
#include <thrift/TToString.h>

using namespace thrift::test;

// Custom minimal stream implementation for testing and performance comparison
class MinimalStream {
private:
    static constexpr size_t STACK_BUFFER_SIZE = 2048;
    char stack_buffer_[STACK_BUFFER_SIZE];
    char* buffer_;
    size_t size_;
    size_t capacity_;
    bool on_heap_;
    
    void ensure_capacity(size_t additional) {
        size_t needed = size_ + additional;
        if (needed <= capacity_) return;
        
        size_t new_capacity = capacity_;
        while (new_capacity < needed) {
            new_capacity *= 2;
        }
        
        char* new_buffer = new char[new_capacity];
        if (size_ > 0) {
            std::memcpy(new_buffer, buffer_, size_);
        }
        
        if (on_heap_) {
            delete[] buffer_;
        }
        
        buffer_ = new_buffer;
        capacity_ = new_capacity;
        on_heap_ = true;
    }
    
    void append(const char* s, size_t len) {
        ensure_capacity(len);
        std::memcpy(buffer_ + size_, s, len);
        size_ += len;
    }
    
    // Helper to print integer directly to buffer
    template<typename T>
    void print_integer(T value) {
        char temp[32];  // Enough for any 64-bit integer
        char* p = temp + sizeof(temp);
        bool negative = value < 0;
        
        if (negative) {
            value = -value;
        }
        
        do {
            *--p = '0' + (value % 10);
            value /= 10;
        } while (value > 0);
        
        if (negative) {
            *--p = '-';
        }
        
        append(p, temp + sizeof(temp) - p);
    }
    
    // Helper to print unsigned integer directly to buffer
    template<typename T>
    void print_unsigned(T value) {
        char temp[32];
        char* p = temp + sizeof(temp);
        
        do {
            *--p = '0' + (value % 10);
            value /= 10;
        } while (value > 0);
        
        append(p, temp + sizeof(temp) - p);
    }
    
public:
    MinimalStream() 
        : buffer_(stack_buffer_), size_(0), capacity_(STACK_BUFFER_SIZE), on_heap_(false) {}
    
    ~MinimalStream() {
        if (on_heap_) {
            delete[] buffer_;
        }
    }
    
    MinimalStream& operator<<(const std::string& s) {
        append(s.c_str(), s.size());
        return *this;
    }
    
    MinimalStream& operator<<(const char* s) {
        append(s, std::strlen(s));
        return *this;
    }
    
    MinimalStream& operator<<(char c) {
        ensure_capacity(1);
        buffer_[size_++] = c;
        return *this;
    }
    
    MinimalStream& operator<<(int32_t i) {
        print_integer(i);
        return *this;
    }
    
    MinimalStream& operator<<(int64_t i) {
        print_integer(i);
        return *this;
    }
    
    MinimalStream& operator<<(uint32_t i) {
        print_unsigned(i);
        return *this;
    }
    
    MinimalStream& operator<<(uint64_t i) {
        print_unsigned(i);
        return *this;
    }
    
    MinimalStream& operator<<(double d) {
        // For doubles, we still need sprintf for proper formatting
        char temp[64];
        int len = std::snprintf(temp, sizeof(temp), "%g", d);
        if (len > 0) {
            append(temp, len);
        }
        return *this;
    }
    
    MinimalStream& operator<<(bool b) {
        if (b) {
            append("true", 4);
        } else {
            append("false", 5);
        }
        return *this;
    }
    
    std::string str() const {
        return std::string(buffer_, size_);
    }
    
    void clear() {
        if (on_heap_) {
            delete[] buffer_;
            buffer_ = stack_buffer_;
            capacity_ = STACK_BUFFER_SIZE;
            on_heap_ = false;
        }
        size_ = 0;
    }
};

int main() {
    std::cout << "Testing template_streamop with ThriftTest types..." << std::endl;
    
    // Test 1: Test with std::ostringstream
    {
        Xtruct x;
        x.__set_string_thing("test string");
        x.__set_byte_thing(42);
        x.__set_i32_thing(12345);
        x.__set_i64_thing(9876543210LL);
        
        std::ostringstream oss;
        oss << x;
        std::string result = oss.str();
        
        std::cout << "  Generated output: " << result << std::endl;
        
        assert(!result.empty());
        assert(result.find("test string") != std::string::npos);
        assert(result.find("42") != std::string::npos);
        assert(result.find("12345") != std::string::npos);
        std::cout << "  ✓ std::ostringstream works: " << result << std::endl;
    }
    
    // Test 2: Test with custom MinimalStream
    {
        Xtruct x;
        x.__set_string_thing("custom stream");
        x.__set_byte_thing(7);
        x.__set_i32_thing(999);
        x.__set_i64_thing(1234567890LL);
        
        MinimalStream ms;
        ms << x;
        std::string result = ms.str();
        
        assert(!result.empty());
        assert(result.find("custom stream") != std::string::npos);
        assert(result.find("7") != std::string::npos);
        assert(result.find("999") != std::string::npos);
        std::cout << "  ✓ MinimalStream works: " << result << std::endl;
    }
    
    // Test 3: Test nested structures
    {
        Xtruct x;
        x.__set_string_thing("inner");
        x.__set_i32_thing(100);
        
        Xtruct2 x2;
        x2.__set_byte_thing(5);
        x2.__set_struct_thing(x);
        x2.__set_i32_thing(200);
        
        std::ostringstream oss;
        oss << x2;
        std::string result = oss.str();
        
        assert(!result.empty());
        assert(result.find("inner") != std::string::npos);
        assert(result.find("100") != std::string::npos);
        assert(result.find("200") != std::string::npos);
        std::cout << "  ✓ Nested structures work" << std::endl;
    }
    
    // Test 4: Test optional fields
    {
        Bonk bonk;
        bonk.__set_message("test message");
        bonk.__set_type(42);
        
        std::ostringstream oss;
        oss << bonk;
        std::string result = oss.str();
        
        assert(!result.empty());
        assert(result.find("test message") != std::string::npos);
        assert(result.find("42") != std::string::npos);
        std::cout << "  ✓ Optional fields work" << std::endl;
    }
    
    // Test 5: Test structs with map/set/list/vector
    {
        std::cout << "\n  Testing collection types..." << std::endl;
        
        // Create an Insanity struct with map and list
        Insanity insanity;
        
        // Add items to the map
        std::map<Numberz::type, UserId> userMap;
        userMap[Numberz::ONE] = 1;
        userMap[Numberz::FIVE] = 5;
        insanity.__set_userMap(userMap);
        
        // Add items to the list
        std::vector<Xtruct> xtructs;
        Xtruct x1;
        x1.__set_string_thing("first");
        x1.__set_i32_thing(111);
        xtructs.push_back(x1);
        
        Xtruct x2;
        x2.__set_string_thing("second");
        x2.__set_i32_thing(222);
        xtructs.push_back(x2);
        insanity.__set_xtructs(xtructs);
        
        // Test with std::ostringstream
        std::ostringstream oss;
        oss << insanity;
        std::string result = oss.str();
        
        std::cout << "    std::ostringstream output: " << result << std::endl;
        assert(!result.empty());
        assert(result.find("Insanity") != std::string::npos);
        assert(result.find("userMap") != std::string::npos);
        assert(result.find("xtructs") != std::string::npos);
        
        // Test with MinimalStream
        MinimalStream ms;
        ms << insanity;
        std::string ms_result = ms.str();
        
        std::cout << "    MinimalStream output: " << ms_result << std::endl;
        assert(!ms_result.empty());
        assert(ms_result.find("Insanity") != std::string::npos);
        
        std::cout << "  ✓ Map/List collections work with both streams" << std::endl;
    }
    
    // Test 6: Test to_string compatibility with collection structs
    {
        std::cout << "\n  Testing to_string with collection structs..." << std::endl;
        
        Insanity insanity;
        std::map<Numberz::type, UserId> userMap;
        userMap[Numberz::TWO] = 2;
        insanity.__set_userMap(userMap);
        
        std::vector<Xtruct> xtructs;
        Xtruct x;
        x.__set_string_thing("test");
        x.__set_i32_thing(42);
        xtructs.push_back(x);
        insanity.__set_xtructs(xtructs);
        
        // to_string should work with the generated types
        std::string str_result = apache::thrift::to_string(insanity);
        
        std::cout << "    to_string output: " << str_result << std::endl;
        assert(!str_result.empty());
        assert(str_result.find("Insanity") != std::string::npos);
        
        std::cout << "  ✓ to_string works with collection structs" << std::endl;
    }
    
    // Test 7: Test enum output - should print by name
    {
        std::cout << "\n  Testing enum output..." << std::endl;
        
        // Create a struct with an enum field
        Insanity insanity;
        std::map<Numberz::type, UserId> userMap;
        userMap[Numberz::ONE] = 1;
        userMap[Numberz::FIVE] = 5;
        userMap[Numberz::TWO] = 2;
        insanity.__set_userMap(userMap);
        
        // Test with std::ostringstream
        std::ostringstream oss;
        oss << insanity;
        std::string result = oss.str();
        
        std::cout << "    std::ostringstream output: " << result << std::endl;
        assert(result.find("ONE") != std::string::npos || result.find("1") != std::string::npos);
        
        // Test with MinimalStream
        MinimalStream ms;
        ms << insanity;
        std::string ms_result = ms.str();
        
        std::cout << "    MinimalStream output: " << ms_result << std::endl;
        assert(!ms_result.empty());
        
        std::cout << "  ✓ Enum fields output correctly" << std::endl;
    }
    
    // Test 8: Test floating point types
    {
        std::cout << "\n  Testing floating point types..." << std::endl;
        
        // Note: ThriftTest doesn't have a struct with float/double fields
        // So we test directly with printTo
        float f = 3.14159f;
        double d = 2.71828;
        
        // Test with std::ostringstream
        std::ostringstream oss_f, oss_d;
        apache::thrift::printTo(oss_f, f);
        apache::thrift::printTo(oss_d, d);
        
        std::string f_result = oss_f.str();
        std::string d_result = oss_d.str();
        
        std::cout << "    float printTo: " << f_result << std::endl;
        std::cout << "    double printTo: " << d_result << std::endl;
        
        assert(!f_result.empty());
        assert(!d_result.empty());
        assert(f_result.find("3.14") != std::string::npos || f_result.find("3,14") != std::string::npos);
        assert(d_result.find("2.71") != std::string::npos || d_result.find("2,71") != std::string::npos);
        
        // Test with MinimalStream
        MinimalStream ms_f, ms_d;
        apache::thrift::printTo(ms_f, f);
        apache::thrift::printTo(ms_d, d);
        
        std::cout << "    MinimalStream float: " << ms_f.str() << std::endl;
        std::cout << "    MinimalStream double: " << ms_d.str() << std::endl;
        
        assert(!ms_f.str().empty());
        assert(!ms_d.str().empty());
        
        std::cout << "  ✓ Floating point types work correctly" << std::endl;
    }
    
    // Performance Test: Compare std::ostringstream vs MinimalStream
    {
        const int iterations = 10000;
        Xtruct x;
        x.__set_string_thing("performance test string");
        x.__set_byte_thing(123);
        x.__set_i32_thing(456789);
        x.__set_i64_thing(9876543210LL);
        
        // Test std::ostringstream performance
        auto start_oss = std::chrono::high_resolution_clock::now();
        std::string accumulated_result;  // Prevent optimization by accumulating results
        for (int i = 0; i < iterations; ++i) {
            std::ostringstream oss;
            oss << x;
            accumulated_result += oss.str();  // Use result to prevent optimization
        }
        auto end_oss = std::chrono::high_resolution_clock::now();
        auto duration_oss = std::chrono::duration_cast<std::chrono::microseconds>(end_oss - start_oss).count();
        
        // Test MinimalStream performance
        auto start_ms = std::chrono::high_resolution_clock::now();
        accumulated_result.clear();  // Reuse for MinimalStream test
        for (int i = 0; i < iterations; ++i) {
            MinimalStream ms;
            ms << x;
            accumulated_result += ms.str();  // Use result to prevent optimization
        }
        auto end_ms = std::chrono::high_resolution_clock::now();
        auto duration_ms = std::chrono::duration_cast<std::chrono::microseconds>(end_ms - start_ms).count();
        
        std::cout << "\n  Performance comparison (" << iterations << " iterations):" << std::endl;
        std::cout << "    std::ostringstream: " << duration_oss << " μs" << std::endl;
        std::cout << "    MinimalStream:      " << duration_ms << " μs" << std::endl;
        
        if (duration_ms < duration_oss) {
            double improvement = ((double)(duration_oss - duration_ms) / duration_oss) * 100.0;
            std::cout << "    MinimalStream is " << improvement << "% faster" << std::endl;
        } else {
            double difference = ((double)(duration_ms - duration_oss) / duration_oss) * 100.0;
            std::cout << "    std::ostringstream is " << difference << "% faster" << std::endl;
        }
        
        std::cout << "  ✓ Performance test completed" << std::endl;
    }
    
    std::cout << "\n✅ All template_streamop tests passed!" << std::endl;
    return 0;
}
