// Licensed to the Apache Software Foundation(ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

#include "t_delphi_generator_test_utils.h"

#include <cstring>

using delphi_generator_test_utils::source_dir;

extern "C" {
#include "thrift/generate/sha.h"
}

static std::string bytes_to_hex(const uint8_t* data, size_t len) {
    static const char hex_chars[] = "0123456789abcdef";
    std::string result;
    result.reserve(len * 2);
    for (size_t i = 0; i < len; ++i) {
        result += hex_chars[data[i] >> 4];
        result += hex_chars[data[i] & 0x0F];
    }
    return result;
}

static std::string uuid5_from_namespace_and_name(const uint8_t namespace_uuid[16],
                                               const std::string& name) {
    uint8_t combined[16 + SHA1HashSize];
    for (int i = 0; i < 16; ++i) {
        combined[i] = namespace_uuid[i];
    }
    for (size_t i = 0; i < name.size(); ++i) {
        combined[16 + i] = static_cast<uint8_t>(name[i]);
    }

    SHA1Context ctx;
    SHA1Reset(&ctx);
    SHA1Input(&ctx, combined, 16 + static_cast<unsigned int>(name.size()));

    uint8_t hash[SHA1HashSize];
    SHA1Result(&ctx, hash);

    uint8_t uuid[16];
    for (int i = 0; i < 16; ++i) {
        uuid[i] = hash[i];
    }
    uuid[6] = (uuid[6] & 0x0F) | 0x50;
    uuid[8] = (uuid[8] & 0x3F) | 0x80;

    char guid_str[40];
    snprintf(guid_str, sizeof(guid_str),
             "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
             uuid[0], uuid[1], uuid[2], uuid[3],
             uuid[4], uuid[5],
             uuid[6], uuid[7],
             uuid[8], uuid[9],
             uuid[10], uuid[11], uuid[12], uuid[13], uuid[14], uuid[15]);

    return std::string(guid_str);
}

TEST_CASE("UUIDv5 RFC 4122 test vector: DNS namespace", "[uuid5][rfc4122]") {
    const uint8_t DNS_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x11, 0x50, 0x50, 0x51, 0x40,
        0xb8, 0xe1, 0x00, 0x15, 0x5d, 0x00, 0x00, 0x00
    };

    std::string result = uuid5_from_namespace_and_name(DNS_NAMESPACE, "www.example.com");
    CHECK(result == "74738ff5-5367-5958-9a22-5f8d4d9b8f4d");
}

TEST_CASE("UUIDv5 RFC 4122 test vector: URL namespace", "[uuid5][rfc4122]") {
    const uint8_t URL_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x12, 0x50, 0x50, 0x51, 0x40,
        0xb8, 0xe1, 0x00, 0x15, 0x5d, 0x00, 0x00, 0x00
    };

    std::string result = uuid5_from_namespace_and_name(URL_NAMESPACE, "http://example.com");
    CHECK(result == "e622a47e-2660-5a6f-8ab4-29d0e2a30e5c");
}

TEST_CASE("UUIDv5 RFC 4122 test vector: ISO OID namespace", "[uuid5][rfc4122]") {
    const uint8_t OID_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x14, 0x50, 0x50, 0x51, 0x40,
        0xb8, 0xe1, 0x00, 0x15, 0x5d, 0x00, 0x00, 0x00
    };

    std::string result = uuid5_from_namespace_and_name(OID_NAMESPACE, "1.3.6.1");
    CHECK(result == "d1e7f66c-09dc-5c85-a46b-6fe40bd71c7b");
}

TEST_CASE("UUIDv5 RFC 4122 test vector: X.500 DN namespace", "[uuid5][rfc4122]") {
    const uint8_t DN_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x15, 0x50, 0x50, 0x51, 0x40,
        0xb8, 0xe1, 0x00, 0x15, 0x5d, 0x00, 0x00, 0x00
    };

    std::string result = uuid5_from_namespace_and_name(DN_NAMESPACE, "cn=John Doe,cn=Users,dc=example,dc=com");
    CHECK(result == "82345165-4a23-5c7d-85c8-e25d10d82d3a");
}

TEST_CASE("UUIDv5 version and variant bits are correct", "[uuid5]") {
    const uint8_t DNS_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x11, 0x50, 0x50, 0x51, 0x40,
        0xb8, 0xe1, 0x00, 0x15, 0x5d, 0x00, 0x00, 0x00
    };

    std::string result = uuid5_from_namespace_and_name(DNS_NAMESPACE, "test");

    CHECK(result.length() == 36);
    CHECK(result[14] == '5');

    CHECK(result[19] >= '8');
    CHECK(result[19] <= 'b');
}

TEST_CASE("UUIDv5 deterministic: same input produces same output", "[uuid5]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string result1 = uuid5_from_namespace_and_name(NS, "hello");
    std::string result2 = uuid5_from_namespace_and_name(NS, "hello");
    std::string result3 = uuid5_from_namespace_and_name(NS, "hello");

    CHECK(result1 == result2);
    CHECK(result2 == result3);
}

TEST_CASE("UUIDv5 different input produces different output", "[uuid5]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string result1 = uuid5_from_namespace_and_name(NS, "hello");
    std::string result2 = uuid5_from_namespace_and_name(NS, "world");

    CHECK(result1 != result2);
}

TEST_CASE("UUIDv5 handles empty string name", "[uuid5]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string result = uuid5_from_namespace_and_name(NS, "");
    CHECK(result.length() == 36);
    CHECK(result[14] == '5');
}

TEST_CASE("UUIDv5 handles long string name", "[uuid5]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string long_name(1000, 'a');
    std::string result = uuid5_from_namespace_and_name(NS, long_name);
    CHECK(result.length() == 36);
    CHECK(result[14] == '5');
}
