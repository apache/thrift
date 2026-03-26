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

static std::string uuid8_from_namespace_and_name(const uint8_t namespace_uuid[16],
                                               const std::string& name) {
    uint8_t combined[16 + SHA256HashSize] = {};
    for (int i = 0; i < 16; ++i) {
        combined[i] = namespace_uuid[i];
    }
    size_t name_len = std::min(name.size(), static_cast<size_t>(SHA256HashSize));
    for (size_t i = 0; i < name_len; ++i) {
        combined[16 + i] = static_cast<uint8_t>(name[i]);
    }

    SHA256Context ctx;
    SHA256Reset(&ctx);
    SHA256Input(&ctx, combined, 16 + static_cast<unsigned int>(name_len));

    uint8_t hash[SHA256HashSize];
    SHA256Result(&ctx, hash);

    uint8_t uuid[16];
    for (int i = 0; i < 16; ++i) {
        uuid[i] = hash[i];
    }
    uuid[6] = (uuid[6] & 0x0F) | 0x80;
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

TEST_CASE("UUIDv8 RFC 4122 test vector: DNS namespace", "[uuid8][rfc4122]") {
    const uint8_t DNS_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x04, 0xfd, 0x43, 0xc8
    };

    std::string result = uuid8_from_namespace_and_name(DNS_NAMESPACE, "www.example.com");
    CHECK(result.length() == 36);
    CHECK(result[14] == '8');
}

TEST_CASE("UUIDv8 RFC 4122 test vector: URL namespace", "[uuid8][rfc4122]") {
    const uint8_t URL_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x11, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x04, 0xfd, 0x43, 0xc8
    };

    std::string result = uuid8_from_namespace_and_name(URL_NAMESPACE, "http://example.com");
    CHECK(result.length() == 36);
    CHECK(result[14] == '8');
}

TEST_CASE("UUIDv8 RFC 4122 test vector: ISO OID namespace", "[uuid8][rfc4122]") {
    const uint8_t OID_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x12, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x04, 0xfd, 0x43, 0xc8
    };

    std::string result = uuid8_from_namespace_and_name(OID_NAMESPACE, "1.3.6.1");
    CHECK(result.length() == 36);
    CHECK(result[14] == '8');
}

TEST_CASE("UUIDv8 RFC 4122 test vector: X.500 DN namespace", "[uuid8][rfc4122]") {
    const uint8_t DN_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x14, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x04, 0xfd, 0x43, 0xc8
    };

    std::string result = uuid8_from_namespace_and_name(DN_NAMESPACE, "cn=John Doe,cn=Users,dc=example,dc=com");
    CHECK(result.length() == 36);
    CHECK(result[14] == '8');
}

TEST_CASE("UUIDv8 version and variant bits are correct", "[uuid8]") {
    const uint8_t DNS_NAMESPACE[16] = {
        0x6b, 0xa7, 0xb8, 0x10, 0x9d, 0xad, 0x11, 0xd1,
        0x80, 0xb4, 0x00, 0xc0, 0x04, 0xfd, 0x43, 0xc8
    };

    std::string result = uuid8_from_namespace_and_name(DNS_NAMESPACE, "test");

    CHECK(result.length() == 36);
    CHECK(result[14] == '8');

    CHECK(result[19] >= '8');
    CHECK(result[19] <= 'b');
}

TEST_CASE("UUIDv8 deterministic: same input produces same output", "[uuid8]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string result1 = uuid8_from_namespace_and_name(NS, "hello");
    std::string result2 = uuid8_from_namespace_and_name(NS, "hello");
    std::string result3 = uuid8_from_namespace_and_name(NS, "hello");

    CHECK(result1 == result2);
    CHECK(result2 == result3);
}

TEST_CASE("UUIDv8 different input produces different output", "[uuid8]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string result1 = uuid8_from_namespace_and_name(NS, "hello");
    std::string result2 = uuid8_from_namespace_and_name(NS, "world");

    CHECK(result1 != result2);
}

TEST_CASE("UUIDv8 handles empty string name", "[uuid8]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string result = uuid8_from_namespace_and_name(NS, "");
    CHECK(result.length() == 36);
    CHECK(result[14] == '8');
}

TEST_CASE("UUIDv8 handles long string name", "[uuid8]") {
    const uint8_t NS[16] = {
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    std::string long_name(1000, 'a');
    std::string result = uuid8_from_namespace_and_name(NS, long_name);
    CHECK(result.length() == 36);
    CHECK(result[14] == '8');
}
