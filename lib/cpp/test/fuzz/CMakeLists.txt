#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

# Fuzz testing configuration

# Generate FuzzTest code
add_custom_command(OUTPUT gen-cpp/FuzzTest_types.cpp
                         gen-cpp/FuzzTest_types.h
    COMMAND ${THRIFT_COMPILER} --gen cpp ${PROJECT_SOURCE_DIR}/test/FuzzTest.thrift
)

# Create a library for the generated code
add_library(fuzztest_gen
  gen-cpp/FuzzTest_types.cpp
=)
target_link_libraries(fuzztest_gen thrift)

# Common fuzzing header
set(FUZZ_COMMON_HEADERS
  FuzzCommon.tcc
)

# Add fuzzing flags when using Clang
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  set(FUZZING_FLAGS "-fsanitize=fuzzer,address -g")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${FUZZING_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${FUZZING_FLAGS}")
endif()

# Compact protocol fuzzer
add_executable(FuzzParseCompact
  FuzzParseCompact.cpp
  ${FUZZ_COMMON_HEADERS}
)

target_link_libraries(FuzzParseCompact
  thrift
  fuzztest_gen
)

# Compact protocol roundtrip fuzzer
add_executable(FuzzRoundtripCompact
  FuzzRoundtripCompact.cpp
)

target_link_libraries(FuzzRoundtripCompact
  thrift
  fuzztest_gen
)

# Binary protocol fuzzer
add_executable(FuzzParseBinary
  FuzzParseBinary.cpp
  ${FUZZ_COMMON_HEADERS}
)

target_link_libraries(FuzzParseBinary
  thrift
  fuzztest_gen
)

# Binary protocol roundtrip fuzzer
add_executable(FuzzRoundtripBinary
  FuzzRoundtripBinary.cpp
)

target_link_libraries(FuzzRoundtripBinary
  thrift
  fuzztest_gen
)

# JSON protocol fuzzer
add_executable(FuzzParseJson
  FuzzParseJson.cpp
  ${FUZZ_COMMON_HEADERS}
)

target_link_libraries(FuzzParseJson
  thrift
  fuzztest_gen
)

# JSON protocol roundtrip fuzzer
add_executable(FuzzRoundtripJson
  FuzzRoundtripJson.cpp
  ${FUZZ_COMMON_HEADERS}
)

target_link_libraries(FuzzRoundtripJson
  thrift
  fuzztest_gen
)

# Create directories for fuzzing corpus
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/corpus/compact_protocol)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/corpus/binary_protocol)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/corpus/compact_protocol_roundtrip)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/corpus/binary_protocol_roundtrip)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/corpus/json_protocol)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/corpus/json_protocol_roundtrip)

# Add test targets that run the fuzzers briefly
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  add_test(NAME FuzzParseCompact_run
           COMMAND FuzzParseCompact -runs=100 ${CMAKE_CURRENT_BINARY_DIR}/corpus/compact_protocol
  )
  add_test(NAME FuzzRoundtripCompact_run
           COMMAND FuzzRoundtripCompact -runs=100 ${CMAKE_CURRENT_BINARY_DIR}/corpus/compact_protocol_roundtrip
  )
  add_test(NAME FuzzParseBinary_run
           COMMAND FuzzParseBinary -runs=100 ${CMAKE_CURRENT_BINARY_DIR}/corpus/binary_protocol
  )
  add_test(NAME FuzzRoundtripBinary_run
           COMMAND FuzzRoundtripBinary -runs=100 ${CMAKE_CURRENT_BINARY_DIR}/corpus/binary_protocol_roundtrip
  )
  add_test(NAME FuzzParseJson_run
           COMMAND FuzzParseJson -runs=100 ${CMAKE_CURRENT_BINARY_DIR}/corpus/json_protocol
  )
  add_test(NAME FuzzRoundtripJson_run
           COMMAND FuzzRoundtripJson -runs=100 ${CMAKE_CURRENT_BINARY_DIR}/corpus/json_protocol_roundtrip
  )
else()
  add_test(NAME FuzzParseCompact_run
           COMMAND FuzzParseCompact ${CMAKE_CURRENT_BINARY_DIR}/corpus/compact_protocol
  )
  add_test(NAME FuzzRoundtripCompact_run
           COMMAND FuzzRoundtripCompact ${CMAKE_CURRENT_BINARY_DIR}/corpus/compact_protocol_roundtrip
  )
  add_test(NAME FuzzParseBinary_run
           COMMAND FuzzParseBinary ${CMAKE_CURRENT_BINARY_DIR}/corpus/binary_protocol
  )
  add_test(NAME FuzzRoundtripBinary_run
           COMMAND FuzzRoundtripBinary ${CMAKE_CURRENT_BINARY_DIR}/corpus/binary_protocol_roundtrip
  )
  add_test(NAME FuzzParseJson_run
           COMMAND FuzzParseJson ${CMAKE_CURRENT_BINARY_DIR}/corpus/json_protocol
  )
  add_test(NAME FuzzRoundtripJson_run
           COMMAND FuzzRoundtripJson ${CMAKE_CURRENT_BINARY_DIR}/corpus/json_protocol_roundtrip
  )
endif() 