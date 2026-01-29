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

# The test executables still depend on Boost
include(BoostMacros)
REQUIRE_BOOST_HEADERS()
set(BOOST_COMPONENTS filesystem program_options random)
REQUIRE_BOOST_LIBRARIES(BOOST_COMPONENTS)

# Contains the thrift specific target_link_libraries
include(ThriftMacros)

find_package(OpenSSL REQUIRED)
include_directories(SYSTEM "${OPENSSL_INCLUDE_DIR}")

find_package(Libevent REQUIRED)  # Libevent comes with CMake support from upstream
include_directories(SYSTEM ${LIBEVENT_INCLUDE_DIRS})

find_package(ZLIB REQUIRED)
include_directories(SYSTEM ${ZLIB_INCLUDE_DIRS})

#Make sure gen-cpp files can be included
include_directories("${CMAKE_CURRENT_BINARY_DIR}")
include_directories("${CMAKE_CURRENT_BINARY_DIR}/gen-cpp")
include_directories("${PROJECT_SOURCE_DIR}/lib/cpp/src")

set(crosstestgencpp_SOURCES
    gen-cpp/SecondService.cpp
    gen-cpp/ThriftTest.cpp
    gen-cpp/ThriftTest_types.cpp
    gen-cpp/ThriftTest_constants.cpp
    src/ThriftTest_extras.cpp
)
add_library(crosstestgencpp STATIC ${crosstestgencpp_SOURCES})
target_link_libraries(crosstestgencpp thrift)

set(crossstressgencpp_SOURCES
    gen-cpp/Service.cpp
)
add_library(crossstressgencpp STATIC ${crossstressgencpp_SOURCES})
target_link_libraries(crossstressgencpp thrift)

set(crossspecificnamegencpp_SOURCES
    gen-cpp/EchoService.cpp
    gen-cpp/SpecificNameTest_types.cpp
)
add_library(crossspecificnamegencpp STATIC ${crossspecificnamegencpp_SOURCES})
target_link_libraries(crossspecificnamegencpp thrift)

add_executable(TestServer src/TestServer.cpp)
target_link_libraries(TestServer crosstestgencpp ${Boost_LIBRARIES})
target_link_libraries(TestServer thriftnb)
target_link_libraries(TestServer thriftz)

add_executable(TestClient src/TestClient.cpp)
target_link_libraries(TestClient crosstestgencpp ${Boost_LIBRARIES})
target_link_libraries(TestClient thriftnb)
target_link_libraries(TestClient thriftz)

add_executable(StressTest src/StressTest.cpp)
target_link_libraries(StressTest crossstressgencpp ${Boost_LIBRARIES})
target_link_libraries(StressTest thriftnb)
add_test(NAME StressTest COMMAND StressTest)
add_test(NAME StressTestConcurrent COMMAND StressTest --client-type=concurrent)

# As of https://jira.apache.org/jira/browse/THRIFT-4282, StressTestNonBlocking
# is broken on Windows. Contributions welcome.
if (NOT WIN32 AND NOT CYGWIN)
    add_executable(StressTestNonBlocking src/StressTestNonBlocking.cpp)
    target_link_libraries(StressTestNonBlocking crossstressgencpp ${Boost_LIBRARIES})
    target_link_libraries(StressTestNonBlocking thriftnb)
    target_link_libraries(StressTestNonBlocking thriftz)
    add_test(NAME StressTestNonBlocking COMMAND StressTestNonBlocking)
endif()

add_executable(SpecificNameTest src/SpecificNameTest.cpp)
target_link_libraries(SpecificNameTest crossspecificnamegencpp ${Boost_LIBRARIES} ${LIBEVENT_LIB})
target_link_libraries(SpecificNameTest thrift)
target_link_libraries(SpecificNameTest thriftnb)
add_test(NAME SpecificNameTest COMMAND SpecificNameTest)

# ForwardSetterTest - tests the forward_setter option
set(forwardsettertestgencpp_SOURCES
    gen-cpp-forward/gen-cpp/ThriftTest_types.cpp
    gen-cpp-forward/gen-cpp/ThriftTest_constants.cpp
    src/ThriftTest_extras.cpp
)
add_library(forwardsettertestgencpp STATIC ${forwardsettertestgencpp_SOURCES})
target_include_directories(forwardsettertestgencpp BEFORE PRIVATE 
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-forward"
    "${CMAKE_CURRENT_BINARY_DIR}"
    "${PROJECT_SOURCE_DIR}/lib/cpp/src"
)
target_link_libraries(forwardsettertestgencpp thrift)

add_executable(ForwardSetterTest src/ForwardSetterTest.cpp)
target_include_directories(ForwardSetterTest BEFORE PRIVATE 
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-forward/gen-cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-forward"
)
target_link_libraries(ForwardSetterTest forwardsettertestgencpp ${Boost_LIBRARIES})
target_link_libraries(ForwardSetterTest thrift)
add_test(NAME ForwardSetterTest COMMAND ForwardSetterTest)

# PrivateOptionalTest - tests the private_optional option
set(privateoptonaltestgencpp_SOURCES
    gen-cpp-private/gen-cpp/ThriftTest_types.cpp
    gen-cpp-private/gen-cpp/ThriftTest_constants.cpp
    src/ThriftTest_extras.cpp
)
add_library(privateoptonaltestgencpp STATIC ${privateoptonaltestgencpp_SOURCES})
target_include_directories(privateoptonaltestgencpp BEFORE PRIVATE 
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-private"
    "${CMAKE_CURRENT_BINARY_DIR}"
    "${PROJECT_SOURCE_DIR}/lib/cpp/src"
)
target_link_libraries(privateoptonaltestgencpp thrift)

add_executable(PrivateOptionalTest src/PrivateOptionalTest.cpp)
target_include_directories(PrivateOptionalTest BEFORE PRIVATE 
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-private/gen-cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-private"
)
target_link_libraries(PrivateOptionalTest privateoptonaltestgencpp ${Boost_LIBRARIES})
target_link_libraries(PrivateOptionalTest thrift)
add_test(NAME PrivateOptionalTest COMMAND PrivateOptionalTest)

# EnumClassTest - tests the pure_enums=enum_class option
set(enumclasstestgencpp_SOURCES
    gen-cpp-enumclass/gen-cpp/ThriftTest_types.cpp
    gen-cpp-enumclass/gen-cpp/ThriftTest_constants.cpp
    src/ThriftTest_extras.cpp
)
add_library(enumclasstestgencpp STATIC ${enumclasstestgencpp_SOURCES})
target_include_directories(enumclasstestgencpp BEFORE PRIVATE 
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-enumclass"
    "${CMAKE_CURRENT_BINARY_DIR}"
    "${PROJECT_SOURCE_DIR}/lib/cpp/src"
)
target_link_libraries(enumclasstestgencpp thrift)

add_executable(EnumClassTest src/EnumClassTest.cpp)
target_include_directories(EnumClassTest BEFORE PRIVATE 
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-enumclass/gen-cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-enumclass"
)
target_link_libraries(EnumClassTest enumclasstestgencpp ${Boost_LIBRARIES})
target_link_libraries(EnumClassTest thrift)
add_test(NAME EnumClassTest COMMAND EnumClassTest)

#
# Common thrift code generation rules
#

add_custom_command(OUTPUT gen-cpp/SecondService.cpp gen-cpp/SecondService.h gen-cpp/ThriftTest.cpp gen-cpp/ThriftTest.h gen-cpp/ThriftTest_types.cpp gen-cpp/ThriftTest_constants.cpp
    COMMAND ${THRIFT_COMPILER} --gen cpp:templates,cob_style -r ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)

# Generate ThriftTest with forward_setter option for ForwardSetterTest
add_custom_command(OUTPUT gen-cpp-forward/gen-cpp/ThriftTest_types.cpp gen-cpp-forward/gen-cpp/ThriftTest_types.h gen-cpp-forward/gen-cpp/ThriftTest_types.tcc gen-cpp-forward/gen-cpp/ThriftTest_constants.cpp
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-forward
    COMMAND ${THRIFT_COMPILER} --gen cpp:moveable_types=forward_setter -o gen-cpp-forward ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# Generate ThriftTest with private_optional option for PrivateOptionalTest
add_custom_command(OUTPUT gen-cpp-private/gen-cpp/ThriftTest_types.cpp gen-cpp-private/gen-cpp/ThriftTest_types.h gen-cpp-private/gen-cpp/ThriftTest_constants.cpp
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-private
    COMMAND ${THRIFT_COMPILER} --gen cpp:private_optional -o gen-cpp-private ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

# Generate ThriftTest with pure_enums=enum_class option for EnumClassTest
add_custom_command(OUTPUT gen-cpp-enumclass/gen-cpp/ThriftTest_types.cpp gen-cpp-enumclass/gen-cpp/ThriftTest_types.h gen-cpp-enumclass/gen-cpp/ThriftTest_constants.cpp
    COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_CURRENT_BINARY_DIR}/gen-cpp-enumclass
    COMMAND ${THRIFT_COMPILER} --gen cpp:pure_enums=enum_class -o gen-cpp-enumclass ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
)

add_custom_command(OUTPUT gen-cpp/Service.cpp
    COMMAND ${THRIFT_COMPILER} --gen cpp ${PROJECT_SOURCE_DIR}/test/StressTest.thrift
)

add_custom_command(OUTPUT gen-cpp/EchoService.cpp gen-cpp/SpecificNameTest_types.cpp
    COMMAND ${THRIFT_COMPILER} --gen cpp ${PROJECT_SOURCE_DIR}/test/SpecificNameTest.thrift
)
