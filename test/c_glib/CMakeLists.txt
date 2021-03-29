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

# Contains the thrift specific LINK_AGAINST_THRIFT_LIBRARY
include(ThriftMacros)

find_package(GLIB REQUIRED COMPONENTS gobject)
include_directories(SYSTEM "${GLIB_INCLUDE_DIR}")
include_directories(SYSTEM "${GLIBCONFIG_INCLUDE_DIR}")

#Make sure gen-c_glib files can be included
include_directories("${CMAKE_CURRENT_BINARY_DIR}")
include_directories("${CMAKE_CURRENT_BINARY_DIR}/gen-c_glib")
include_directories("${PROJECT_SOURCE_DIR}/lib/c_glib/src")
include_directories(SYSTEM "${OPENSSL_INCLUDE_DIR}")

set(crosstestgencglib_SOURCES
	gen-c_glib/t_test_second_service.c
	gen-c_glib/t_test_second_service.h
	gen-c_glib/t_test_thrift_test.c
	gen-c_glib/t_test_thrift_test.h
	gen-c_glib/t_test_thrift_test_types.c
	gen-c_glib/t_test_thrift_test_types.h
)
add_library(crosstestgencglib STATIC ${crosstestgencglib_SOURCES})
LINK_AGAINST_THRIFT_LIBRARY(crosstestgencglib thrift_c_glib)

add_executable(test_server src/test_server.c src/thrift_test_handler.c src/thrift_second_service_handler.c)
target_link_libraries(test_server crosstestgencglib)

add_executable(test_client src/test_client.c)
target_link_libraries(test_client crosstestgencglib "${OPENSSL_LIBRARIES}")

#
# Common thrift code generation rules
#

add_custom_command(OUTPUT gen-c_glib/t_test_second_service.c  gen-c_glib/t_test_second_service.h  gen-c_glib/t_test_thrift_test.c  gen-c_glib/t_test_thrift_test.h  gen-c_glib/t_test_thrift_test_types.c  gen-c_glib/t_test_thrift_test_types.h
    COMMAND ${THRIFT_COMPILER} --gen c_glib -r ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)
