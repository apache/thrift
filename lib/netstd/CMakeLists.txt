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

find_program(DOTNET_EXE dotnet REQUIRED)

# Allow users to override the installation directory:
if(NOT NETSTD_INSTALL_DIR)
    if(IS_ABSOLUTE "${BIN_INSTALL_DIR}")
        set(NETSTD_INSTALL_DIR "${BIN_INSTALL_DIR}")
    else()
        set(NETSTD_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${BIN_INSTALL_DIR}")
    endif()
endif()

# Track the list of sources, to have cmake infer when rebuild is needed:
file(GLOB_RECURSE THRIFTNETSTD_SOURCES LIST_DIRECTORIES false
    "${CMAKE_CURRENT_SOURCE_DIR}/*")
list(FILTER THRIFTNETSTD_SOURCES EXCLUDE REGEX ".*/(bin|obj)/.*")

# Publish for the latest .NET framework into ${CMAKE_CURRENT_BINARY_DIR}/Thrift/bin:
add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/Thrift/bin/Thrift.dll"
    COMMENT "Building Thrift NetSTD with dotnet"
    DEPENDS copy-thrift-compiler ${THRIFTNETSTD_SOURCES}
    COMMAND "${DOTNET_EXE}" publish "Thrift.csproj"
        --nologo --configuration "${CMAKE_BUILD_TYPE}"
        --output "${CMAKE_CURRENT_BINARY_DIR}/Thrift/bin"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/Thrift")

add_custom_target(ThriftNetSTD ALL
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/Thrift/bin/Thrift.dll")

# Install binaries:
install(DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/Thrift/bin/"
    DESTINATION "${NETSTD_INSTALL_DIR}")


# Execute the tests:
if(BUILD_TESTING)
    add_test(NAME ThriftNetSTDTests
        COMMAND dotnet test "Thrift.sln"
            --nologo --no-build --no-restore --configuration "${CMAKE_BUILD_TYPE}"
            --output "${CMAKE_CURRENT_BINARY_DIR}/Thrift/bin"
            --results-directory "${CMAKE_CURRENT_BINARY_DIR}/Tests/Thrift.Tests/results"
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")
endif()
