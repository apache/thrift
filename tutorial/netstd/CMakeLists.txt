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

# Track the list of sources, to have cmake infer when rebuild is needed:
file(GLOB_RECURSE TUTORIALNETSTD_SOURCES LIST_DIRECTORIES false
    "${CMAKE_CURRENT_SOURCE_DIR}/Interfaces/*")
list(FILTER TUTORIALNETSTD_SOURCES EXCLUDE REGEX ".*/(bin|obj)/.*")

# Build the solution into ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE}/bin:
add_custom_command(OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE}/bin/Server"
    BYPRODUCTS "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE}/bin/Client"
    COMMENT "Building Thrift NetSTD Tutorial with dotnet"
    DEPENDS ThriftNetSTD ${TUTORIALNETSTD_SOURCES}
    COMMAND "${DOTNET_EXE}" build "Tutorial.sln"
        --nologo --configuration "${CMAKE_BUILD_TYPE}"
        --output "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE}/bin"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}")

add_custom_target(TutorialNetSTD ALL
    DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_BUILD_TYPE}/bin/Server")
