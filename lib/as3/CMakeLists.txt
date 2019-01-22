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

if (IS_ABSOLUTE "${LIB_INSTALL_DIR}")
    set(AS3_INSTALL_DIR "${LIB_INSTALL_DIR}/as3")
else ()
    set(AS3_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${LIB_INSTALL_DIR}/as3")
endif ()

set(PRELEASE "true")
if (CMAKE_BUILD_TYPE MATCHES DEBUG)
    set(PRELEASE "false")
endif ()

add_custom_target(ThriftAs3 ALL
    COMMENT "Building as3 library using Gradle Wrapper"
    COMMAND ${GRADLEW_EXECUTABLE} ${GRADLE_OPTS} compileFlex
        --console=plain --no-daemon
        -Prelease=${PRELEASE}
        "-Pbuild.dir=${CMAKE_CURRENT_BINARY_DIR}/build"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

# Enable publishing from CMake if the publishing information is provided
if (NOT CMAKE_BUILD_TYPE MATCHES DEBUG)
    add_custom_target(MavenPublishAs3
        COMMENT "Publishing as3 library to Apache Maven staging"
        COMMAND ${GRADLEW_EXECUTABLE} ${GRADLE_OPTS} clean publishMavenPublicationToMavenRepository
            --console=plain --no-daemon
            -Prelease=${PRELEASE}
            -Psign=${PRELEASE}
            "-Pbuild.dir=${CMAKE_CURRENT_BINARY_DIR}/build"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif ()

# Hook the CMake install process to the results from make ALL.
# This works best when 'make all && sudo make install/fast' is used.
# Using slash to end the source location to avoid copying the directory path.
install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/build/libs/
        DESTINATION ${AS3_INSTALL_DIR}
        FILES_MATCHING PATTERN "libthrift-as3.swc")

if (BUILD_TESTING)
    add_test(NAME As3Test
            COMMAND ${GRADLEW_EXECUTABLE} ${GRADLE_OPTS} test
                --console=plain --no-daemon
                -Prelease=${PRELEASE}
                "-Pbuild.dir=${CMAKE_CURRENT_BINARY_DIR}/build"
                "-Pthrift.compiler=${THRIFT_COMPILER}"
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif ()
