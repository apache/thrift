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

if(NOT THRIFTMAVENPLUGIN_INSTALL_DIR)
    if(IS_ABSOLUTE "${CONTRIB_INSTALL_DIR}")
        set(THRIFTMAVENPLUGIN_INSTALL_DIR "${CONTRIB_INSTALL_DIR}/thrift-maven-plugin")
    else()
        set(THRIFTMAVENPLUGIN_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${CONTRIB_INSTALL_DIR}/thrift-maven-plugin")
    endif()
endif()

set(PRELEASE "true")
if (CMAKE_BUILD_TYPE MATCHES DEBUG)
    set(PRELEASE "false")
endif ()

set(MAVEN_OUTPUT_JAR "${CMAKE_CURRENT_SOURCE_DIR}/target/thrift-maven-plugin-${thrift_VERSION}.jar")

file(GLOB_RECURSE THRIFTMAVENPLUGIN_SOURCES LIST_DIRECTORIES false
    "${CMAKE_CURRENT_SOURCE_DIR}/src/*"
    "${CMAKE_CURRENT_SOURCE_DIR}/pom.xml")

add_custom_command(OUTPUT "${MAVEN_OUTPUT_JAR}"
    COMMENT "Building Maven plugin using Maven"
    COMMAND ${MAVEN_EXECUTABLE} clean package
        -B -DskipTests
        -Drelease=${PRELEASE}
        -Dthrift.version=${thrift_VERSION}
    DEPENDS ${THRIFTMAVENPLUGIN_SOURCES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

add_custom_target(ThriftMavenPlugin ALL DEPENDS "${MAVEN_OUTPUT_JAR}")

# Enable publishing from CMake if the publishing information is provided
add_custom_target(ThriftMavenPluginMavenPublish
    COMMENT "Publishing Maven plugin to Apache Maven staging"
    DEPENDS ThriftMavenPlugin
    COMMAND ${MAVEN_EXECUTABLE} deploy
        -B -DskipTests
        -Drelease=${PRELEASE}
        -Dthrift.version=${thrift_VERSION}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# Hook the CMake install process to the results from make ALL.
# This works best when 'make all && sudo make install/fast' is used.
install(FILES "${MAVEN_OUTPUT_JAR}" DESTINATION ${THRIFTMAVENPLUGIN_INSTALL_DIR})

if(BUILD_TESTING)
    add_test(NAME ThriftMavenPluginTest
        COMMAND ${MAVEN_EXECUTABLE} test
            -B
            -Drelease=${PRELEASE}
            -Dthrift.version=${thrift_VERSION}
            -Dthrift.compiler=${THRIFT_COMPILER}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()