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

if(IS_ABSOLUTE "${LIB_INSTALL_DIR}")
    set(JAVA_INSTALL_DIR "${LIB_INSTALL_DIR}/java")
else()
    set(JAVA_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${LIB_INSTALL_DIR}/java")
endif()

if(IS_ABSOLUTE "${DOC_INSTALL_DIR}")
    set(JAVA_DOC_INSTALL_DIR "${DOC_INSTALL_DIR}/java")
else()
    set(JAVA_DOC_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${DOC_INSTALL_DIR}/java")
endif()

add_custom_target(ant_build ALL
    COMMAND ${Ant_EXECUTABLE} ${ANT_FLAGS} -Dbuild.dir=${CMAKE_CURRENT_BINARY_DIR} -Dworking.dir=${work_dir} -f build.xml
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Java library using Ant"
)

# Hook the ant install task into CMake install
install(CODE "execute_process(
    COMMAND ${Ant_EXECUTABLE} ${ANT_FLAGS} install
    -Dbuild.dir=${CMAKE_CURRENT_BINARY_DIR} -Dworking.dir=${work_dir}
    -Dinstall.path=${JAVA_INSTALL_DIR} -Dinstall.javadoc.path=${JAVA_DOC_INSTALL_DIR} -f build.xml
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)")

if(BUILD_TESTING)
    #add_subdirectory(test)
endif()
