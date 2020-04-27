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

if(NOT JAVASCRIPT_INSTALL_DIR)
    if(IS_ABSOLUTE "${LIB_INSTALL_DIR}")
        set(JAVASCRIPT_INSTALL_DIR "${LIB_INSTALL_DIR}/js")
    else()
        set(JAVASCRIPT_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${LIB_INSTALL_DIR}/js")
    endif()
endif()

if(IS_ABSOLUTE "${DOC_INSTALL_DIR}")
    set(JAVASCRIPT_DOC_INSTALL_DIR "${DOC_INSTALL_DIR}/js")
else()
    set(JAVASCRIPT_DOC_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${DOC_INSTALL_DIR}/js")
endif()

add_custom_target(ThriftJavascriptPreDeps ALL
    COMMENT "Installing Javascript dependencies using npm"
    DEPENDS copy-thrift-compiler
    COMMAND npm install
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
)

add_custom_target(ThriftJavascript ALL
    COMMENT "Building Javascript library using npx Grunt wrapper"
    DEPENDS ThriftJavascriptPreDeps
    COMMAND npx grunt
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
)

install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/dist/"
        DESTINATION "${JAVASCRIPT_INSTALL_DIR}"
        FILES_MATCHING PATTERN "thrift*.js")
install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/doc/"
        DESTINATION "${JAVASCRIPT_DOC_INSTALL_DIR}")
