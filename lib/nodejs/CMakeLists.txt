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

if(NOT NODEJS_INSTALL_DIR)
    if(IS_ABSOLUTE "${LIB_INSTALL_DIR}")
        set(NODEJS_INSTALL_DIR "${LIB_INSTALL_DIR}/nodejs")
    else()
        set(NODEJS_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${LIB_INSTALL_DIR}/nodejs")
    endif()
endif()

# Currently no doc
#if(IS_ABSOLUTE "${DOC_INSTALL_DIR}")
#    set(NODEJS_DOC_INSTALL_DIR "${DOC_INSTALL_DIR}/nodejs")
#else()
#    set(NODEJS_DOC_INSTALL_DIR "${CMAKE_INSTALL_PREFIX}/${DOC_INSTALL_DIR}/nodejs")
#endif()

add_custom_target(ThriftNodeJS ALL
    COMMENT "Installing NodeJS dependencies with npm"
    COMMAND npm install
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../../"
)

install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/lib/"
        DESTINATION "${NODEJS_INSTALL_DIR}")
#install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/doc/"
#        DESTINATION "${NODEJS_DOC_INSTALL_DIR}")
