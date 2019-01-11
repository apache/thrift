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

macro(ADD_LIBRARY_THRIFT name)
    add_library(${name} ${ARGN})
    set_target_properties(${name} PROPERTIES
        OUTPUT_NAME ${name}${THRIFT_RUNTIME_POSTFIX}   # windows link variants (/MT, /MD, /MTd, /MDd) get different names
        VERSION ${thrift_VERSION} )
    # set_target_properties(${name} PROPERTIES PUBLIC_HEADER "${thriftcpp_HEADERS}")
    install(TARGETS ${name}
        RUNTIME DESTINATION "${BIN_INSTALL_DIR}"
        LIBRARY DESTINATION "${LIB_INSTALL_DIR}"
        ARCHIVE DESTINATION "${LIB_INSTALL_DIR}"
        PUBLIC_HEADER DESTINATION "${INCLUDE_INSTALL_DIR}")
endmacro()

macro(TARGET_INCLUDE_DIRECTORIES_THRIFT name)
    target_include_directories(${name} ${ARGN})
endmacro()

macro(TARGET_LINK_LIBRARIES_THRIFT name)
    target_link_libraries(${name} ${ARGN})
endmacro()

macro(LINK_AGAINST_THRIFT_LIBRARY target libname)
    target_link_libraries(${target} ${libname})
endmacro()

macro(TARGET_LINK_LIBRARIES_THRIFT_AGAINST_THRIFT_LIBRARY target libname)
    target_link_libraries(${target} ${ARGN} ${libname})
endmacro()
