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

cmake_minimum_required(VERSION 3.4)

set(MANIFEST "${CMAKE_CURRENT_BINARY_DIR}/install_manifest.txt")

if(NOT EXISTS ${MANIFEST})
    message(FATAL_ERROR "Cannot find install mainfest: ${MANIFEST}")
endif()

file(STRINGS ${MANIFEST} files)
foreach(file ${files})
    if(EXISTS ${file} OR IS_SYMLINK ${file})
        message(STATUS "Removing: ${file}")

	execute_process(
	    COMMAND ${CMAKE_COMMAND} -E remove  ${file}
	    RESULT_VARIABLE result
	    OUTPUT_QUIET
	    ERROR_VARIABLE stderr
	    ERROR_STRIP_TRAILING_WHITESPACE
        )

        if(NOT ${result} EQUAL 0)
            message(FATAL_ERROR "${stderr}")
        endif()
    else()
        message(STATUS "Does-not-exist: ${file}")
    endif()
endforeach(file)
