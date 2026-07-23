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

file(GLOB KEYWORD_SAMPLES "${CMAKE_CURRENT_SOURCE_DIR}/keyword-samples/*.thrift")
set(KEYWORD_LANGS php)  # the whole ticket THRIFT-4655 is only about PHP so leave other languages alone
foreach(LANG ${KEYWORD_LANGS})
    foreach(SAMPLE ${KEYWORD_SAMPLES})
        get_filename_component(FILENAME ${SAMPLE} NAME_WE)
        add_test(NAME "${LANG}_${FILENAME}"
            COMMAND thrift-compiler --gen ${LANG} ${SAMPLE})
        set_tests_properties("${LANG}_${FILENAME}" PROPERTIES
            PASS_REGULAR_EXPRESSION "Cannot use reserved language keyword")
    endforeach()
endforeach()

# THRIFT-6062: the Smalltalk generator inline-expands nested struct
# serialization into the service send/recv methods. With no bound and no
# per-struct method to call, a recursive type made it recurse until the C++
# stack overflowed, so "thrift --gen st" on a recursive IDL died with SIGSEGV.
#
# No PASS_REGULAR_EXPRESSION here on purpose: the test passes only on a zero
# exit, and a signal is not one.
add_test(NAME st_recursive_types
    COMMAND thrift-compiler --gen st -out "${CMAKE_CURRENT_BINARY_DIR}"
            "${CMAKE_CURRENT_SOURCE_DIR}/../../../test/Recursive.thrift")

find_package(Python3 COMPONENTS Interpreter QUIET)
if(Python3_Interpreter_FOUND)
  add_test(NAME StalenessCheckTest COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/compiler/staleness_check.py ${THRIFT_COMPILER})
  add_test(NAME MarkdownDocTest COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/compiler/markdown_doc_test.py ${THRIFT_COMPILER})
  add_test(NAME ExceptionDocTest COMMAND Python3::Interpreter ${CMAKE_CURRENT_SOURCE_DIR}/compiler/exception_doc_test.py ${THRIFT_COMPILER})
else()
  message(WARNING "Skipping Python compiler tests as there is no python interpreter available.")
endif()
