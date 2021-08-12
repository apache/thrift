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

include_directories(${PYTHON_INCLUDE_DIRS})

add_custom_target(python_build ALL
    COMMAND ${THRIFT_COMPILER} --gen py test/test_thrift_file/TestServer.thrift
    COMMAND ${PYTHON_EXECUTABLE} setup.py build
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Building Python library"
)

if(BUILD_TESTING)
    add_test(PythonTestSSLSocket ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_sslsocket.py)
    add_test(PythonThriftJson ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/thrift_json.py)
    add_test(PythonThriftTransport ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/thrift_transport.py)
    add_test(PythonThriftTBinaryProtocol ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/thrift_TBinaryProtocol.py)
    add_test(PythonThriftTZlibTransport ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/thrift_TZlibTransport.py)
    add_test(PythonThriftProtocol ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/thrift_TCompactProtocol.py)
    add_test(PythonThriftTNonblockingServer ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/thrift_TNonblockingServer.py)
endif()
