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

add_test(NAME python_test
    COMMAND ${PYTHON_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/RunClientServer.py
    DEPENDS gen-py-default/ThriftTest/ThriftTest.py
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

add_custom_command(OUTPUT gen-py-default/ThriftTest/ThriftTest.py
    COMMAND thrift-compiler --gen gen-py-default -out gen-py-default ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)

add_custom_command(OUTPUT gen-py-slots/ThriftTest/ThriftTest.py
    COMMAND thrift-compiler --gen py:slots -out gen-py-slots ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)

add_custom_command(OUTPUT gen-py-newstyle/ThriftTest/ThriftTest.py
    COMMAND thrift-compiler --gen py:new_style -out gen-py-newstyle ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)

add_custom_command(OUTPUT gen-py-newstyleslots/ThriftTest/ThriftTest.py
    COMMAND thrift-compiler --gen py:new_style,slots -out gen-py-newstyleslots ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)

add_custom_command(OUTPUT gen-py-dynamic/ThriftTest/ThriftTest.py
    COMMAND thrift-compiler --gen py:dynamic -out gen-py-dynamic ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)

add_custom_command(OUTPUT gen-py-dynamicslots/ThriftTest/ThriftTest.py
    COMMAND thrift-compiler --gen py:dynamic,slots -out gen-py-dynamicslots ${PROJECT_SOURCE_DIR}/test/ThriftTest.thrift
)
