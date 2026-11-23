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

macro(GENERATE FILENAME GENERATOR OUTPUTDIR)
  file(MAKE_DIRECTORY ${MY_CURRENT_BINARY_DIR}/${OUTPUTDIR})
  execute_process(COMMAND ${THRIFTCOMPILER} --gen ${GENERATOR} -out ${MY_CURRENT_BINARY_DIR}/${OUTPUTDIR} ${FILENAME}
                  RESULT_VARIABLE CMD_RESULT)
  if(CMD_RESULT)
        message(FATAL_ERROR "Error generating ${FILENAME} with generator ${GENERATOR}")
  endif()
endmacro(GENERATE)

generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py gen-py-default)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:slots gen-py-slots)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:old_style gen-py-oldstyle)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:no_utf8strings gen-py-no_utf8strings)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:dynamic gen-py-dynamic)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:dynamic,slots gen-py-dynamicslots)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:enum gen-py-enum)
generate(${MY_PROJECT_DIR}/test/ThriftTest.thrift py:type_hints,enum gen-py-type_hints)

generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py gen-py-default)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:slots gen-py-slots)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:old_style gen-py-oldstyle)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:no_utf8strings gen-py-no_utf8strings)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:dynamic gen-py-dynamic)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:dynamic,slots gen-py-dynamicslots)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:enum gen-py-enum)
generate(${MY_PROJECT_DIR}/test/DebugProtoTest.thrift py:type_hints,enum gen-py-type_hints)

generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py gen-py-default)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:slots gen-py-slots)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:old_style gen-py-oldstyle)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:no_utf8strings gen-py-no_utf8strings)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:dynamic gen-py-dynamic)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:dynamic,slots gen-py-dynamicslots)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:enum gen-py-enum)
generate(${MY_PROJECT_DIR}/test/DoubleConstantsTest.thrift py:type_hints,enum gen-py-type_hints)

generate(${MY_PROJECT_DIR}/test/Recursive.thrift py gen-py-default)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:slots gen-py-slots)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:old_style gen-py-oldstyle)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:no_utf8strings gen-py-no_utf8strings)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:dynamic gen-py-dynamic)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:dynamic,slots gen-py-dynamicslots)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:enum gen-py-enum)
generate(${MY_PROJECT_DIR}/test/Recursive.thrift py:type_hints,enum gen-py-type_hints)
