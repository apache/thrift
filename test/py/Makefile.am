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
.NOTPARALLEL:
THRIFT = $(top_srcdir)/compiler/cpp/thrift

py_unit_tests = RunClientServer.py

thrift_gen =                                    \
        gen-py/ThriftTest/__init__.py           \
        gen-py/DebugProtoTest/__init__.py \
        gen-py-default/ThriftTest/__init__.py           \
        gen-py-default/DebugProtoTest/__init__.py \
        gen-py-slots/ThriftTest/__init__.py           \
        gen-py-slots/DebugProtoTest/__init__.py \
        gen-py-newstyle/ThriftTest/__init__.py           \
        gen-py-newstyle/DebugProtoTest/__init__.py \
        gen-py-newstyleslots/ThriftTest/__init__.py           \
        gen-py-newstyleslots/DebugProtoTest/__init__.py \
        gen-py-dynamic/ThriftTest/__init__.py           \
        gen-py-dynamic/DebugProtoTest/__init__.py \
        gen-py-dynamicslots/ThriftTest/__init__.py           \
        gen-py-dynamicslots/DebugProtoTest/__init__.py

helper_scripts=                                 \
        TestClient.py                           \
        TestServer.py

check_SCRIPTS=                                  \
        $(thrift_gen) \
        $(py_unit_tests)                        \
        $(helper_scripts)

TESTS= $(py_unit_tests)


gen-py/%/__init__.py: ../%.thrift
	$(THRIFT) --gen py  $<
	test -d gen-py-default || mkdir gen-py-default
	$(THRIFT) --gen py -out gen-py-default $<

gen-py-slots/%/__init__.py: ../%.thrift
	test -d gen-py-slots || mkdir gen-py-slots
	$(THRIFT) --gen py:slots -out gen-py-slots $<

gen-py-newstyle/%/__init__.py: ../%.thrift
	test -d gen-py-newstyle || mkdir gen-py-newstyle
	$(THRIFT) --gen py:new_style -out gen-py-newstyle $<

gen-py-newstyleslots/%/__init__.py: ../%.thrift
	test -d gen-py-newstyleslots || mkdir gen-py-newstyleslots
	$(THRIFT) --gen py:new_style,slots -out gen-py-newstyleslots $<

gen-py-dynamic/%/__init__.py: ../%.thrift
	test -d gen-py-dynamic || mkdir gen-py-dynamic
	$(THRIFT) --gen py:dynamic -out gen-py-dynamic $<

gen-py-dynamicslots/%/__init__.py: ../%.thrift
	test -d gen-py-dynamicslots || mkdir gen-py-dynamicslots
	$(THRIFT) --gen py:dynamic,slots -out gen-py-dynamicslots $<

clean-local:
	$(RM) -r gen-py gen-py-slots gen-py-default gen-py-newstyle gen-py-newstyleslots gen-py-dynamic gen-py-dynamicslots
