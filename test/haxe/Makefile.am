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

THRIFT = $(top_builddir)/compiler/cpp/thrift
THRIFTCMD = $(THRIFT) --gen haxe -r
THRIFTTEST = $(top_srcdir)/test/ThriftTest.thrift

BIN_CPP = bin/Main-debug

gen-haxe/thrift/test/ThriftTest.hx: $(THRIFTTEST)
	$(THRIFTCMD) $(THRIFTTEST)

all-local: $(BIN_CPP)

$(BIN_CPP): \
		src/*.hx \
		../../lib/haxe/src/org/apache/thrift/**/*.hx \
		gen-haxe/thrift/test/ThriftTest.hx
	$(HAXE) --cwd .  cpp.hxml


#TODO: other haxe targets
#    $(HAXE)  --cwd .  csharp
#    $(HAXE)  --cwd .  flash
#    $(HAXE)  --cwd .  java
#    $(HAXE)  --cwd .  javascript
#    $(HAXE)  --cwd .  neko
#    $(HAXE)  --cwd .  php
#    $(HAXE)  --cwd .  python  # needs Haxe 3.2.0


clean-local:
	$(RM) -r gen-haxe bin

check: $(BIN_CPP)
	timeout 10 $(BIN_CPP) server &
	sleep 1
	$(BIN_CPP) client
	sleep 10

EXTRA_DIST = \
             src \
             cpp.hxml \
             csharp.hxml \
             flash.hxml \
             java.hxml \
             javascript.hxml \
             neko.hxml \
             php.hxml \
             python.hxml \
             project.hide \
             TestClientServer.hxproj \
             make_all.bat \
             make_all.sh
