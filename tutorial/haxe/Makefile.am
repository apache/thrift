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

gen-haxe/tutorial/calculator.hx gen-haxe/shared/shared_service.hx: $(top_srcdir)/tutorial/tutorial.thrift
	$(THRIFT) --gen haxe -r $<

all-local: bin/Main-debug

check: gen-haxe/tutorial/calculator.hx

bin/Main-debug: \
		src/*.hx \
		../../lib/haxe/src/org/apache/thrift/**/*.hx \
		gen-haxe/tutorial/calculator.hx
	$(HAXE) --cwd .  cpp.hxml

tutorialserver: all
	bin/Main-debug server

tutorialclient: all
	bin/Main-debug

tutorialsecureserver: all
	bin/Main-debug server secure

tutorialsecureclient: all
	bin/Main-debug secure

clean-local:
	$(RM) -r gen-haxe bin

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
             Tutorial.hxproj \
             make_all.bat \
             make_all.sh
