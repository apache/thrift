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


THRIFT = $(top_srcdir)/compiler/cpp/thrift

stubs: ../ThriftTest.thrift
	$(THRIFT) --gen js:node ../ThriftTest.thrift

check: stubs
	@if which nodeunit &> /dev/null ; then \
		echo "   Testing thrift/binary"; \
		NODE_PATH=../../lib/nodejs/lib:../../lib/nodejs/lib/thrift:$(NODE_PATH) nodeunit ../../lib/nodejs/test/binary.test.js; \
	fi
	@if which node &> /dev/null ; then \
		echo "   Testing Client/Server"; \
		timeout -s14 5 $(MAKE) server & \
		sleep 1; $(MAKE) client; sleep 2; \
	fi

clean-local:
	$(RM) -r gen-nodejs

server:
	NODE_PATH=../../lib/nodejs/lib:../../lib/nodejs/lib/thrift:$(NODE_PATH) node server.js

client:
	NODE_PATH=../../lib/nodejs/lib:../../lib/nodejs/lib/thrift:$(NODE_PATH) node client.js
