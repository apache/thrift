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

THRIFTCMD = $(THRIFT) --gen haxe -r
THRIFTTEST = $(top_srcdir)/test/ThriftTest.thrift

BIN_CPP = bin/Main-debug
BIN_PHP = bin/php/Main-debug.php
BIN_PHP_WEB = bin/php-web-server/Main-debug.php
BIN_NODE_JS = bin/nodejs/Main.js
BIN_JS = bin/js/Main.js

gen-haxe/thrift/test/ThriftTest.hx: $(THRIFTTEST)
	$(THRIFTCMD) $(THRIFTTEST)

all-local: $(BIN_CPP) $(BIN_PHP) $(BIN_PHP_WEB)

HX_SRC = \
		src/*.hx \
		../../lib/haxe/src/org/apache/thrift/**/*.hx \
		gen-haxe/thrift/test/ThriftTest.hx

$(BIN_CPP): $(HX_SRC) \
		cpp.hxml
	$(HAXE) --cwd .  cpp.hxml

$(BIN_PHP): $(HX_SRC) \
		php.hxml
	$(HAXE) --cwd .  php.hxml

$(BIN_PHP_WEB): $(HX_SRC) \
		php-web-server.hxml
	$(HAXE) --cwd .  php-web-server.hxml

$(BIN_NODE_JS): $(HX_SRC) \
		nodejs.hxml
	haxelib list | grep hxnodejs > /dev/null || haxelib install hxnodejs
	$(HAXE) --cwd .  nodejs.hxml

$(BIN_JS): $(HX_SRC) \
		javascript.hxml
	$(HAXE) --cwd .  javascript.hxml

PHANTOMJS_BIN = node_modules/.bin/phantomjs

$(PHANTOMJS_BIN):
	npm install 
	

#TODO: other haxe targets
#    $(HAXE)  --cwd .  csharp
#    $(HAXE)  --cwd .  flash
#    $(HAXE)  --cwd .  java
#    $(HAXE)  --cwd .  javascript
#    $(HAXE)  --cwd .  neko
#    $(HAXE)  --cwd .  python  # needs Haxe 3.2.0


clean-local:
	$(RM) -r gen-haxe bin node_modules

.NOTPARALLEL:

check: check_cpp \
	check_php \
	check_php_web \
	check_node_web \
	check_js_web

check_cpp: $(BIN_CPP) 
	timeout 20 $(BIN_CPP) server & echo $$! > server.PID
	sleep 1
	$(BIN_CPP) client
	kill `cat server.PID` && rm server.PID

check_php: $(BIN_PHP) 
	timeout 20 php -f $(BIN_PHP) server & echo $$! > server.PID
	sleep 1
	php -f $(BIN_PHP) client
	kill `cat server.PID` && rm server.PID

check_php_web: $(BIN_PHP_WEB) $(BIN_CPP)
	timeout 20 php -S 127.0.0.1:9090 router.php & echo $$! > server.PID
	sleep 1
	$(BIN_CPP) client --transport http
	kill `cat server.PID` && rm server.PID

check_node_web: $(BIN_NODE_JS)
	timeout 20 php -S 127.0.0.1:9090 router.php & echo $$! > server.PID
	sleep 1
	node $(BIN_NODE_JS) client --transport http --skip-speed-test
	kill `cat server.PID` && rm server.PID

check_js_web: $(BIN_JS) $(PHANTOMJS_BIN)
	timeout 20 php -S 127.0.0.1:9090 router.php & echo $$! > server.PID
	sleep 1
	$(PHANTOMJS_BIN) phantom-js-client-test.js
	kill `cat server.PID` && rm server.PID

nodejs_client_http: $(BIN_NODE_JS)
	node $(BIN_NODE_JS) client --transport http --skip-speed-test

js_client_http: $(BIN_JS) $(PHANTOMJS_BIN)
	$(PHANTOMJS_BIN) phantom-js-client-test.js

php_client_http: $(BIN_PHP)
	php -f $(BIN_PHP) client --transport http

cpp_client_http: $(BIN_CPP)
	$(BIN_CPP) client --transport http

php_web_server: $(BIN_PHP_WEB) 
	php -S 127.0.0.1:9090 router.php 


EXTRA_DIST = \
             src \
             cpp.hxml \
             csharp.hxml \
             flash.hxml \
             java.hxml \
             javascript.hxml \
             nodejs.hxml \
             neko.hxml \
             php.hxml \
             python.hxml \
             project.hide \
             TestClientServer.hxproj \
             make_all.bat \
             make_all.sh
