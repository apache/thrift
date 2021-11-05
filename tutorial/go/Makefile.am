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

gen-go/tutorial/calculator.go gen-go/shared/shared_service.go: $(top_srcdir)/tutorial/tutorial.thrift
	$(THRIFT) --gen go:thrift_import=github.com/apache/thrift/lib/go/thrift,package_prefix=github.com/apache/thrift/tutorial/go/gen-go/$(COMPILER_EXTRAFLAG) -r $<

all-local: gen-go/tutorial/calculator.go

check: thirdparty-dep all
	$(GO) build -mod=mod -o go-tutorial ./src
	$(GO) build -mod=mod -o calculator-remote ./gen-go/tutorial/calculator-remote/calculator-remote.go

thirdparty-dep:

tutorialserver: all
	$(GO) run -mod=mod src/*.go -server=true

tutorialclient: all
	$(GO) run -mod=mod src/*.go

tutorialsecureserver: all
	$(GO) run -mod=mod src/*.go -server=true -secure=true

tutorialsecureclient: all
	$(GO) run -mod=mod src/*.go -secure=true

clean-local:
	$(RM) -r gen-* go-tutorial calculator-remote

EXTRA_DIST = \
	src/client.go \
	src/handler.go \
	src/server.go \
	src/main.go \
	server.crt \
	server.key

