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

THRIFT = ../../compiler/cpp/thrift
THRIFT_TUTORIAL = ../tutorial.thrift multiplexing.thrift
THRIFT_TUTORIAL_SRC = gen-cocoa/tutorial.m gen-cocoa/shared.m gen-cocoa/multiplexing.m
COCOA_BUILD_CONFIG = Release

all: $(TUTORIAL_SRC) CocoaServer CocoaClient

$(THRIFT_TUTORIAL_SRC): $(THRIFT_TUTORIAL)
	@for file in $? ; do \
		$(THRIFT) --gen cocoa -r $$file ; \
	done

CocoaClient CocoaServer CocoaMultiplexedClient: $(THRIFT_TUTORIAL_SRC)
	@xcodebuild -target $@ -configuration $(COCOA_BUILD_CONFIG)

tutorialserver: CocoaServer
	./build/$(COCOA_BUILD_CONFIG)/$<

tutorialclient: CocoaClient
	./build/$(COCOA_BUILD_CONFIG)/$<

tutorialclient_multiplexed: CocoaMultiplexedClient
	./build/$(COCOA_BUILD_CONFIG)/$<

clean:
	@$(RM) -r gen-*
	@$(RM) -r build
