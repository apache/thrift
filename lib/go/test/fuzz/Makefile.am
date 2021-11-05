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

gopathfuzz: $(THRIFT) fuzz.go
	$(THRIFT) -r --gen go:thrift_import=github.com/apache/thrift/lib/go/thrift$(COMPILER_EXTRAFLAG) ../../../../tutorial/tutorial.thrift
	cd gen-go/shared && go mod init shared
	cd gen-go/tutorial && go mod init tutorial
	touch gopathfuzz

check: gopathfuzz
	go test -mod=mod -tags gofuzz

clean-local:
	$(RM) -r gopathfuzz gen-go

