#!/bin/sh

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

set -e

export GOPATH=$(mktemp -d -t gopath-XXXXXXXXXX)

go install go.uber.org/mock/mockgen@v0.6.0

gobin=$(go env GOBIN); [ -z "$gobin" ] && gobin=$(go env GOPATH)/bin
"$gobin/mockgen" -destination=src/common/mock_handler.go -package=common github.com/apache/thrift/test/go/src/gen/thrifttest ThriftTest

chmod -R a+w "$GOPATH" && rm -Rf "$GOPATH"
