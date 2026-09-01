#!/bin/sh

set -e

export GOPATH=$(mktemp -d -t gopath-XXXXXXXXXX)

go install go.uber.org/mock/mockgen@v0.6.0

gobin=$(go env GOBIN); [ -z "$gobin" ] && gobin=$(go env GOPATH)/bin
"$gobin/mockgen" -destination=src/common/mock_handler.go -package=common github.com/apache/thrift/test/go/src/gen/thrifttest ThriftTest

chmod a+w -R $GOPATH && rm -Rf $GOPATH
