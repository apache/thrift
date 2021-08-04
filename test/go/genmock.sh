#!/bin/sh

set -e

export GOPATH=$(mktemp -d -t gopath-XXXXXXXXXX)

GO111MODULE=on go install -mod=mod github.com/golang/mock/mockgen

`go env GOPATH`/bin/mockgen -build_flags "-mod=mod" -destination=src/common/mock_handler.go -package=common github.com/apache/thrift/test/go/src/gen/thrifttest ThriftTest

chmod a+w -R $GOPATH && rm -Rf $GOPATH
