#!/bin/sh

set -e

export GOPATH=$(mktemp -d -t gopath-XXXXXXXXXX)

# TODO: Once we dropped support to go 1.15, add "@v1.5.0" suffix to go install
GO111MODULE=on go install -mod=mod github.com/golang/mock/mockgen

`go env GOPATH`/bin/mockgen -build_flags "-mod=mod" -destination=src/common/mock_handler.go -package=common github.com/apache/thrift/test/go/src/gen/thrifttest ThriftTest

rm -Rf $GOPATH
