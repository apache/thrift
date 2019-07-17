#!/bin/sh
set -e

export GOPATH=`pwd`
export GOBIN=`pwd`/bin
export GO111MODULE=off

mkdir -p src/github.com/golang/mock
cd src/github.com/golang
curl -fsSL https://github.com/golang/mock/archive/v1.2.0.tar.gz -o mock.tar.gz
tar -xzvf mock.tar.gz -C mock --strip-components=1
cd mock/mockgen
go install .
cd ../../../../../
bin/mockgen -destination=src/common/mock_handler.go -package=common gen/thrifttest ThriftTest
