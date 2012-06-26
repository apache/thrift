# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

all: test

test:
	export GOPATH="$(CURDIR)"
	go build thrift
	go test thrift

install:
	export GOPATH="$(CURDIR)"
	go install -v -x -work thrift
