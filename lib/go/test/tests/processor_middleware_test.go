/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package tests

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"github.com/apache/thrift/lib/go/test/gopath/src/processormiddlewaretest"
	"github.com/apache/thrift/lib/go/thrift"
)

const errorMessage = "foo error"

type serviceImpl struct{}

func (serviceImpl) Ping(_ context.Context) (err error) {
	return &processormiddlewaretest.Error{
		Foo: thrift.StringPtr(errorMessage),
	}
}

func middleware(t *testing.T) thrift.ProcessorMiddleware {
	return func(name string, next thrift.TProcessorFunction) thrift.TProcessorFunction {
		return thrift.WrappedTProcessorFunction{
			Wrapped: func(ctx context.Context, seqId int32, in, out thrift.TProtocol) (_ bool, err thrift.TException) {
				defer func() {
					checkError(t, err)
				}()
				return next.Process(ctx, seqId, in, out)
			},
		}
	}
}

func checkError(tb testing.TB, err error) {
	tb.Helper()

	var idlErr *processormiddlewaretest.Error
	if !errors.As(err, &idlErr) {
		tb.Errorf("expected error to be of type *processormiddlewaretest.Error, actual %T, %#v", err, err)
		return
	}
	if actual := idlErr.GetFoo(); actual != errorMessage {
		tb.Errorf("expected error message to be %q, actual %q", errorMessage, actual)
	}
}

func TestProcessorMiddleware(t *testing.T) {
	const timeout = time.Second

	processor := processormiddlewaretest.NewServiceProcessor(&serviceImpl{})
	serverTransport, err := thrift.NewTServerSocket("127.0.0.1:0")
	if err != nil {
		t.Fatalf("Could not find available server port: %v", err)
	}
	server := thrift.NewTSimpleServer4(
		thrift.WrapProcessor(processor, middleware(t)),
		serverTransport,
		thrift.NewTHeaderTransportFactoryConf(nil, nil),
		thrift.NewTHeaderProtocolFactoryConf(nil),
	)
	defer server.Stop()
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		defer wg.Done()
		server.Serve()
	}()

	time.Sleep(10 * time.Millisecond)

	cfg := &thrift.TConfiguration{
		ConnectTimeout: timeout,
		SocketTimeout:  timeout,
	}
	transport := thrift.NewTSocketFromAddrConf(serverTransport.Addr(), cfg)
	if err := transport.Open(); err != nil {
		t.Fatalf("Could not open client transport: %v", err)
	}
	defer transport.Close()
	protocol := thrift.NewTHeaderProtocolConf(transport, nil)

	client := processormiddlewaretest.NewServiceClient(thrift.NewTStandardClient(protocol, protocol))

	err = client.Ping(context.Background())
	checkError(t, err)
}
