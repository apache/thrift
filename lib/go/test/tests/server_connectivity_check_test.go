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
	"runtime/debug"
	"testing"
	"time"

	"github.com/apache/thrift/lib/go/test/gopath/src/clientmiddlewareexceptiontest"
	"github.com/apache/thrift/lib/go/thrift"
)

func TestServerConnectivityCheck(t *testing.T) {
	const (
		// Server will sleep for longer than client is willing to wait
		// so client will close the connection.
		serverSleep         = 50 * time.Millisecond
		clientSocketTimeout = time.Millisecond
	)
	serverSocket, err := thrift.NewTServerSocket(":0")
	if err != nil {
		t.Fatalf("failed to create server socket: %v", err)
	}
	processor := clientmiddlewareexceptiontest.NewClientMiddlewareExceptionTestProcessor(fakeClientMiddlewareExceptionTestHandler(
		func(ctx context.Context) (*clientmiddlewareexceptiontest.FooResponse, error) {
			time.Sleep(serverSleep)
			err := ctx.Err()
			if err == nil {
				t.Error("Expected server ctx to be cancelled, did not happen")
				return new(clientmiddlewareexceptiontest.FooResponse), nil
			}
			return nil, err
		},
	))
	server := thrift.NewTSimpleServer2(processor, serverSocket)
	if err := server.Listen(); err != nil {
		t.Fatalf("failed to listen server: %v", err)
	}
	server.SetLogger(func(msg string) {
		t.Errorf("Server logger called with %q", msg)
		t.Errorf("Server logger callstack:\n%s", debug.Stack())
	})
	addr := serverSocket.Addr().String()
	go server.Serve()
	t.Cleanup(func() {
		server.Stop()
	})

	cfg := &thrift.TConfiguration{
		SocketTimeout: clientSocketTimeout,
	}
	socket := thrift.NewTSocketConf(addr, cfg)
	if err := socket.Open(); err != nil {
		t.Fatalf("failed to create client connection: %v", err)
	}
	t.Cleanup(func() {
		socket.Close()
	})
	inProtocol := thrift.NewTBinaryProtocolConf(socket, cfg)
	outProtocol := thrift.NewTBinaryProtocolConf(socket, cfg)
	client := thrift.NewTStandardClient(inProtocol, outProtocol)
	ctx, cancel := context.WithTimeout(context.Background(), clientSocketTimeout)
	defer cancel()
	_, err = clientmiddlewareexceptiontest.NewClientMiddlewareExceptionTestClient(client).Foo(ctx)
	socket.Close()
	if err == nil {
		t.Error("Expected client to time out, did not happen")
	}
}
