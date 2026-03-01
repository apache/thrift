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
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/clientmiddlewareexceptiontest"
	"github.com/apache/thrift/lib/go/thrift"
)

type fakeClientMiddlewareExceptionTestHandler func(ctx context.Context) (*clientmiddlewareexceptiontest.FooResponse, error)

func (f fakeClientMiddlewareExceptionTestHandler) Foo(ctx context.Context) (*clientmiddlewareexceptiontest.FooResponse, error) {
	return f(ctx)
}

type clientMiddlewareErrorChecker func(err error) error

var clientMiddlewareExceptionCases = []struct {
	label   string
	handler fakeClientMiddlewareExceptionTestHandler
	checker clientMiddlewareErrorChecker
}{
	{
		label: "no-error",
		handler: func(_ context.Context) (*clientmiddlewareexceptiontest.FooResponse, error) {
			return new(clientmiddlewareexceptiontest.FooResponse), nil
		},
		checker: func(err error) error {
			if err != nil {
				return errors.New("expected err to be nil")
			}
			return nil
		},
	},
	{
		label: "exception-1",
		handler: func(_ context.Context) (*clientmiddlewareexceptiontest.FooResponse, error) {
			return nil, new(clientmiddlewareexceptiontest.Exception1)
		},
		checker: func(err error) error {
			if !errors.As(err, new(*clientmiddlewareexceptiontest.Exception1)) {
				return errors.New("expected err to be of type *clientmiddlewareexceptiontest.Exception1")
			}
			return nil
		},
	},
	{
		label: "no-error",
		handler: func(_ context.Context) (*clientmiddlewareexceptiontest.FooResponse, error) {
			return nil, new(clientmiddlewareexceptiontest.Exception2)
		},
		checker: func(err error) error {
			if !errors.As(err, new(*clientmiddlewareexceptiontest.Exception2)) {
				return errors.New("expected err to be of type *clientmiddlewareexceptiontest.Exception2")
			}
			return nil
		},
	},
}

func TestClientMiddlewareException(t *testing.T) {
	for _, c := range clientMiddlewareExceptionCases {
		t.Run(c.label, func(t *testing.T) {
			serverSocket, err := thrift.NewTServerSocket(":0")
			if err != nil {
				t.Fatalf("failed to create server socket: %v", err)
			}
			processor := clientmiddlewareexceptiontest.NewClientMiddlewareExceptionTestProcessor(c.handler)
			server := thrift.NewTSimpleServer2(processor, serverSocket)
			if err := server.Listen(); err != nil {
				t.Fatalf("failed to listen server: %v", err)
			}
			addr := serverSocket.Addr().String()
			go server.Serve()
			t.Cleanup(func() {
				server.Stop()
			})

			var cfg *thrift.TConfiguration
			socket := thrift.NewTSocketConf(addr, cfg)
			if err := socket.Open(); err != nil {
				t.Fatalf("failed to create client connection: %v", err)
			}
			t.Cleanup(func() {
				socket.Close()
			})
			inProtocol := thrift.NewTBinaryProtocolConf(socket, cfg)
			outProtocol := thrift.NewTBinaryProtocolConf(socket, cfg)
			middleware := func(next thrift.TClient) thrift.TClient {
				return thrift.WrappedTClient{
					Wrapped: func(ctx context.Context, method string, args, result thrift.TStruct) (_ thrift.ResponseMeta, err error) {
						defer func() {
							if checkErr := c.checker(err); checkErr != nil {
								t.Errorf("middleware result unexpected: %v (result=%#v, err=%#v)", checkErr, result, err)
							}
						}()
						return next.Call(ctx, method, args, result)
					},
				}
			}
			client := thrift.WrapClient(
				thrift.NewTStandardClient(inProtocol, outProtocol),
				middleware,
				thrift.ExtractIDLExceptionClientMiddleware,
			)
			result, err := clientmiddlewareexceptiontest.NewClientMiddlewareExceptionTestClient(client).Foo(context.Background())
			if checkErr := c.checker(err); checkErr != nil {
				t.Errorf("final result unexpected: %v (result=%#v, err=%#v)", checkErr, result, err)
			}
		})
	}
}

func TestExtractExceptionFromResult(t *testing.T) {

	for _, c := range clientMiddlewareExceptionCases {
		t.Run(c.label, func(t *testing.T) {
			serverSocket, err := thrift.NewTServerSocket(":0")
			if err != nil {
				t.Fatalf("failed to create server socket: %v", err)
			}
			processor := clientmiddlewareexceptiontest.NewClientMiddlewareExceptionTestProcessor(c.handler)
			server := thrift.NewTSimpleServer2(processor, serverSocket)
			if err := server.Listen(); err != nil {
				t.Fatalf("failed to listen server: %v", err)
			}
			addr := serverSocket.Addr().String()
			go server.Serve()
			t.Cleanup(func() {
				server.Stop()
			})

			var cfg *thrift.TConfiguration
			socket := thrift.NewTSocketConf(addr, cfg)
			if err := socket.Open(); err != nil {
				t.Fatalf("failed to create client connection: %v", err)
			}
			t.Cleanup(func() {
				socket.Close()
			})
			inProtocol := thrift.NewTBinaryProtocolConf(socket, cfg)
			outProtocol := thrift.NewTBinaryProtocolConf(socket, cfg)
			middleware := func(next thrift.TClient) thrift.TClient {
				return thrift.WrappedTClient{
					Wrapped: func(ctx context.Context, method string, args, result thrift.TStruct) (_ thrift.ResponseMeta, err error) {
						defer func() {
							if err == nil {
								err = thrift.ExtractExceptionFromResult(result)
							}
							if checkErr := c.checker(err); checkErr != nil {
								t.Errorf("middleware result unexpected: %v (result=%#v, err=%#v)", checkErr, result, err)
							}
						}()
						return next.Call(ctx, method, args, result)
					},
				}
			}
			client := thrift.WrapClient(
				thrift.NewTStandardClient(inProtocol, outProtocol),
				middleware,
			)
			result, err := clientmiddlewareexceptiontest.NewClientMiddlewareExceptionTestClient(client).Foo(context.Background())
			if checkErr := c.checker(err); checkErr != nil {
				t.Errorf("final result unexpected: %v (result=%#v, err=%#v)", checkErr, result, err)
			}
		})
	}
}
