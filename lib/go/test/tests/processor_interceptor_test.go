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
	"processorinterceptortest"
	"testing"
	"thrift"
	"time"
)

const requestArgument = "Test message"
const handlerResult = " is processed, return processed result"
const expectedHandlerResult = requestArgument + handlerResult
const expectedInterceptedHandlerResult = expectedHandlerResult + " was intercepted"

func NewResultInterceptor() thrift.HandlerInterceptor {
	return func(ctx context.Context, methodName string, arg interface{}, handlerFunc thrift.HandlerFunc) (result interface{}, err error) {
		handlerResult, handlerError := handlerFunc(ctx, arg)

		return handlerResult.(string) + " was intercepted", handlerError
	}
}

type DummyHandler struct {
}

func (h *DummyHandler) DummyRequest(ctx context.Context, arg1 string) (string, error) {
	return expectedHandlerResult, nil
}

func TestProcessorInterceptor(t *testing.T) {
	processor := processorinterceptortest.NewProcessorInterceptorProcessor(new(DummyHandler), thrift.NewHandlerInterceptorOption(NewResultInterceptor()))
	protocolFactory := thrift.NewTBinaryProtocolFactoryDefault()
	transportFactory := thrift.NewTTransportFactory()
	transportFactory = thrift.NewTFramedTransportFactory(transportFactory)
	addr := FindAvailableTCPServerPort()
	serverTransport, err := thrift.NewTServerSocketTimeout(addr.String(), TIMEOUT)
	if err != nil {
		t.Fatal("Unable to create server socket", err)
	}
	server = thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)

	defer server.Stop()
	go server.Serve()
	time.Sleep(10 * time.Millisecond)

	transport, err := createTransport(addr)
	if err != nil {
		t.Fatal(err)
	}
	defer transport.Close()
	protocol := protocolFactory.GetProtocol(transport)

	client := processorinterceptortest.NewProcessorInterceptorClient(thrift.NewTStandardClient(protocol, protocol))

	ret, err := client.DummyRequest(context.Background(), requestArgument)

	if err != nil {
		t.Fatal("Unable to call server:", err)
	} else if ret != expectedInterceptedHandlerResult {
		t.Fatal("Unexpected result from server: ", ret)
	}
}
