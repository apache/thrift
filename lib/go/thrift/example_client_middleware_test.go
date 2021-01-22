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
package thrift

import (
	"context"
	"log"
)

// BEGIN THRIFT GENERATED CODE SECTION
//
// In real code this section should be from thrift generated code instead,
// but for this example we just define some placeholders here.

type MyEndpointRequest struct{}

type MyEndpointResponse struct{}

type MyService interface {
	MyEndpoint(ctx context.Context, req *MyEndpointRequest) (*MyEndpointResponse, error)
}

func NewMyServiceClient(_ TClient) MyService {
	// In real code this certainly won't return nil.
	return nil
}

// END THRIFT GENERATED CODE SECTION

func simpleClientLoggingMiddleware(next TClient) TClient {
	return WrappedTClient{
		Wrapped: func(ctx context.Context, method string, args, result TStruct) (ResponseMeta, error) {
			log.Printf("Before: %q", method)
			log.Printf("Args: %#v", args)
			headers, err := next.Call(ctx, method, args, result)
			log.Printf("After: %q", method)
			log.Printf("Result: %#v", result)
			if err != nil {
				log.Printf("Error: %v", err)
			}
			return headers, err
		},
	}
}

// This example demonstrates how to define and use a simple logging middleware
// to your thrift client.
func ExampleClientMiddleware() {
	var (
		trans        TTransport
		protoFactory TProtocolFactory
	)
	var client TClient
	client = NewTStandardClient(
		protoFactory.GetProtocol(trans),
		protoFactory.GetProtocol(trans),
	)
	client = WrapClient(client, simpleClientLoggingMiddleware)
	myServiceClient := NewMyServiceClient(client)
	myServiceClient.MyEndpoint(context.Background(), &MyEndpointRequest{})
}
