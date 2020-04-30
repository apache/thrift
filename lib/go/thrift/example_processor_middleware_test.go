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

func SimpleProcessorLoggingMiddleware(name string, next TProcessorFunction) TProcessorFunction {
	return WrappedTProcessorFunction{
		Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
			log.Printf("Before: %q", name)
			success, err := next.Process(ctx, seqId, in, out)
			log.Printf("After: %q", name)
			log.Printf("Success: %v", success)
			if err != nil {
				log.Printf("Error: %v", err)
			}
			return success, err
		},
	}
}

// This example demonstrates how to define and use a simple logging middleware
// to your thrift server/processor.
func ExampleProcessorMiddleware() {
	var (
		processor    TProcessor
		trans        TServerTransport
		transFactory TTransportFactory
		protoFactory TProtocolFactory
	)
	processor = WrapProcessor(processor, SimpleProcessorLoggingMiddleware)
	server := NewTSimpleServer4(processor, trans, transFactory, protoFactory)
	log.Fatal(server.Serve())
}
