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
	"testing"
)

type counter struct {
	count int
}

func (c *counter) incr() {
	c.count++
}

func newCounter(t *testing.T) *counter {
	c := counter{}
	if c.count != 0 {
		t.Fatal("Unexpected initial count.")
	}
	return &c
}

func testProcessorMiddleware(c *counter) ProcessorMiddleware {
	return func(name string, next TProcessorFunction) TProcessorFunction {
		return WrappedTProcessorFunction{
			Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
				c.incr()
				return next.Process(ctx, seqId, in, out)
			},
		}
	}
}

func testClientMiddleware(c *counter) ClientMiddleware {
	return func(next TClient) TClient {
		return WrappedTClient{
			Wrapped: func(ctx context.Context, method string, args, result TStruct) (ResponseMeta, error) {
				c.incr()
				return next.Call(ctx, method, args, result)
			},
		}
	}
}

func TestWrapProcessor(t *testing.T) {
	name := "test"
	processor := &mockWrappableProcessor{
		ProcessorFuncs: map[string]TProcessorFunction{
			name: WrappedTProcessorFunction{
				Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
					return true, nil
				},
			},
		},
	}
	c := newCounter(t)
	ctx := setMockWrappableProcessorName(context.Background(), name)
	wrapped := WrapProcessor(processor, testProcessorMiddleware(c))
	wrapped.Process(ctx, nil, nil)
	if c.count != 1 {
		t.Fatalf("Unexpected count value %v", c.count)
	}
}

func TestWrapTMultiplexedProcessor(t *testing.T) {
	name := "test"
	processorName := "foo"
	c := newCounter(t)
	processor := &TMultiplexedProcessor{}
	processor.RegisterDefault(&mockWrappableProcessor{
		ProcessorFuncs: map[string]TProcessorFunction{
			name: WrappedTProcessorFunction{
				Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
					return true, nil
				},
			},
		},
	})
	processor.RegisterProcessor(processorName, &mockWrappableProcessor{
		ProcessorFuncs: map[string]TProcessorFunction{
			name: WrappedTProcessorFunction{
				Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
					return true, nil
				},
			},
		},
	})
	wrapped := WrapProcessor(processor, testProcessorMiddleware(c))
	ctx := setMockWrappableProcessorName(context.Background(), name)
	in := NewStoredMessageProtocol(nil, name, 1, 1)
	wrapped.Process(ctx, in, nil)
	if c.count != 1 {
		t.Fatalf("Unexpected count value %v", c.count)
	}

	in = NewStoredMessageProtocol(nil, processorName+MULTIPLEXED_SEPARATOR+name, 1, 1)
	wrapped.Process(ctx, in, nil)
	if c.count != 2 {
		t.Fatalf("Unexpected count value %v", c.count)
	}
}

func TestWrapClient(t *testing.T) {
	client := WrappedTClient{
		Wrapped: func(ctx context.Context, method string, args, result TStruct) (ResponseMeta, error) {
			return ResponseMeta{}, nil
		},
	}
	c := newCounter(t)
	wrapped := WrapClient(client, testClientMiddleware(c))
	wrapped.Call(context.Background(), "test", nil, nil)
	if c.count != 1 {
		t.Fatalf("Unexpected count value %v", c.count)
	}
}
