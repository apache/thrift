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
	"fmt"
)

type mockProcessor struct {
	ProcessFunc func(in, out TProtocol) (bool, TException)
}

func (m *mockProcessor) Process(ctx context.Context, in, out TProtocol) (bool, TException) {
	return m.ProcessFunc(in, out)
}

func (m *mockProcessor) ProcessorMap() map[string]TProcessorFunction {
	return map[string]TProcessorFunction{
		"mock": WrappedTProcessorFunction{
			Wrapped: func(ctx context.Context, seqId int32, in, out TProtocol) (bool, TException) {
				return m.ProcessFunc(in, out)
			},
		},
	}
}

func (m *mockProcessor) AddToProcessorMap(name string, processorFunc TProcessorFunction) {}

type mockWrappedProcessorContextKey int

const (
	processorName mockWrappedProcessorContextKey = iota
)

// setMockWrappableProcessorName sets the "name" of the TProcessorFunction to
// call on a mockWrappableProcessor when calling Process.
//
// In a normal TProcessor, the request name is read from the request itself
// which happens in TProcessor.Process, so it is not passed into the call to
// Process itself, to get around this in testing, mockWrappableProcessor calls
// getMockWrappableProcessorName  to get the name to use from the context
// object.
func setMockWrappableProcessorName(ctx context.Context, name string) context.Context {
	return context.WithValue(ctx, processorName, name)
}

// getMockWrappableProcessorName gets the "name" of the TProcessorFunction to
// call on a mockWrappableProcessor when calling Process.
func getMockWrappableProcessorName(ctx context.Context) (string, bool) {
	val, ok := ctx.Value(processorName).(string)
	return val, ok
}

// mockWrappableProcessor can be used to create a mock object that fufills the
// TProcessor interface in testing.
type mockWrappableProcessor struct {
	ProcessorFuncs map[string]TProcessorFunction
}

// Process calls the TProcessorFunction assigned to the "name" set on the
// context object by setMockWrappableProcessorName.
//
// If no name is set on the context or there is no TProcessorFunction mapped to
// that name, the call will panic.
func (p *mockWrappableProcessor) Process(ctx context.Context, in, out TProtocol) (bool, TException) {
	name, ok := getMockWrappableProcessorName(ctx)
	if !ok {
		panic("MockWrappableProcessorName not set on context")
	}
	processor, ok := p.ProcessorMap()[name]
	if !ok {
		panic(fmt.Sprintf("No processor set for name %q", name))
	}
	return processor.Process(ctx, 0, in, out)
}

func (p *mockWrappableProcessor) ProcessorMap() map[string]TProcessorFunction {
	return p.ProcessorFuncs
}

func (p *mockWrappableProcessor) AddToProcessorMap(name string, processorFunc TProcessorFunction) {
	p.ProcessorFuncs[name] = processorFunc
}

var (
	_ TProcessor = (*mockProcessor)(nil)
	_ TProcessor = (*mockWrappableProcessor)(nil)
)
