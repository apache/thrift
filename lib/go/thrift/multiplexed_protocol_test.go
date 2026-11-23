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
	"strings"
	"testing"
)

func TestMultiplexedProcessorMap(t *testing.T) {
	name := "test"
	processorName := "foo"
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

	processorMap := processor.ProcessorMap()
	if len(processorMap) != 2 {
		t.Fatalf("Wrong processor map size %#v", processorMap)
	}
	for k := range processorMap {
		components := strings.SplitN(k, MULTIPLEXED_SEPARATOR, 2)
		if len(components) == 1 {
			if components[0] != name {
				t.Fatalf("Wrong name for default processor func, expected %q, got %q", name, components[0])
			}
		} else if len(components) == 2 {
			if components[0] != processorName {
				t.Errorf("Wrong processor name, expected %q, got %q", processorName, components[0])
			}
			if components[1] != name {
				t.Errorf("Wrong name for processor func, expected %q, got %q", name, components[1])
			}
		} else {
			t.Fatalf("Wrong number of components %#v", components)
		}
	}
}
