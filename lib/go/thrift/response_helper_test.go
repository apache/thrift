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

func TestResponseHelperContext(t *testing.T) {
	ctx := context.Background()

	t.Run(
		"empty-noop",
		func(t *testing.T) {
			helper, ok := GetResponseHelper(ctx)
			if ok {
				t.Error("GetResponseHelper expected ok == false")
			}
			// Just make sure those function calls does not panic
			helper.SetHeader("foo", "bar")
			helper.ClearHeaders()
		},
	)

	t.Run(
		"set-get",
		func(t *testing.T) {
			trans := NewTHeaderTransport(NewTMemoryBuffer())
			proto := NewTHeaderProtocol(trans)
			ctx = SetResponseHelper(
				ctx,
				TResponseHelper{
					THeaderResponseHelper: NewTHeaderResponseHelper(proto),
				},
			)
			helper, ok := GetResponseHelper(ctx)
			if !ok {
				t.Error("GetResponseHelper expected ok == true")
			}
			if helper.THeaderResponseHelper == nil {
				t.Error("GetResponseHelper expected THeaderResponseHelper to be non-nil")
			}
		},
	)
}

func TestHeaderHelper(t *testing.T) {
	t.Run(
		"THeaderProtocol",
		func(t *testing.T) {
			trans := NewTHeaderTransport(NewTMemoryBuffer())
			proto := NewTHeaderProtocol(trans)
			helper := NewTHeaderResponseHelper(proto)

			const (
				key   = "key"
				value = "value"
			)
			helper.SetHeader(key, value)
			if len(trans.writeHeaders) != 1 {
				t.Errorf(
					"Expected THeaderTransport.writeHeaders to be with size of 1, got %+v",
					trans.writeHeaders,
				)
			}
			actual := trans.writeHeaders[key]
			if actual != value {
				t.Errorf(
					"Expected THeaderTransport.writeHeaders to have %q:%q, got %+v",
					key,
					value,
					trans.writeHeaders,
				)
			}
			helper.ClearHeaders()
			if len(trans.writeHeaders) != 0 {
				t.Errorf(
					"Expected THeaderTransport.writeHeaders to be empty after ClearHeaders call, got %+v",
					trans.writeHeaders,
				)
			}
		},
	)

	t.Run(
		"other-protocol",
		func(t *testing.T) {
			trans := NewTMemoryBuffer()
			proto := NewTCompactProtocol(trans)
			helper := NewTHeaderResponseHelper(proto)

			// We only need to make sure that functions in helper
			// don't panic here.
			helper.SetHeader("foo", "bar")
			helper.ClearHeaders()
		},
	)

	t.Run(
		"zero-value",
		func(t *testing.T) {
			var helper *THeaderResponseHelper

			// We only need to make sure that functions in helper
			// don't panic here.
			helper.SetHeader("foo", "bar")
			helper.ClearHeaders()
		},
	)
}

func TestTResponseHelperZeroValue(t *testing.T) {
	var helper THeaderResponseHelper

	// We only need to make sure that functions in helper
	// don't panic here.
	helper.SetHeader("foo", "bar")
	helper.ClearHeaders()
}
