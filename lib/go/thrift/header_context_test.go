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
	"reflect"
	"testing"
)

func TestSetGetHeader(t *testing.T) {
	const (
		key   = "foo"
		value = "bar"
	)
	ctx := context.Background()

	ctx = SetHeader(ctx, key, value)

	checkGet := func(t *testing.T, ctx context.Context) {
		t.Helper()
		got, ok := GetHeader(ctx, key)
		if !ok {
			t.Fatalf("Cannot get header %q back after setting it.", key)
		}
		if got != value {
			t.Fatalf("Header value expected %q, got %q instead", value, got)
		}
	}

	checkGet(t, ctx)

	t.Run(
		"NoConflicts",
		func(t *testing.T) {
			type otherType string
			const otherValue = "bar2"

			ctx = context.WithValue(ctx, otherType(key), otherValue)
			checkGet(t, ctx)
		},
	)

	t.Run(
		"GetHeaderOnNonExistKey",
		func(t *testing.T) {
			const otherKey = "foo2"

			if _, ok := GetHeader(ctx, otherKey); ok {
				t.Errorf("GetHeader returned ok on non-existing key %q", otherKey)
			}
		},
	)
}

func TestReadKeyList(t *testing.T) {
	headers := THeaderMap{
		"key1": "value1",
		"key2": "value2",
	}
	ctx := context.Background()

	ctx = AddReadTHeaderToContext(ctx, headers)

	got := make(THeaderMap)
	keys := GetReadHeaderList(ctx)
	t.Logf("keys: %+v", keys)
	for _, key := range keys {
		value, ok := GetHeader(ctx, key)
		if ok {
			got[key] = value
		} else {
			t.Errorf("Cannot get key %q from context", key)
		}
	}

	if !reflect.DeepEqual(headers, got) {
		t.Errorf("Expected header map %+v, got %+v", headers, got)
	}

	writtenKeys := GetWriteHeaderList(ctx)
	if len(writtenKeys) > 0 {
		t.Errorf(
			"Expected empty GetWriteHeaderList() result, got %+v",
			writtenKeys,
		)
	}
}

func TestWriteKeyList(t *testing.T) {
	keys := []string{
		"key1",
		"key2",
	}
	ctx := context.Background()

	ctx = SetWriteHeaderList(ctx, keys)
	got := GetWriteHeaderList(ctx)

	if !reflect.DeepEqual(keys, got) {
		t.Errorf("Expected header keys %+v, got %+v", keys, got)
	}

	readKeys := GetReadHeaderList(ctx)
	if len(readKeys) > 0 {
		t.Errorf(
			"Expected empty GetReadHeaderList() result, got %+v",
			readKeys,
		)
	}
}
