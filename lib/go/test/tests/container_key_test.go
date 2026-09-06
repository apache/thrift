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
	"strings"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/containerkeytest"
	"github.com/apache/thrift/lib/go/thrift"
)

// ContainerKeyTest.thrift is generated without the struct_key_entries option,
// so a struct-keyed map keeps the default Go representation: a map keyed by
// pointer. This assignment stops that changing without a deliberate edit.
var _ map[*containerkeytest.PlainKey]string = containerkeytest.DefaultStructKeyStruct{}.ByKey

func newContainerKeyStruct() *containerkeytest.ContainerKeyStruct {
	s := containerkeytest.NewContainerKeyStruct()
	s.ListKey = []thrift.MapEntry[[]string, string]{
		{Key: []string{"a", "b"}, Value: "ab"},
		{Key: []string{}, Value: "empty"},
	}
	s.SetKey = []thrift.MapEntry[[]int32, int64]{
		{Key: []int32{1, 2, 3}, Value: 6},
	}
	s.MapKey = []thrift.MapEntry[map[string]int32, bool]{
		{Key: map[string]int32{"x": 1}, Value: true},
	}
	s.TypedefKey = []thrift.MapEntry[containerkeytest.IntList, string]{
		{Key: containerkeytest.IntList{7}, Value: "seven"},
	}
	s.Nested = [][]thrift.MapEntry[[]string, []int32]{
		{{Key: []string{"k"}, Value: []int32{1}}},
		{},
	}
	s.ValueAlsoKeyed = []thrift.MapEntry[[]string, []thrift.MapEntry[[]int32, string]]{
		{Key: []string{"outer"}, Value: []thrift.MapEntry[[]int32, string]{{Key: []int32{9}, Value: "nine"}}},
	}
	return s
}

func TestContainerKeyRoundTrip(t *testing.T) {
	for label, factory := range map[string]thrift.TProtocolFactory{
		"binary":  thrift.NewTBinaryProtocolFactoryConf(nil),
		"compact": thrift.NewTCompactProtocolFactoryConf(nil),
		"json":    thrift.NewTJSONProtocolFactory(),
	} {
		t.Run(label, func(t *testing.T) {
			ctx := context.Background()
			src := newContainerKeyStruct()
			serializer := thrift.NewTSerializer()
			serializer.Protocol = factory.GetProtocol(serializer.Transport)
			data, err := serializer.Write(ctx, src)
			if err != nil {
				t.Fatalf("write: %v", err)
			}
			dst := containerkeytest.NewContainerKeyStruct()
			des := thrift.NewTDeserializer()
			des.Protocol = factory.GetProtocol(des.Transport)
			if err := des.Read(ctx, dst, data); err != nil {
				t.Fatalf("read: %v", err)
			}
			if !src.Equals(dst) {
				t.Errorf("round trip mismatch:\n src=%v\n dst=%v", src, dst)
			}
		})
	}
}

func TestContainerKeyWriteRejectsDuplicateKeys(t *testing.T) {
	s := containerkeytest.NewContainerKeyStruct()
	s.ListKey = []thrift.MapEntry[[]string, string]{
		{Key: []string{"a", "b"}, Value: "first"},
		{Key: []string{"a", "b"}, Value: "second"},
	}
	_, err := thrift.NewTSerializer().Write(context.Background(), s)
	var perr thrift.TProtocolException
	if !errors.As(err, &perr) || perr.TypeId() != thrift.INVALID_DATA {
		t.Fatalf("expected INVALID_DATA protocol exception for duplicate keys, got %v", err)
	}
	s.ListKey[1].Key = []string{"a", "c"}
	if _, err := thrift.NewTSerializer().Write(context.Background(), s); err != nil {
		t.Fatalf("distinct keys must serialize: %v", err)
	}
}

func TestContainerKeyWriteAcceptsMapKeysWithDifferentKeySets(t *testing.T) {
	s := containerkeytest.NewContainerKeyStruct()
	s.MapKey = []thrift.MapEntry[map[string]int32, bool]{
		{Key: map[string]int32{"a": 0}, Value: true},
		{Key: map[string]int32{"b": 0}, Value: false},
	}
	if _, err := thrift.NewTSerializer().Write(context.Background(), s); err != nil {
		t.Fatalf("map keys with different key sets are distinct and must serialize: %v", err)
	}
}

func TestContainerKeyEqualsDetectsKeyDifference(t *testing.T) {
	a := newContainerKeyStruct()
	b := newContainerKeyStruct()
	if !a.Equals(b) {
		t.Fatal("identical structs must be equal")
	}
	b.ListKey[0].Key[1] = "changed"
	if a.Equals(b) {
		t.Error("differing map keys must not be equal")
	}
	b = newContainerKeyStruct()
	b.SetKey[0].Value = 7
	if a.Equals(b) {
		t.Error("differing map values must not be equal")
	}
}

func TestContainerKeyConst(t *testing.T) {
	// Const map entries are emitted in the compiler's key order, so look up
	// by key rather than by position.
	want := map[string]int32{"a,b": 2, "": 0}
	if len(containerkeytest.LIST_KEYED_CONST) != len(want) {
		t.Fatalf("const has %d entries, want %d", len(containerkeytest.LIST_KEYED_CONST), len(want))
	}
	for _, e := range containerkeytest.LIST_KEYED_CONST {
		got, ok := want[strings.Join(e.Key, ",")]
		if !ok || got != e.Value {
			t.Errorf("unexpected entry %v", e)
		}
	}
}

func TestContainerKeyValidate(t *testing.T) {
	s := containerkeytest.NewContainerKeyStruct()
	s.Validated = []thrift.MapEntry[[]string, string]{{Key: []string{"k"}, Value: "v"}}
	if err := s.Validate(); err != nil {
		t.Fatalf("valid entry rejected: %v", err)
	}
	s.Validated[0].Key = []string{}
	if err := s.Validate(); err == nil {
		t.Error("empty key must fail vt.key.min_size")
	}
	s.Validated[0].Key = []string{"k"}
	s.Validated[0].Value = ""
	if err := s.Validate(); err == nil {
		t.Error("empty value must fail vt.value.min_size")
	}
}

// An entry slice stands in for a map, which is unordered, so two values
// holding the same entries in a different order are equal.
func TestContainerKeyEqualsIgnoresOrder(t *testing.T) {
	tgt := newContainerKeyStruct()
	src := newContainerKeyStruct()
	if len(src.ListKey) < 2 {
		t.Fatalf("fixture needs at least two entries, got %d", len(src.ListKey))
	}
	src.ListKey[0], src.ListKey[1] = src.ListKey[1], src.ListKey[0]
	if !tgt.Equals(src) {
		t.Error("reordering map entries must not change equality")
	}

	// The entries themselves still have to match.
	src = newContainerKeyStruct()
	src.ListKey[0].Value = "changed"
	if tgt.Equals(src) {
		t.Error("changing an entry value must change equality")
	}
}
