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

	"github.com/apache/thrift/lib/go/test/gopath/src/structkeytest"
	"github.com/apache/thrift/lib/go/thrift"
)

func comparableKey() *structkeytest.ComparableKey {
	return &structkeytest.ComparableKey{
		Flag:  true,
		Tiny:  7,
		Small: 300,
		ID:    11,
		Big:   1 << 40,
		Ratio: 0.5,
		Name:  "comparable",
		Kind:  structkeytest.KeyKind_ALPHA,
	}
}

func pairwiseKey() *structkeytest.PairwiseKey {
	return &structkeytest.PairwiseKey{ID: 12, Blob: []byte{1, 2, 3}}
}

func optionalFieldKey(note string) *structkeytest.OptionalFieldKey {
	return &structkeytest.OptionalFieldKey{ID: 13, Note: &note}
}

func unionAliasKey(num int32) *structkeytest.KeyUnion {
	return &structkeytest.KeyUnion{Num: &num}
}

func newStructKeyStruct() *structkeytest.StructKeyStruct {
	s := structkeytest.NewStructKeyStruct()
	s.ByKey = []thrift.MapEntry[*structkeytest.Key, string]{
		{Key: &structkeytest.Key{ID: 1, Name: "one"}, Value: "1"},
		{Key: &structkeytest.Key{ID: 2, Name: "two"}, Value: "2"},
	}
	s.ByErr = []thrift.MapEntry[*structkeytest.KeyErr, int32]{
		{Key: &structkeytest.KeyErr{Msg: "boom"}, Value: 7},
	}
	// A map with a hashable key stays a Go map under the option.
	s.Plain = map[string]int32{"a": 1}
	num := int32(9)
	text := "nine"
	s.ByUnion = []thrift.MapEntry[*structkeytest.KeyUnion, int32]{
		{Key: &structkeytest.KeyUnion{Num: &num}, Value: 1},
		{Key: &structkeytest.KeyUnion{Text: &text}, Value: 2},
	}
	// A typedef of a struct resolves to the struct pointer, because the
	// generated "type KeyAlias *Key" has no methods of its own.
	s.ByAlias = []thrift.MapEntry[*structkeytest.Key, string]{
		{Key: &structkeytest.Key{ID: 3, Name: "three"}, Value: "3"},
	}
	s.Nested = [][]thrift.MapEntry[*structkeytest.Key, string]{
		{{Key: &structkeytest.Key{ID: 4, Name: "four"}, Value: "4"}},
		{},
	}
	s.ValueAlsoKeyed = []thrift.MapEntry[*structkeytest.Key, []thrift.MapEntry[*structkeytest.Key, int32]]{
		{
			Key:   &structkeytest.Key{ID: 5, Name: "outer"},
			Value: []thrift.MapEntry[*structkeytest.Key, int32]{{Key: &structkeytest.Key{ID: 6, Name: "inner"}, Value: 6}},
		},
	}
	s.Validated = []thrift.MapEntry[*structkeytest.ValidatedKey, string]{
		{Key: &structkeytest.ValidatedKey{ID: 1}, Value: "ok"},
	}
	// A list or a set value is a slice on both sides of the entry.
	s.ListValue = []thrift.MapEntry[*structkeytest.Key, []string]{
		{Key: &structkeytest.Key{ID: 7, Name: "seven"}, Value: []string{"a", "b"}},
	}
	s.SetValue = []thrift.MapEntry[*structkeytest.Key, []string]{
		{Key: &structkeytest.Key{ID: 8, Name: "eight"}, Value: []string{"c"}},
	}
	// A key of only non-pointer scalars takes the linear seen-map write path.
	s.ByComparable = []thrift.MapEntry[*structkeytest.ComparableKey, string]{
		{Key: comparableKey(), Value: "cmp"},
	}
	// A binary field is not comparable with ==, so this key takes the
	// pairwise write path even though it is a plain struct.
	s.ByPairwise = []thrift.MapEntry[*structkeytest.PairwiseKey, string]{
		{Key: pairwiseKey(), Value: "blob"},
	}
	// An optional field generates as a pointer, so this key is pairwise too.
	s.ByOptionalField = []thrift.MapEntry[*structkeytest.OptionalFieldKey, string]{
		{Key: optionalFieldKey("nine"), Value: "opt"},
	}
	// A typedef of a union resolves to the union pointer.
	s.ByUnionAlias = []thrift.MapEntry[*structkeytest.KeyUnion, int32]{
		{Key: unionAliasKey(14), Value: 4},
	}
	return s
}

func structKeyProtocols() map[string]thrift.TProtocolFactory {
	return map[string]thrift.TProtocolFactory{
		"binary":  thrift.NewTBinaryProtocolFactoryConf(nil),
		"compact": thrift.NewTCompactProtocolFactoryConf(nil),
		"json":    thrift.NewTJSONProtocolFactory(),
	}
}

func structKeyRoundTrip(t *testing.T, factory thrift.TProtocolFactory, src, dst thrift.TStruct) error {
	t.Helper()
	ctx := context.Background()
	serializer := thrift.NewTSerializer()
	serializer.Protocol = factory.GetProtocol(serializer.Transport)
	data, err := serializer.Write(ctx, src)
	if err != nil {
		return err
	}
	deserializer := thrift.NewTDeserializer()
	deserializer.Protocol = factory.GetProtocol(deserializer.Transport)
	return deserializer.Read(ctx, dst, data)
}

func TestStructKeyRoundTrip(t *testing.T) {
	for label, factory := range structKeyProtocols() {
		t.Run(label, func(t *testing.T) {
			src := newStructKeyStruct()
			dst := structkeytest.NewStructKeyStruct()
			if err := structKeyRoundTrip(t, factory, src, dst); err != nil {
				t.Fatalf("round trip: %v", err)
			}
			// Keys are fresh allocations after decoding, so this only holds if
			// Equals compares key contents rather than pointer identity.
			if !src.Equals(dst) {
				t.Errorf("decoded struct not equal to original:\n src=%v\n dst=%v", src, dst)
			}
		})
	}
}

func TestStructKeyEmptyRoundTrip(t *testing.T) {
	for label, factory := range structKeyProtocols() {
		t.Run(label, func(t *testing.T) {
			// Every entry-slice field is nil here. They must survive a round
			// trip and still compare equal to the decoded empty slices.
			src := structkeytest.NewStructKeyStruct()
			dst := structkeytest.NewStructKeyStruct()
			if err := structKeyRoundTrip(t, factory, src, dst); err != nil {
				t.Fatalf("round trip: %v", err)
			}
			if !src.Equals(dst) {
				t.Errorf("empty struct not equal after round trip:\n src=%v\n dst=%v", src, dst)
			}
			if len(dst.ByKey) != 0 {
				t.Errorf("ByKey = %v, want no entries", dst.ByKey)
			}
		})
	}
}

func TestStructKeyUnionWithNoFieldSet(t *testing.T) {
	t.Run("single", func(t *testing.T) {
		s := newStructKeyStruct()
		s.ByUnion = []thrift.MapEntry[*structkeytest.KeyUnion, int32]{
			{Key: structkeytest.NewKeyUnion(), Value: 1},
		}
		if _, err := thrift.NewTSerializer().Write(context.Background(), s); err == nil {
			t.Error("expected a write error for a union key with no field set")
		}
	})
	t.Run("duplicate", func(t *testing.T) {
		// Two unions with no field set are equal, so the uniqueness check
		// rejects them before the union reports its own set-field error.
		s := newStructKeyStruct()
		s.ByUnion = []thrift.MapEntry[*structkeytest.KeyUnion, int32]{
			{Key: structkeytest.NewKeyUnion(), Value: 1},
			{Key: structkeytest.NewKeyUnion(), Value: 2},
		}
		_, err := thrift.NewTSerializer().Write(context.Background(), s)
		var perr thrift.TProtocolException
		if !errors.As(err, &perr) || perr.TypeId() != thrift.INVALID_DATA {
			t.Fatalf("expected INVALID_DATA protocol exception, got %v", err)
		}
	})
}

func TestStructKeyWriteRejectsDuplicateKeys(t *testing.T) {
	// Two entries whose keys are distinct pointers with equal contents are
	// the same key, on both the seen-map and the pairwise write path.
	tests := map[string]func(s *structkeytest.StructKeyStruct){
		"struct key, seen-map path": func(s *structkeytest.StructKeyStruct) {
			s.ByKey[1].Key = &structkeytest.Key{ID: 1, Name: "one"}
		},
		"scalar-only key": func(s *structkeytest.StructKeyStruct) {
			s.ByComparable = append(s.ByComparable,
				thrift.MapEntry[*structkeytest.ComparableKey, string]{Key: comparableKey(), Value: "dup"})
		},
		"binary key, pairwise path": func(s *structkeytest.StructKeyStruct) {
			s.ByPairwise = append(s.ByPairwise,
				thrift.MapEntry[*structkeytest.PairwiseKey, string]{Key: pairwiseKey(), Value: "dup"})
		},
		"optional-field key": func(s *structkeytest.StructKeyStruct) {
			s.ByOptionalField = append(s.ByOptionalField,
				thrift.MapEntry[*structkeytest.OptionalFieldKey, string]{Key: optionalFieldKey("nine"), Value: "dup"})
		},
		"union alias key": func(s *structkeytest.StructKeyStruct) {
			s.ByUnionAlias = append(s.ByUnionAlias,
				thrift.MapEntry[*structkeytest.KeyUnion, int32]{Key: unionAliasKey(14), Value: 5})
		},
	}
	for name, inject := range tests {
		t.Run(name, func(t *testing.T) {
			s := newStructKeyStruct()
			inject(s)
			_, err := thrift.NewTSerializer().Write(context.Background(), s)
			var perr thrift.TProtocolException
			if !errors.As(err, &perr) || perr.TypeId() != thrift.INVALID_DATA {
				t.Fatalf("expected INVALID_DATA protocol exception for duplicate keys, got %v", err)
			}
		})
	}
}

func TestStructKeyWriteAcceptsDistinctKeys(t *testing.T) {
	// Every scalar field counts: changing only the enum field makes the
	// keys distinct where an == on a prefix of the fields would not.
	s := newStructKeyStruct()
	distinct := comparableKey()
	distinct.Kind = structkeytest.KeyKind_BETA
	s.ByComparable = append(s.ByComparable,
		thrift.MapEntry[*structkeytest.ComparableKey, string]{Key: distinct, Value: "distinct"})
	if _, err := thrift.NewTSerializer().Write(context.Background(), s); err != nil {
		t.Fatalf("write: %v", err)
	}
}

func TestStructKeyWriteHandlesNilKeys(t *testing.T) {
	// A nil key writes as an empty struct, so one of them is legal.
	t.Run("single", func(t *testing.T) {
		s := newStructKeyStruct()
		s.ByKey[0].Key = nil
		if _, err := thrift.NewTSerializer().Write(context.Background(), s); err != nil {
			t.Errorf("write: %v", err)
		}
	})
	// Two are not: Equals reports two nil keys as equal.
	t.Run("duplicate", func(t *testing.T) {
		s := newStructKeyStruct()
		s.ByKey[0].Key = nil
		s.ByKey[1].Key = nil
		_, err := thrift.NewTSerializer().Write(context.Background(), s)
		var perr thrift.TProtocolException
		if !errors.As(err, &perr) || perr.TypeId() != thrift.INVALID_DATA {
			t.Fatalf("expected INVALID_DATA protocol exception, got %v", err)
		}
	})
	// The pairwise path must agree: one nil key is legal there too.
	t.Run("pairwise single", func(t *testing.T) {
		s := newStructKeyStruct()
		s.ByPairwise[0].Key = nil
		if _, err := thrift.NewTSerializer().Write(context.Background(), s); err != nil {
			t.Errorf("write: %v", err)
		}
	})
	// And two nil keys are rejected, because Equals reports two nil keys
	// as equal.
	t.Run("pairwise duplicate", func(t *testing.T) {
		s := newStructKeyStruct()
		s.ByPairwise[0].Key = nil
		s.ByPairwise = append(s.ByPairwise,
			thrift.MapEntry[*structkeytest.PairwiseKey, string]{Key: nil, Value: "dup"})
		_, err := thrift.NewTSerializer().Write(context.Background(), s)
		var perr thrift.TProtocolException
		if !errors.As(err, &perr) || perr.TypeId() != thrift.INVALID_DATA {
			t.Fatalf("expected INVALID_DATA protocol exception, got %v", err)
		}
	})
}

func TestStructKeyEqualsDetectsKeyDifference(t *testing.T) {
	tests := map[string]func(s *structkeytest.StructKeyStruct){
		"struct key":       func(s *structkeytest.StructKeyStruct) { s.ByKey[1].Key.Name = "deux" },
		"map value":        func(s *structkeytest.StructKeyStruct) { s.ByKey[1].Value = "changed" },
		"exception key":    func(s *structkeytest.StructKeyStruct) { s.ByErr[0].Key.Msg = "different" },
		"union key":        func(s *structkeytest.StructKeyStruct) { *s.ByUnion[0].Key.Num = 10 },
		"typedef key":      func(s *structkeytest.StructKeyStruct) { s.ByAlias[0].Key.ID = 30 },
		"nested key":       func(s *structkeytest.StructKeyStruct) { s.Nested[0][0].Key.ID = 40 },
		"keyed value key":  func(s *structkeytest.StructKeyStruct) { s.ValueAlsoKeyed[0].Value[0].Key.ID = 60 },
		"entry order only": func(s *structkeytest.StructKeyStruct) { s.ByKey[0], s.ByKey[1] = s.ByKey[1], s.ByKey[0] },
		"list value key":   func(s *structkeytest.StructKeyStruct) { s.ListValue[0].Key.ID = 70 },
		"list value":       func(s *structkeytest.StructKeyStruct) { s.ListValue[0].Value[1] = "changed" },
		"set value":        func(s *structkeytest.StructKeyStruct) { s.SetValue[0].Value = []string{"d"} },
		"comparable key":   func(s *structkeytest.StructKeyStruct) { s.ByComparable[0].Key.Kind = structkeytest.KeyKind_BETA },
		"binary key":       func(s *structkeytest.StructKeyStruct) { s.ByPairwise[0].Key.Blob[0] = 9 },
		"optional field":   func(s *structkeytest.StructKeyStruct) { s.ByOptionalField[0].Key.Note = nil },
		"union alias key":  func(s *structkeytest.StructKeyStruct) { *s.ByUnionAlias[0].Key.Num = 15 },
	}
	a := newStructKeyStruct()
	for name, mutate := range tests {
		t.Run(name, func(t *testing.T) {
			b := newStructKeyStruct()
			if !a.Equals(b) {
				t.Fatal("identical structs must be equal")
			}
			mutate(b)
			if a.Equals(b) {
				t.Error("expected structs to differ")
			}
		})
	}
}

func TestStructKeyValidateChecksKeys(t *testing.T) {
	s := newStructKeyStruct()
	if err := s.Validate(); err != nil {
		t.Fatalf("valid struct rejected: %v", err)
	}
	// vt.key.skip = "false" runs the key's own validator over every entry.
	s.Validated[0].Key.ID = 0
	err := s.Validate()
	var aerr thrift.TApplicationException
	if !errors.As(err, &aerr) || aerr.TypeId() != thrift.VALIDATION_FAILED {
		t.Fatalf("expected VALIDATION_FAILED for an invalid key, got %v", err)
	}
}

func TestStructKeyConst(t *testing.T) {
	// Const map entries are emitted in the compiler's key order, so validate
	// by key content rather than assuming positional order.
	want := []thrift.MapEntry[*structkeytest.Key, int32]{
		{Key: &structkeytest.Key{ID: 1, Name: "one"}, Value: 1},
		{Key: &structkeytest.Key{ID: 2, Name: "two"}, Value: 2},
	}
	if len(structkeytest.STRUCT_KEYED_CONST) != len(want) {
		t.Fatalf("got %d entries, want %d", len(structkeytest.STRUCT_KEYED_CONST), len(want))
	}
	for _, w := range want {
		found := false
		for _, entry := range structkeytest.STRUCT_KEYED_CONST {
			if entry.Key.Equals(w.Key) {
				found = true
				if entry.Value != w.Value {
					t.Errorf("key %v value = %d, want %d", w.Key, entry.Value, w.Value)
				}
				break
			}
		}
		if !found {
			t.Errorf("missing const entry for key %v", w.Key)
		}
	}
}
