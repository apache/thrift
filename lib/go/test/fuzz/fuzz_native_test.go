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

// Native Go fuzz targets for reading and round-tripping a generated struct.
//
// These carry no build tag, so unlike the go-fuzz targets in fuzz.go they run
// under a plain "go test": each one replays its seed corpus plus anything under
// testdata/fuzz/<Target>/ as an ordinary test case. "make check" runs them
// alongside the go-fuzz build check. To fuzz for real:
//
//	go test -run '^$' -fuzz FuzzStructReadBinary -fuzztime 2m
//
// A failing input is written to testdata/fuzz/<Target>/ and should be committed
// with the fix.
//
// Targets that need no generated code live in lib/go/thrift/fuzz_test.go and
// run with the library's own tests.

package fuzz

import (
	"context"
	"testing"

	"github.com/apache/thrift/lib/go/test/fuzz/gen-go/fuzztest"
	"github.com/apache/thrift/lib/go/thrift"
)

// nativeFuzzConf keeps a made-up size on the wire from costing memory rather
// than fuzzer time.
func nativeFuzzConf() *thrift.TConfiguration {
	return &thrift.TConfiguration{
		MaxMessageSize: 1 << 18,
		MaxFrameSize:   1 << 18,
	}
}

func nativeFuzzSeeds(f *testing.F) {
	f.Add([]byte{})
	// An empty struct, in binary and in compact.
	f.Add([]byte{0x00})
	// One i32 field, then stop.
	f.Add([]byte{0x08, 0x00, 0x01, 0x00, 0x00, 0x00, 0x2a, 0x00})
	// One string field, then stop.
	f.Add([]byte{0x0b, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 'a', 'b', 'c', 0x00})
	// A string field claiming a negative length.
	f.Add([]byte{0x0b, 0x00, 0x02, 0xff, 0xff, 0xff, 0xff, 0x00})
	// A list claiming a huge element count.
	f.Add([]byte{0x0f, 0x00, 0x03, 0x08, 0x7f, 0xff, 0xff, 0xff, 0x00})
	// A map with a large count and mismatched types.
	f.Add([]byte{0x0d, 0x00, 0x04, 0x0b, 0x0c, 0x7f, 0xff, 0xff, 0xff, 0x00})
	// Compact: an i32 field carrying a varint, then stop.
	f.Add([]byte{0x15, 0x54, 0x00})
	// JSON.
	f.Add([]byte(`{"1":{"i32":42}}`))
	f.Add([]byte(`{"2":{"str":"abc"}}`))
}

// readStruct reads a generated struct out of data, and reports whether that
// succeeded.
func readStruct(t *testing.T, data []byte, factory thrift.TProtocolFactory) (*fuzztest.FuzzTest, bool) {
	t.Helper()
	trans := thrift.NewTMemoryBufferLen(len(data))
	defer func() {
		trans.Close()
		trans.Buffer.Reset()
	}()
	trans.Write(data)
	value := fuzztest.NewFuzzTest()
	if err := value.Read(context.Background(), factory.GetProtocol(trans)); err != nil {
		return nil, false
	}
	return value, true
}

// roundtrip writes value back out, reads it again, and requires the two to be
// equal.
func roundtrip(t *testing.T, value *fuzztest.FuzzTest, factory thrift.TProtocolFactory) {
	t.Helper()
	ctx := context.Background()

	out := thrift.NewTMemoryBuffer()
	if err := value.Write(ctx, factory.GetProtocol(out)); err != nil {
		// Not every struct that reads is one this protocol can write back;
		// that is the writer's business, not a round-trip failure.
		return
	}

	serialized := out.Bytes()
	in := thrift.NewTMemoryBufferLen(len(serialized))
	in.Write(serialized)
	reread := fuzztest.NewFuzzTest()
	if err := reread.Read(ctx, factory.GetProtocol(in)); err != nil {
		t.Fatalf("re-reading what we just wrote failed: %v", err)
	}
	if !value.Equals(reread) {
		t.Fatalf("round trip changed the value:\n before: %v\n  after: %v", value, reread)
	}
}

func FuzzStructReadBinary(f *testing.F) {
	nativeFuzzSeeds(f)
	factory := thrift.NewTBinaryProtocolFactoryConf(nativeFuzzConf())
	f.Fuzz(func(t *testing.T, data []byte) {
		readStruct(t, data, factory)
	})
}

func FuzzStructReadCompact(f *testing.F) {
	nativeFuzzSeeds(f)
	factory := thrift.NewTCompactProtocolFactoryConf(nativeFuzzConf())
	f.Fuzz(func(t *testing.T, data []byte) {
		readStruct(t, data, factory)
	})
}

func FuzzStructReadJSON(f *testing.F) {
	nativeFuzzSeeds(f)
	factory := thrift.NewTJSONProtocolFactory()
	f.Fuzz(func(t *testing.T, data []byte) {
		readStruct(t, data, factory)
	})
}

func FuzzStructRoundtripBinary(f *testing.F) {
	nativeFuzzSeeds(f)
	factory := thrift.NewTBinaryProtocolFactoryConf(nativeFuzzConf())
	f.Fuzz(func(t *testing.T, data []byte) {
		if value, ok := readStruct(t, data, factory); ok {
			roundtrip(t, value, factory)
		}
	})
}

func FuzzStructRoundtripCompact(f *testing.F) {
	nativeFuzzSeeds(f)
	factory := thrift.NewTCompactProtocolFactoryConf(nativeFuzzConf())
	f.Fuzz(func(t *testing.T, data []byte) {
		if value, ok := readStruct(t, data, factory); ok {
			roundtrip(t, value, factory)
		}
	})
}

func FuzzStructRoundtripJSON(f *testing.F) {
	nativeFuzzSeeds(f)
	factory := thrift.NewTJSONProtocolFactory()
	f.Fuzz(func(t *testing.T, data []byte) {
		if value, ok := readStruct(t, data, factory); ok {
			roundtrip(t, value, factory)
		}
	})
}
