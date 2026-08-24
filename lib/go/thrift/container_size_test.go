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
	"bytes"
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"testing"
)

// writeTruncatedListHeader writes a complete message and struct field header
// followed by a list header that declares count elements, and nothing else.
// The declared count is therefore not backed by any element bytes.
func writeTruncatedListHeader(t *testing.T, p TProtocol, count int32) {
	t.Helper()
	ctx := context.Background()
	if err := p.WriteMessageBegin(ctx, "m", CALL, 1); err != nil {
		t.Fatal(err)
	}
	if err := p.WriteFieldBegin(ctx, "f", LIST, 1); err != nil {
		t.Fatal(err)
	}
	if err := p.WriteListBegin(ctx, STRUCT, int(count)); err != nil {
		t.Fatal(err)
	}
	if err := p.Flush(ctx); err != nil {
		t.Fatal(err)
	}
}

func readListHeader(t *testing.T, p TProtocol) (int, error) {
	t.Helper()
	ctx := context.Background()
	if _, _, _, err := p.ReadMessageBegin(ctx); err != nil {
		t.Fatalf("ReadMessageBegin: %v", err)
	}
	if _, _, _, err := p.ReadFieldBegin(ctx); err != nil {
		t.Fatalf("ReadFieldBegin: %v", err)
	}
	_, size, err := p.ReadListBegin(ctx)
	return size, err
}

// A wire-declared element count that the remaining bytes cannot possibly back
// must be rejected before it is handed to the caller, which uses it to size an
// allocation.
func TestContainerSizeExceedsRemainingBytes(t *testing.T) {
	const bogusCount = 1000000

	for _, c := range []struct {
		name    string
		factory TProtocolFactory
	}{
		{"binary", NewTBinaryProtocolFactoryConf(nil)},
		{"compact", NewTCompactProtocolFactoryConf(nil)},
		{"json", NewTJSONProtocolFactory()},
	} {
		t.Run(c.name, func(t *testing.T) {
			trans := NewTMemoryBuffer()
			writeTruncatedListHeader(t, c.factory.GetProtocol(trans), bogusCount)

			payload := trans.Bytes()
			t.Logf("%d byte payload declares %d elements", len(payload), bogusCount)

			read := NewTMemoryBuffer()
			read.Write(payload)
			size, err := readListHeader(t, c.factory.GetProtocol(read))
			if err == nil {
				t.Fatalf("ReadListBegin accepted size %d with only %d bytes of payload",
					size, len(payload))
			}
			var te TProtocolException
			if !errors.As(err, &te) || te.TypeId() != SIZE_LIMIT {
				t.Errorf("expected a SIZE_LIMIT TProtocolException, got %#v", err)
			}
		})
	}
}

// The same count must still be accepted when the transport cannot report how
// many bytes remain: rejecting there would break streaming transports.
func TestContainerSizeUnknownRemainingBytes(t *testing.T) {
	const count = 1000000

	buf := NewTMemoryBuffer()
	writeTruncatedListHeader(t, NewTBinaryProtocolConf(buf, nil), count)

	// StreamTransport reports UnknownRemainingBytes.
	trans := NewStreamTransportR(bytes.NewReader(buf.Bytes()))
	if got := trans.RemainingBytes(); got != UnknownRemainingBytes {
		t.Fatalf("precondition: StreamTransport.RemainingBytes() = %d, want UnknownRemainingBytes", got)
	}
	size, err := readListHeader(t, NewTBinaryProtocolConf(trans, nil))
	if err != nil {
		t.Fatalf("ReadListBegin rejected size on an unknown-size transport: %v", err)
	}
	if size != count {
		t.Errorf("size = %d, want %d", size, count)
	}
}

// A container that the remaining bytes can back must be read unchanged. This is
// the regression guard for the layered transports whose RemainingBytes() is not
// a bound on their own readable bytes.
func TestContainerSizeValidPayloadRoundTrip(t *testing.T) {
	const count = 300

	writePayload := func(t *testing.T, p TProtocol) {
		t.Helper()
		ctx := context.Background()
		if err := p.WriteMessageBegin(ctx, "m", CALL, 1); err != nil {
			t.Fatal(err)
		}
		if err := p.WriteFieldBegin(ctx, "f", LIST, 1); err != nil {
			t.Fatal(err)
		}
		if err := p.WriteListBegin(ctx, I64, count); err != nil {
			t.Fatal(err)
		}
		for i := 0; i < count; i++ {
			if err := p.WriteI64(ctx, int64(i)); err != nil {
				t.Fatal(err)
			}
		}
		if err := p.WriteListEnd(ctx); err != nil {
			t.Fatal(err)
		}
		if err := p.WriteFieldEnd(ctx); err != nil {
			t.Fatal(err)
		}
		if err := p.WriteFieldStop(ctx); err != nil {
			t.Fatal(err)
		}
		if err := p.WriteMessageEnd(ctx); err != nil {
			t.Fatal(err)
		}
		if err := p.Flush(ctx); err != nil {
			t.Fatal(err)
		}
	}

	// Each case layers a transport over a memory buffer that holds exactly one
	// message, so the underlying buffer is drained by the time the protocol
	// reads the list header.
	cases := []struct {
		name string
		wrap func(TTransport) (TTransport, error)
	}{
		{
			name: "memory",
			wrap: func(t TTransport) (TTransport, error) { return t, nil },
		},
		{
			name: "framed",
			wrap: func(t TTransport) (TTransport, error) {
				return NewTFramedTransportConf(t, nil), nil
			},
		},
		{
			name: "buffered",
			wrap: func(t TTransport) (TTransport, error) {
				return NewTBufferedTransport(t, 8192), nil
			},
		},
		{
			name: "zlib",
			wrap: func(t TTransport) (TTransport, error) {
				return NewTZlibTransport(t, 6)
			},
		},
		{
			name: "header",
			wrap: func(t TTransport) (TTransport, error) {
				return NewTHeaderTransportConf(t, nil), nil
			},
		},
		{
			name: "header-zlib",
			wrap: func(t TTransport) (TTransport, error) {
				return NewTHeaderTransportConf(t, &TConfiguration{
					THeaderTransforms: []THeaderTransformID{TransformZlib},
				}), nil
			},
		},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			ctx := context.Background()
			mem := NewTMemoryBuffer()

			w, err := c.wrap(mem)
			if err != nil {
				t.Fatal(err)
			}
			writePayload(t, NewTBinaryProtocolConf(w, nil))

			// The serialized bytes go into the buffer as they are; only the
			// read side is layered on top of them.
			inner := NewTMemoryBufferLen(mem.Len())
			if _, err := inner.Write(mem.Bytes()); err != nil {
				t.Fatal(err)
			}
			r, err := c.wrap(inner)
			if err != nil {
				t.Fatal(err)
			}

			p := NewTBinaryProtocolConf(r, nil)
			if _, _, _, err := p.ReadMessageBegin(ctx); err != nil {
				t.Fatalf("ReadMessageBegin: %v", err)
			}
			if _, _, _, err := p.ReadFieldBegin(ctx); err != nil {
				t.Fatalf("ReadFieldBegin: %v", err)
			}
			_, size, err := p.ReadListBegin(ctx)
			if err != nil {
				t.Fatalf("ReadListBegin rejected a valid %d element list: %v", count, err)
			}
			if size != count {
				t.Fatalf("size = %d, want %d", size, count)
			}
			for i := 0; i < size; i++ {
				v, err := p.ReadI64(ctx)
				if err != nil {
					t.Fatalf("ReadI64 %d: %v", i, err)
				}
				if v != int64(i) {
					t.Fatalf("element %d = %d", i, v)
				}
			}
		})
	}
}

// PreallocSize bounds what a wire-supplied count may reserve up front, while
// leaving counts that are already small alone.
func TestPreallocSize(t *testing.T) {
	for _, c := range []struct {
		in, want int
	}{
		{-1, 0},
		{0, 0},
		{1, 1},
		{maxPreallocSize - 1, maxPreallocSize - 1},
		{maxPreallocSize, maxPreallocSize},
		{maxPreallocSize + 1, maxPreallocSize},
		{1000000, maxPreallocSize},
	} {
		if got := PreallocSize(c.in); got != c.want {
			t.Errorf("PreallocSize(%d) = %d, want %d", c.in, got, c.want)
		}
	}
}

// The map counterpart of the reported allocation: a bogus count must not size
// the map hint either.
func TestPreallocSizeBoundsAllocation(t *testing.T) {
	const bogusCount = 1000000

	slice := make([]*int64, 0, PreallocSize(bogusCount))
	if cap(slice) != maxPreallocSize {
		t.Errorf("slice cap = %d, want %d", cap(slice), maxPreallocSize)
	}

	m := make(map[int64]int64, PreallocSize(bogusCount))
	if len(m) != 0 {
		t.Errorf("map len = %d, want 0", len(m))
	}
}

func TestUnknownRemainingBytesSentinel(t *testing.T) {
	// Every transport that cannot tell how much is left must use the same
	// sentinel, since that is what suppresses the container-count bound.
	for _, c := range []struct {
		name  string
		trans TTransport
	}{
		{"socket", &TSocket{}},
		{"ssl socket", &TSSLSocket{}},
		{"stream", NewStreamTransportR(bytes.NewReader(nil))},
	} {
		if got := c.trans.RemainingBytes(); got != UnknownRemainingBytes {
			t.Errorf("%s.RemainingBytes() = %d, want UnknownRemainingBytes", c.name, got)
		}
	}
}

// A fresh TFramedTransport reports 0 remaining bytes until it has pulled a
// frame. Reading a container header pulls one first, because the header's own
// bytes come through Read. Guard the ordering: if it ever changed, every
// container read on a fresh framed transport would fail with SIZE_LIMIT.
func TestContainerSizeFreshFramedTransport(t *testing.T) {
	const count = 3
	ctx := context.Background()

	mem := NewTMemoryBuffer()
	w := NewTBinaryProtocolConf(NewTFramedTransportConf(mem, nil), nil)
	if err := w.WriteListBegin(ctx, I64, count); err != nil {
		t.Fatal(err)
	}
	for i := 0; i < count; i++ {
		if err := w.WriteI64(ctx, int64(i)); err != nil {
			t.Fatal(err)
		}
	}
	if err := w.WriteListEnd(ctx); err != nil {
		t.Fatal(err)
	}
	if err := w.Flush(ctx); err != nil {
		t.Fatal(err)
	}

	inner := NewTMemoryBuffer()
	if _, err := inner.Write(mem.Bytes()); err != nil {
		t.Fatal(err)
	}
	trans := NewTFramedTransportConf(inner, nil)
	if got := trans.RemainingBytes(); got != 0 {
		t.Logf("fresh framed transport reports %d remaining bytes", got)
	}

	// ReadListBegin as the very first protocol operation on the transport.
	_, size, err := NewTBinaryProtocolConf(trans, nil).ReadListBegin(ctx)
	if err != nil {
		t.Fatalf("first container read on a fresh framed transport failed: %v", err)
	}
	if size != count {
		t.Errorf("size = %d, want %d", size, count)
	}
}

// TSimpleJSONProtocol reads container headers through its own
// ParseElemListBegin/ReadMapBegin rather than the TJSONProtocol overrides, so
// the element-count bound has to be wired into both.
func TestSimpleJSONContainerSizeExceedsRemainingBytes(t *testing.T) {
	const bogusCount = 1000000
	ctx := context.Background()

	for _, c := range []struct {
		name  string
		write func(TProtocol) error
		read  func(TProtocol) (int, error)
	}{
		{
			name:  "list",
			write: func(p TProtocol) error { return p.WriteListBegin(ctx, STRUCT, bogusCount) },
			read:  func(p TProtocol) (int, error) { _, size, err := p.ReadListBegin(ctx); return size, err },
		},
		{
			name:  "set",
			write: func(p TProtocol) error { return p.WriteSetBegin(ctx, STRUCT, bogusCount) },
			read:  func(p TProtocol) (int, error) { _, size, err := p.ReadSetBegin(ctx); return size, err },
		},
		{
			name:  "map",
			write: func(p TProtocol) error { return p.WriteMapBegin(ctx, I64, I64, bogusCount) },
			read: func(p TProtocol) (int, error) {
				_, _, size, err := p.ReadMapBegin(ctx)
				return size, err
			},
		},
	} {
		t.Run(c.name, func(t *testing.T) {
			trans := NewTMemoryBuffer()
			p := NewTSimpleJSONProtocol(trans)
			if err := c.write(p); err != nil {
				t.Fatal(err)
			}
			if err := p.Flush(ctx); err != nil {
				t.Fatal(err)
			}

			payload := trans.Bytes()
			t.Logf("%d byte payload declares %d elements", len(payload), bogusCount)

			read := NewTMemoryBuffer()
			read.Write(payload)
			size, err := c.read(NewTSimpleJSONProtocol(read))
			if err == nil {
				t.Fatalf("accepted size %d with only %d bytes of payload", size, len(payload))
			}
			var te TProtocolException
			if !errors.As(err, &te) || te.TypeId() != SIZE_LIMIT {
				t.Errorf("expected a SIZE_LIMIT TProtocolException, got %#v", err)
			}
		})
	}
}

// A StreamTransport reports unknown unless the caller knows how much is
// readable; a known size must be reported so the container check applies.
func TestStreamTransportKnownRemainingBytes(t *testing.T) {
	trans := NewStreamTransportR(bytes.NewReader([]byte("payload")))
	if got := trans.RemainingBytes(); got != UnknownRemainingBytes {
		t.Errorf("default RemainingBytes() = %d, want UnknownRemainingBytes", got)
	}

	trans.setKnownSize(7)
	if got := trans.RemainingBytes(); got != 7 {
		t.Errorf("RemainingBytes() = %d, want 7", got)
	}

	// A negative Content-Length (chunked, or absent) means unknown, and zero
	// carries no information worth acting on either.
	for _, size := range []int64{-1, 0} {
		trans := NewStreamTransportR(bytes.NewReader(nil))
		trans.setKnownSize(size)
		if got := trans.RemainingBytes(); got != UnknownRemainingBytes {
			t.Errorf("setKnownSize(%d): RemainingBytes() = %d, want UnknownRemainingBytes", size, got)
		}
	}
}

type capturingProtocolFactory struct {
	trans TTransport
}

func (f *capturingProtocolFactory) GetProtocol(t TTransport) TProtocol {
	f.trans = t
	return NewTBinaryProtocolConf(t, nil)
}

type stubProcessor struct{}

func (stubProcessor) Process(ctx context.Context, in, out TProtocol) (bool, TException) {
	return true, nil
}
func (stubProcessor) ProcessorMap() map[string]TProcessorFunction  { return nil }
func (stubProcessor) AddToProcessorMap(string, TProcessorFunction) {}

// Thrift's own HTTP server hands the decoder a StreamTransport over the request
// body. Without the request's Content-Length that transport reports unknown, so
// the container check cannot fire on the one server the library ships itself.
func TestThriftHandlerFuncReportsContentLength(t *testing.T) {
	body := []byte("a thrift payload")

	inFactory := &capturingProtocolFactory{}
	handler := NewThriftHandlerFunc(stubProcessor{}, inFactory, &capturingProtocolFactory{})

	req := httptest.NewRequest(http.MethodPost, "/", bytes.NewReader(body))
	if req.ContentLength != int64(len(body)) {
		t.Fatalf("precondition: request ContentLength = %d, want %d", req.ContentLength, len(body))
	}
	handler(httptest.NewRecorder(), req)

	if inFactory.trans == nil {
		t.Fatal("handler never built an input protocol")
	}
	if got, want := inFactory.trans.RemainingBytes(), uint64(len(body)); got != want {
		t.Errorf("RemainingBytes() = %d, want %d", got, want)
	}
}
