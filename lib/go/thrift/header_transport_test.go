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
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"math"
	"runtime"
	"strings"
	"testing"
	"testing/iotest"
	"testing/quick"
)

func testTHeaderHeadersReadWriteProtocolID(t *testing.T, protoID THeaderProtocolID) {
	trans := NewTMemoryBuffer()
	reader := NewTHeaderTransport(trans)
	writer := NewTHeaderTransportConf(trans, &TConfiguration{
		THeaderProtocolID: &protoID,
	})

	const key1 = "key1"
	const value1 = "value1"
	const key2 = "key2"
	const value2 = "value2"
	const payload1 = "hello, world1\n"
	const payload2 = "hello, world2\n"

	// Write
	if err := writer.AddTransform(TransformZlib); err != nil {
		t.Fatalf(
			"writer.AddTransform(TransformZlib) returned error: %v",
			err,
		)
	}
	// Use double zlib to make sure that we close them in the right order.
	if err := writer.AddTransform(TransformZlib); err != nil {
		t.Fatalf(
			"writer.AddTransform(TransformZlib) returned error: %v",
			err,
		)
	}
	if err := writer.AddTransform(TransformNone); err != nil {
		t.Fatalf(
			"writer.AddTransform(TransformNone) returned error: %v",
			err,
		)
	}
	writer.SetWriteHeader(key1, value1)
	writer.SetWriteHeader(key2, value2)
	if _, err := writer.Write([]byte(payload1)); err != nil {
		t.Errorf("writer.Write returned error: %v", err)
	}
	if err := writer.Flush(context.Background()); err != nil {
		t.Errorf("writer.Flush returned error: %v", err)
	}
	if _, err := writer.Write([]byte(payload2)); err != nil {
		t.Errorf("writer.Write returned error: %v", err)
	}
	if err := writer.Flush(context.Background()); err != nil {
		t.Errorf("writer.Flush returned error: %v", err)
	}

	// Read

	// Make sure multiple calls to ReadFrame is fine.
	if err := reader.ReadFrame(context.Background()); err != nil {
		t.Errorf("reader.ReadFrame returned error: %v", err)
	}
	if err := reader.ReadFrame(context.Background()); err != nil {
		t.Errorf("reader.ReadFrame returned error: %v", err)
	}
	read, err := io.ReadAll(reader)
	if err != nil {
		t.Errorf("Read returned error: %v", err)
	}
	if err := reader.ReadFrame(context.Background()); err != nil && err != io.EOF {
		t.Errorf("reader.ReadFrame returned error: %v", err)
	}
	if string(read) != payload1+payload2 {
		t.Errorf(
			"Read content expected %q, got %q",
			payload1+payload2,
			read,
		)
	}
	if prot := reader.Protocol(); prot != protoID {
		t.Errorf(
			"reader.Protocol() expected %d, got %d",
			protoID,
			prot,
		)
	}
	if reader.clientType != clientHeaders {
		t.Errorf(
			"reader.clientType expected %d, got %d",
			clientHeaders,
			reader.clientType,
		)
	}
	headers := reader.GetReadHeaders()
	if len(headers) != 2 || headers[key1] != value1 || headers[key2] != value2 {
		t.Errorf(
			"reader.GetReadHeaders() expected size 2, actual content: %+v",
			headers,
		)
	}
}

func TestTHeaderHeadersReadWrite(t *testing.T) {
	for label, id := range map[string]THeaderProtocolID{
		"default": THeaderProtocolDefault,
		"binary":  THeaderProtocolBinary,
		"compact": THeaderProtocolCompact,
	} {
		t.Run(label, func(t *testing.T) {
			testTHeaderHeadersReadWriteProtocolID(t, id)
		})
	}
}

func TestTHeaderTransportNoDoubleWrapping(t *testing.T) {
	trans := NewTMemoryBuffer()
	orig := NewTHeaderTransport(trans)
	wrapped := NewTHeaderTransport(orig)

	if wrapped != orig {
		t.Errorf("NewTHeaderTransport double wrapped THeaderTransport")
	}
}

func TestTHeaderTransportNoReadBeyondFrame(t *testing.T) {
	trans := NewTMemoryBuffer()
	writeContent := func(writer TTransport, content string) error {
		if _, err := io.Copy(writer, strings.NewReader(content)); err != nil {
			return err
		}
		if err := writer.Flush(context.Background()); err != nil {
			return err
		}
		return nil
	}
	f := func(content string) bool {
		trans.Reset()
		if len(content) == 0 {
			return true
		}

		reader := NewTHeaderTransport(trans)
		writer := NewTHeaderTransport(trans)
		// Write content twice
		if err := writeContent(writer, content); err != nil {
			t.Error(err)
		}
		if err := writeContent(writer, content); err != nil {
			t.Error(err)
		}
		// buf is big enough to read both content out,
		// but it shouldn't read beyond the first one in a single Read call.
		buf := make([]byte, len(content)*3)
		read, err := reader.Read(buf)
		if err != nil {
			t.Error(err)
		}
		if read == 0 || read > len(content) {
			t.Errorf(
				"Expected read in no more than %d:%q, got %d:%q",
				len(content),
				content,
				read,
				buf[:read],
			)
		}

		// Check for endOfFrame handling
		if !reader.needReadFrame() {
			t.Error("Expected needReadFrame to be true after read the frame fully, got false")
		}
		return !t.Failed()
	}
	if err := quick.Check(f, nil); err != nil {
		t.Error(err)
	}
}

func TestTHeaderTransportEndOfFrameHandling(t *testing.T) {
	trans := NewTMemoryBuffer()
	writeContent := func(writer TTransport, content string) error {
		if _, err := io.Copy(writer, strings.NewReader(content)); err != nil {
			return err
		}
		if err := writer.Flush(context.Background()); err != nil {
			return err
		}
		return nil
	}

	readFully := func(content string) bool {
		trans.Reset()
		if len(content) == 0 {
			return true
		}

		reader := NewTHeaderTransport(trans)
		writer := NewTHeaderTransport(trans)
		// Write content
		if err := writeContent(writer, content); err != nil {
			t.Error(err)
		}
		buf := make([]byte, len(content))
		_, err := reader.Read(buf)
		if err != nil {
			t.Error(err)
		}
		if !reader.needReadFrame() {
			t.Error("Expected needReadFrame to be true after read the frame fully, got false")
		}
		return !t.Failed()
	}
	if err := quick.Check(readFully, nil); err != nil {
		t.Error(err)
	}

	readPartially := func(content string) bool {
		trans.Reset()
		if len(content) < 1 {
			return true
		}

		reader := NewTHeaderTransport(trans)
		writer := NewTHeaderTransport(trans)
		// Write content
		if err := writeContent(writer, content); err != nil {
			t.Error(err)
		}
		// Make the buf smaller so it can't read fully
		buf := make([]byte, len(content)-1)
		_, err := reader.Read(buf)
		if err != nil {
			t.Error(err)
		}
		if reader.needReadFrame() {
			t.Error("Expected needReadFrame to be false before read the frame fully, got true")
		}
		return !t.Failed()
	}
	if err := quick.Check(readPartially, nil); err != nil {
		t.Error(err)
	}
}

func BenchmarkTHeaderProtocolIDValidate(b *testing.B) {
	for _, c := range []THeaderProtocolID{
		THeaderProtocolBinary,
		THeaderProtocolCompact,
		-1,
	} {
		b.Run(fmt.Sprintf("%2v", c), func(b *testing.B) {
			b.RunParallel(func(pb *testing.PB) {
				for pb.Next() {
					c.Validate()
				}
			})
		})
	}
}

func TestSetTHeaderTransportProtocolID(t *testing.T) {
	const expected = THeaderProtocolCompact
	factory := NewTHeaderTransportFactoryConf(nil, &TConfiguration{
		THeaderProtocolID: THeaderProtocolIDPtrMust(expected),
	})
	buf := NewTMemoryBuffer()
	trans, err := factory.GetTransport(buf)
	if err != nil {
		t.Fatalf("Failed to get transport from factory: %v", err)
	}
	ht, ok := trans.(*THeaderTransport)
	if !ok {
		t.Fatalf("Transport is not *THeaderTransport: %#v", trans)
	}
	if actual := ht.Protocol(); actual != expected {
		t.Errorf("Expected protocol id %v, got %v", expected, actual)
	}

	ht.SetTConfiguration(&TConfiguration{})
	if actual := ht.Protocol(); actual != expected {
		t.Errorf("Expected protocol id %v, got %v", expected, actual)
	}
}

func TestTHeaderTransportReuseTransport(t *testing.T) {
	const (
		content = "Hello, world!"
		n       = 10
	)
	trans := NewTMemoryBuffer()
	reader := NewTHeaderTransport(trans)
	writer := NewTHeaderTransport(trans)

	t.Run("pair", func(t *testing.T) {
		for i := range n {
			// write
			if _, err := io.Copy(writer, strings.NewReader(content)); err != nil {
				t.Fatalf("Failed to write on #%d: %v", i, err)
			}
			if err := writer.Flush(context.Background()); err != nil {
				t.Fatalf("Failed to flush on #%d: %v", i, err)
			}

			// read
			read, err := io.ReadAll(iotest.OneByteReader(reader))
			if err != nil {
				t.Errorf("Failed to read on #%d: %v", i, err)
			}
			if string(read) != content {
				t.Errorf("Read #%d: want %q, got %q", i, content, read)
			}
		}
	})

	t.Run("batched", func(t *testing.T) {
		// write
		for i := range n {
			if _, err := io.Copy(writer, strings.NewReader(content)); err != nil {
				t.Fatalf("Failed to write on #%d: %v", i, err)
			}
			if err := writer.Flush(context.Background()); err != nil {
				t.Fatalf("Failed to flush on #%d: %v", i, err)
			}
		}

		// read
		for i := range n {
			const (
				size = len(content)
			)
			var buf []byte
			var err error
			if i%2 == 0 {
				// on even calls, use OneByteReader to make
				// sure that small reads are fine
				buf, err = io.ReadAll(io.LimitReader(iotest.OneByteReader(reader), int64(size)))
			} else {
				// on odd calls, make sure that we don't read
				// more than written per frame
				buf = make([]byte, size*2)
				var n int
				n, err = reader.Read(buf)
				buf = buf[:n]
			}
			if err != nil {
				t.Errorf("Failed to read on #%d: %v", i, err)
			}
			if string(buf) != content {
				t.Errorf("Read #%d: want %q, got %q", i, content, buf)
			}
		}
	})
}

// TestTHeaderTransportTransformCountBounded builds a frame by hand where the
// header declares a transformCount that is not backed by anywhere near that
// many transform ID entries -- the shape of an element count read straight
// off the wire, as opposed to a real transform list. It must be rejected by
// the same size check every other wire-supplied container count goes
// through, before it is used to size any allocation.
//
// The declared count (1000) is intentionally small enough that even without
// the fix, sizing allocations from it cannot exhaust memory; unpatched code
// still fails, but only later, via EOF from the transform-ID read loop
// running out of header bytes. A configured MaxMessageSize well below 1000
// distinguishes the two: fixed code rejects with SIZE_LIMIT before touching
// the read loop, unpatched code gets there and fails with EOF instead.
func TestTHeaderTransportTransformCountBounded(t *testing.T) {
	headers := NewTMemoryBuffer()
	hp := NewTCompactProtocol(headers)
	if _, err := hp.writeVarint32(int32(THeaderProtocolCompact)); err != nil {
		t.Fatalf("writeVarint32(protoID) returned error: %v", err)
	}
	const declaredTransformCount = 1000
	if _, err := hp.writeVarint32(declaredTransformCount); err != nil {
		t.Fatalf("writeVarint32(transformCount) returned error: %v", err)
	}
	// No transform ID entries follow, unlike a legitimate frame.

	if padding := 4 - headers.Len()%4; padding < 4 {
		if _, err := headers.Write(make([]byte, padding)); err != nil {
			t.Fatalf("headers.Write(padding) returned error: %v", err)
		}
	}

	meta := headerMeta{
		MagicFlags:   THeaderHeaderMagic,
		SequenceID:   0,
		HeaderLength: uint16(headers.Len() / 4),
	}
	frame := NewTMemoryBuffer()
	if err := binary.Write(frame, binary.BigEndian, meta); err != nil {
		t.Fatalf("binary.Write(meta) returned error: %v", err)
	}
	if _, err := io.Copy(frame, headers); err != nil {
		t.Fatalf("io.Copy(headers) returned error: %v", err)
	}

	trans := NewTMemoryBuffer()
	frameSizeBuf := make([]byte, size32)
	binary.BigEndian.PutUint32(frameSizeBuf, uint32(frame.Len()))
	if _, err := trans.Write(frameSizeBuf); err != nil {
		t.Fatalf("trans.Write(frameSize) returned error: %v", err)
	}
	if _, err := io.Copy(trans, frame); err != nil {
		t.Fatalf("io.Copy(trans, frame) returned error: %v", err)
	}

	reader := NewTHeaderTransportConf(trans, &TConfiguration{
		MaxMessageSize: 64,
	})
	err := reader.ReadFrame(context.Background())
	if err == nil {
		t.Fatal("ReadFrame with an over-limit transformCount unexpectedly succeeded")
	}
	terr, ok := err.(TProtocolException)
	if !ok || terr.TypeId() != SIZE_LIMIT {
		t.Fatalf(
			"ReadFrame returned %v, want a TProtocolException with TypeId SIZE_LIMIT (%d); "+
				"an EOF-shaped error here means transformCount was not bounds-checked "+
				"before being used to size an allocation",
			err, SIZE_LIMIT,
		)
	}
}

// headerDepthTransport records the call-stack depth observed on every Read of
// the underlying transport, so that a test can tell whether THeaderTransport
// consumes successive frames iteratively or by re-entering itself once per
// frame.
type headerDepthTransport struct {
	TTransport

	depths []int
}

func (h *headerDepthTransport) Read(p []byte) (int, error) {
	var pcs [8192]uintptr
	h.depths = append(h.depths, runtime.Callers(0, pcs[:]))
	return h.TTransport.Read(p)
}

// emptyPayloadHeaderFrame builds a THeader frame whose header block fills the
// whole frame, so that it carries no payload at all.
func emptyPayloadHeaderFrame(payload []byte) []byte {
	// One word of header block: protocol id 0 (binary) and a transform count
	// of 0, as varints, then padding.
	headerBlock := []byte{0x00, 0x00, 0x00, 0x00}

	var frame bytes.Buffer
	binary.Write(&frame, binary.BigEndian, THeaderHeaderMagic)
	binary.Write(&frame, binary.BigEndian, int32(0)) // sequence id
	binary.Write(&frame, binary.BigEndian, uint16(len(headerBlock)/4))
	frame.Write(headerBlock)
	frame.Write(payload)

	var out bytes.Buffer
	binary.Write(&out, binary.BigEndian, uint32(frame.Len()))
	out.Write(frame.Bytes())
	return out.Bytes()
}

func TestTHeaderTransportEmptyPayloadFrames(t *testing.T) {
	// A frame whose header block fills it carries no payload, so Read has to
	// move on to the next frame to satisfy its caller. It must do so without
	// adding a stack frame per empty frame: such a frame costs a peer 18 bytes.
	const (
		emptyFrames = 50000
		content     = "hello, world!"
	)

	var raw bytes.Buffer
	for range emptyFrames {
		raw.Write(emptyPayloadHeaderFrame(nil))
	}
	raw.Write(emptyPayloadHeaderFrame([]byte(content)))

	base := &headerDepthTransport{TTransport: NewStreamTransportR(&raw)}
	trans := NewTHeaderTransport(base)

	buf := make([]byte, len(content))
	n, err := io.ReadFull(trans, buf)
	if err != nil {
		t.Fatalf("Failed to read after %d empty frames: %v", emptyFrames, err)
	}
	if got := string(buf[:n]); got != content {
		t.Errorf("Read after %d empty frames: want %q, got %q", emptyFrames, content, got)
	}

	if len(base.depths) < 2 {
		t.Fatalf(
			"Expected the underlying transport to be read more than once, got %d reads",
			len(base.depths),
		)
	}
	first := base.depths[0]
	last := base.depths[len(base.depths)-1]
	// An iterative implementation reads every frame from the same depth, so
	// the difference is 0. Allow a small margin for the runtime rather than
	// pinning an exact number.
	if delta := last - first; delta > 16 {
		t.Errorf(
			"Stack depth grew by %d (from %d to %d) over %d reads of the underlying transport, "+
				"which means one empty frame costs one stack frame",
			delta, first, last, len(base.depths),
		)
	}
}

// poisonBufPool puts a buffer whose leftover bytes look like a compact framed
// message back into the pool, so that the next frame buffer taken from it
// would answer a version check with the previous request's data.
func poisonBufPool() {
	poison := bufPool.get()
	poison.Write(bytes.Repeat([]byte{COMPACT_PROTOCOL_ID, COMPACT_VERSION, 0xAA, 0xBB}, 200))
	poison.Reset()
	bufPool.put(&poison)
}

func TestTHeaderTransportShortFrame(t *testing.T) {
	// A frame carrying fewer than 4 bytes cannot hold the version word that
	// says what is in it. Reading one must not fall back on whatever the
	// recycled frame buffer happens to still contain.
	for _, frameSize := range []uint32{0, 1, 2, 3} {
		t.Run(fmt.Sprintf("size-%d", frameSize), func(t *testing.T) {
			poisonBufPool()

			// Fill the frame with the leading bytes of the poison, so
			// that whatever the frame is short of is supplied by the pool
			// and the four bytes together read as a compact framed message.
			trans := NewTMemoryBuffer()
			binary.Write(trans, binary.BigEndian, frameSize)
			trans.Write([]byte{COMPACT_PROTOCOL_ID, COMPACT_VERSION, 0xAA, 0xBB}[:frameSize])
			reader := NewTHeaderTransport(trans)

			err := reader.ReadFrame(context.Background())
			if err == nil {
				t.Fatalf(
					"ReadFrame accepted a %d-byte frame, client type is now %v",
					frameSize, reader.clientType,
				)
			}
			if reader.clientType != clientUnknown {
				t.Errorf(
					"ReadFrame rejected the frame but left client type %v, want %v",
					reader.clientType, clientUnknown,
				)
			}
		})
	}
}

// flushCountingTransport serves reads from in and counts, without keeping,
// every byte written to it.
type flushCountingTransport struct {
	in      io.Reader
	written int
}

func (c *flushCountingTransport) Read(p []byte) (int, error) { return c.in.Read(p) }

func (c *flushCountingTransport) Write(p []byte) (int, error) {
	c.written += len(p)
	return len(p), nil
}

func (c *flushCountingTransport) Close() error                { return nil }
func (c *flushCountingTransport) Flush(context.Context) error { return nil }
func (c *flushCountingTransport) RemainingBytes() uint64      { return UnknownRemainingBytes }
func (c *flushCountingTransport) Open() error                 { return nil }
func (c *flushCountingTransport) IsOpen() bool                { return true }

func requireFlushSizeLimit(t *testing.T, err error) {
	t.Helper()
	var pe TProtocolException
	if !errors.As(err, &pe) || pe.TypeId() != SIZE_LIMIT {
		t.Fatalf("Flush returned %v, want a TProtocolException of type SIZE_LIMIT", err)
	}
}

// headerFrameOverhead returns how many bytes a header frame written with conf
// adds to its payload, not counting the length in front of the frame.
func headerFrameOverhead(t *testing.T, conf *TConfiguration) int {
	t.Helper()
	trans := NewTMemoryBuffer()
	writer := NewTHeaderTransportConf(trans, conf)
	writer.Write([]byte{0})
	if err := writer.Flush(context.Background()); err != nil {
		t.Fatal(err)
	}
	return trans.Len() - size32 - 1
}

// Flush has to refuse a frame that ReadFrame, holding the same configuration,
// would refuse, and has to refuse it before writing any of it.
func TestTHeaderTransportFlushFrameSizeLimit(t *testing.T) {
	const limit = 1024
	conf := &TConfiguration{MaxFrameSize: limit}
	payloadLimit := limit - headerFrameOverhead(t, conf)

	t.Run("at-limit", func(t *testing.T) {
		trans := NewTMemoryBuffer()
		writer := NewTHeaderTransportConf(trans, conf)
		payload := bytes.Repeat([]byte("x"), payloadLimit)
		writer.Write(payload)
		if err := writer.Flush(context.Background()); err != nil {
			t.Fatalf("Flush refused a frame of exactly %d bytes: %v", limit, err)
		}
		if size := binary.BigEndian.Uint32(trans.Bytes()); size != limit {
			t.Fatalf("Flush wrote a frame of %d bytes, want %d", size, limit)
		}

		reader := NewTHeaderTransportConf(trans, conf)
		if err := reader.ReadFrame(context.Background()); err != nil {
			t.Fatalf("ReadFrame refused the frame Flush wrote: %v", err)
		}
		read := make([]byte, len(payload))
		if _, err := io.ReadFull(reader, read); err != nil {
			t.Fatal(err)
		}
		if !bytes.Equal(read, payload) {
			t.Error("payload read back differs from the payload written")
		}
	})

	t.Run("over-limit", func(t *testing.T) {
		out := &flushCountingTransport{in: bytes.NewReader(nil)}
		writer := NewTHeaderTransportConf(out, conf)
		writer.Write(bytes.Repeat([]byte("x"), payloadLimit+1))
		requireFlushSizeLimit(t, writer.Flush(context.Background()))
		if out.written != 0 {
			t.Errorf("Flush wrote %d bytes of a frame it refused, want 0", out.written)
		}
	})
}

// A transport that was asked in plain framing answers in plain framing, and
// that frame is held to the same limit.
func TestTHeaderTransportFlushFramedFrameSizeLimit(t *testing.T) {
	const limit = 1024
	for _, tc := range []struct {
		name       string
		protocol   func(TTransport) TProtocol
		clientType clientType
	}{
		{
			name:       "binary",
			protocol:   func(trans TTransport) TProtocol { return NewTBinaryProtocolConf(trans, nil) },
			clientType: clientFramedBinary,
		},
		{
			name:       "compact",
			protocol:   func(trans TTransport) TProtocol { return NewTCompactProtocolConf(trans, nil) },
			clientType: clientFramedCompact,
		},
	} {
		for _, size := range []int{limit, limit + 1} {
			t.Run(fmt.Sprintf("%s-%d", tc.name, size), func(t *testing.T) {
				ctx := context.Background()
				message := NewTMemoryBuffer()
				proto := tc.protocol(message)
				proto.WriteMessageBegin(ctx, "method", CALL, 1)
				proto.WriteStructBegin(ctx, "args")
				proto.WriteFieldStop(ctx)
				proto.WriteStructEnd(ctx)
				proto.WriteMessageEnd(ctx)
				proto.Flush(ctx)
				request := new(bytes.Buffer)
				binary.Write(request, binary.BigEndian, uint32(message.Len()))
				request.Write(message.Bytes())

				out := &flushCountingTransport{in: request}
				server := NewTHeaderTransportConf(out, &TConfiguration{MaxFrameSize: limit})
				if err := server.ReadFrame(ctx); err != nil {
					t.Fatal(err)
				}
				if server.clientType != tc.clientType {
					t.Fatalf("client type is %v after the request, want %v", server.clientType, tc.clientType)
				}

				server.Write(bytes.Repeat([]byte("x"), size))
				err := server.Flush(ctx)
				if size <= limit {
					if err != nil {
						t.Fatalf("Flush refused a frame of exactly %d bytes: %v", limit, err)
					}
					if want := size32 + size; out.written != want {
						t.Errorf("Flush wrote %d bytes, want %d", out.written, want)
					}
					return
				}
				requireFlushSizeLimit(t, err)
				if out.written != 0 {
					t.Errorf("Flush wrote %d bytes of a frame it refused, want 0", out.written)
				}
			})
		}
	}
}

// The header block's length travels as a 16-bit count of 4-byte words, so a
// frame cannot declare more than 4*math.MaxUint16 bytes of headers.
func TestTHeaderTransportFlushHeaderLengthLimit(t *testing.T) {
	// With a single header "k", the block holds the value plus nine bytes:
	// one each for the protocol id, the transform count, the info type, the
	// header count, the key's length and the key, and three for the value's
	// length at the sizes used here.
	const blockOverhead = 9
	maxValue := 4*math.MaxUint16 - blockOverhead

	t.Run("at-limit", func(t *testing.T) {
		trans := NewTMemoryBuffer()
		writer := NewTHeaderTransportConf(trans, &TConfiguration{})
		writer.SetWriteHeader("k", strings.Repeat("v", maxValue))
		writer.Write([]byte{0})
		if err := writer.Flush(context.Background()); err != nil {
			t.Fatalf("Flush refused a header block of exactly %d bytes: %v", 4*math.MaxUint16, err)
		}
		// Frame length, magic and flags, sequence id, then the header length.
		if words := binary.BigEndian.Uint16(trans.Bytes()[12:14]); words != math.MaxUint16 {
			t.Fatalf("header length field is %d words, want %d", words, math.MaxUint16)
		}

		reader := NewTHeaderTransportConf(trans, &TConfiguration{})
		if err := reader.ReadFrame(context.Background()); err != nil {
			t.Fatalf("ReadFrame refused the frame Flush wrote: %v", err)
		}
		if value := reader.GetReadHeaders()["k"]; len(value) != maxValue {
			t.Errorf("header read back has %d bytes, want %d", len(value), maxValue)
		}
	})

	t.Run("over-limit", func(t *testing.T) {
		out := &flushCountingTransport{in: bytes.NewReader(nil)}
		writer := NewTHeaderTransportConf(out, &TConfiguration{})
		writer.SetWriteHeader("k", strings.Repeat("v", maxValue+1))
		writer.Write([]byte{0})
		requireFlushSizeLimit(t, writer.Flush(context.Background()))
		if out.written != 0 {
			t.Errorf("Flush wrote %d bytes of a frame it refused, want 0", out.written)
		}
	})
}
