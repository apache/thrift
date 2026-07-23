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

// byteCountingTransport wraps a TTransport and records how many bytes have been
// read from it, so a test can assert that an oversized delimited JSON value is
// rejected as it grows rather than buffered in full.
type byteCountingTransport struct {
	TTransport
	bytesRead int
}

func (t *byteCountingTransport) Read(buf []byte) (int, error) {
	n, err := t.TTransport.Read(buf)
	t.bytesRead += n
	return n, err
}

func jsonProtoReadingMaxSize(t *testing.T, payload string, maxSize int32) (*TSimpleJSONProtocol, *byteCountingTransport) {
	t.Helper()
	inner := NewTMemoryBuffer()
	inner.WriteString(payload)
	inner.Flush(context.Background())
	trans := &byteCountingTransport{TTransport: inner}
	return NewTSimpleJSONProtocolConf(trans, &TConfiguration{MaxMessageSize: maxSize}), trans
}

func requireSizeLimit(t *testing.T, err error, what string) {
	t.Helper()
	if err == nil {
		t.Fatalf("%s: expected a SIZE_LIMIT error, got nil (oversized value accepted)", what)
	}
	terr, ok := err.(TProtocolException)
	if !ok || terr.TypeId() != SIZE_LIMIT {
		t.Fatalf("%s: got %v, want a TProtocolException with TypeId SIZE_LIMIT (%d)", what, err, SIZE_LIMIT)
	}
}

// A JSON string comfortably under the configured limit is read unchanged.
func TestSimpleJSONStringWithinMaxSize(t *testing.T) {
	value := strings.Repeat("A", 512)
	p, _ := jsonProtoReadingMaxSize(t, jsonQuote(value), 4096)
	v, err := p.ReadString(context.Background())
	if err != nil {
		t.Fatalf("ReadString within limit failed: %v", err)
	}
	if v != value {
		t.Fatalf("ReadString returned %d bytes, want %d", len(v), len(value))
	}
}

// A single JSON string larger than the configured limit is rejected. Unlike a
// length-prefixed binary string there is no declared size to reject up front, so
// this exercises the running check in ParseStringBody.
func TestSimpleJSONStringExceedsMaxSize(t *testing.T) {
	p, _ := jsonProtoReadingMaxSize(t, jsonQuote(strings.Repeat("A", 4096)), 1024)
	_, err := p.ReadString(context.Background())
	requireSizeLimit(t, err, "ReadString over limit")
}

// A JSON numeric literal longer than the configured limit is rejected. Numbers
// are read character-by-character with no declared length (readNumeric).
func TestSimpleJSONNumberExceedsMaxSize(t *testing.T) {
	p, _ := jsonProtoReadingMaxSize(t, strings.Repeat("1", 4096), 1024)
	_, err := p.ReadI64(context.Background())
	requireSizeLimit(t, err, "ReadI64 over limit")
}

// A base64-encoded binary field longer than the configured limit is rejected
// (ParseBase64EncodedBody), before any decode is attempted.
func TestSimpleJSONBinaryExceedsMaxSize(t *testing.T) {
	p, _ := jsonProtoReadingMaxSize(t, `"`+strings.Repeat("A", 4096)+`"`, 1024)
	_, err := p.ReadBinary(context.Background())
	requireSizeLimit(t, err, "ReadBinary over limit")
}

// The decisive test: a huge unbroken run must be rejected as it grows, so the
// bytes actually read from the transport stay bounded by the limit -- it must
// not be buffered in full first. This is what distinguishes bounding the read
// (ReadSlice loop) from checking the size only after the whole value has already
// been slurped by bufio.
func TestSimpleJSONStringReadIsBounded(t *testing.T) {
	const maxSize = 1024
	payload := `"` + strings.Repeat("A", 1<<20) + `"` // ~1 MiB, no closing quote until the very end
	p, trans := jsonProtoReadingMaxSize(t, payload, maxSize)
	_, err := p.ReadString(context.Background())
	requireSizeLimit(t, err, "ReadString over limit (bounded)")
	if trans.bytesRead > maxSize+64*1024 {
		t.Fatalf(
			"read %d bytes from the transport for a %d-byte limit; the oversized string was "+
				"buffered in full instead of being rejected as it grew",
			trans.bytesRead, maxSize,
		)
	}
}

// A string larger than one bufio buffer (>4096 bytes) but within the limit must
// be reassembled correctly across ReadSlice chunks -- this exercises the
// multi-buffer accumulation path (the ErrBufferFull branch of readBinaryBounded),
// which the shorter within-limit case reads in a single chunk and never covers.
func TestSimpleJSONStringMultiChunkWithinMaxSize(t *testing.T) {
	value := strings.Repeat("Z", 10000) // spans several 4096-byte bufio buffers
	p, _ := jsonProtoReadingMaxSize(t, jsonQuote(value), 1<<20)
	v, err := p.ReadString(context.Background())
	if err != nil {
		t.Fatalf("ReadString of a %d-byte string failed: %v", len(value), err)
	}
	if v != value {
		t.Fatalf("multi-chunk ReadString corrupted the value: got %d bytes, want %d", len(v), len(value))
	}
}

// The structural end of an object/list is also read up to its closing
// delimiter, so a run of non-'}' bytes before '}' must be bounded too --
// otherwise a peer could exhaust memory with e.g. `{"1":{"i32":5<huge run>}`
// (the run is read in full before the "expected end of object" check rejects
// it) even after every string value is bounded. Leading whitespace is discarded
// byte-by-byte by readIfNull, so a non-whitespace run is the accumulation path.
func TestSimpleJSONObjectEndIsBounded(t *testing.T) {
	const maxSize = 1024
	payload := `{` + strings.Repeat("A", 1<<20) + `}` // ~1 MiB of non-'}' bytes before '}'
	p, trans := jsonProtoReadingMaxSize(t, payload, maxSize)
	if _, err := p.ParseObjectStart(); err != nil {
		t.Fatalf("ParseObjectStart failed: %v", err)
	}
	err := p.ParseObjectEnd()
	requireSizeLimit(t, err, "ParseObjectEnd over limit")
	if trans.bytesRead > maxSize+64*1024 {
		t.Fatalf(
			"read %d bytes closing an object with a %d-byte limit; the run before '}' was buffered in full",
			trans.bytesRead, maxSize,
		)
	}
}

// The list end has the same shape as the object end (read up to ']'), so it
// must be bounded too. Read one element to reach the list context, then feed a
// long run of non-']' bytes before ']'.
func TestSimpleJSONListEndIsBounded(t *testing.T) {
	const maxSize = 1024
	payload := `[0,` + strings.Repeat("A", 1<<20) + `]` // ~1 MiB of non-']' bytes before ']'
	p, trans := jsonProtoReadingMaxSize(t, payload, maxSize)
	if _, err := p.ParseListBegin(); err != nil {
		t.Fatalf("ParseListBegin failed: %v", err)
	}
	if _, err := p.ReadI64(context.Background()); err != nil {
		t.Fatalf("reading first list element failed: %v", err)
	}
	err := p.ParseListEnd()
	requireSizeLimit(t, err, "ParseListEnd over limit")
	if trans.bytesRead > maxSize+64*1024 {
		t.Fatalf(
			"read %d bytes closing a list with a %d-byte limit; the run before ']' was buffered in full",
			trans.bytesRead, maxSize,
		)
	}
}
