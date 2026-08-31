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
	"io"
	"runtime"
	"strings"
	"testing"
	"testing/iotest"
)

func TestFramedTransport(t *testing.T) {
	trans := NewTFramedTransport(NewTMemoryBuffer())
	TransportTest(t, trans, trans)
}

func TestTFramedTransportReuseTransport(t *testing.T) {
	const (
		content = "Hello, world!"
		n       = 10
	)
	trans := NewTMemoryBuffer()
	reader := NewTFramedTransport(trans)
	writer := NewTFramedTransport(trans)

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

// stackDepthTransport records the call-stack depth observed on every Read of
// the underlying transport, so that a test can tell whether TFramedTransport
// consumes successive frames iteratively or by re-entering itself once per
// frame.
type stackDepthTransport struct {
	TTransport

	depths []int
}

func (s *stackDepthTransport) Read(p []byte) (int, error) {
	var pcs [8192]uintptr
	s.depths = append(s.depths, runtime.Callers(0, pcs[:]))
	return s.TTransport.Read(p)
}

func TestTFramedTransportEmptyFrames(t *testing.T) {
	// An empty frame carries no payload, so Read has to move on to the next
	// frame to satisfy its caller. It must do so without adding a stack
	// frame per empty frame: a peer can send them 4 bytes at a time.
	const (
		emptyFrames = 100000
		content     = "hello, world!"
	)

	var raw bytes.Buffer
	for range emptyFrames {
		raw.Write([]byte{0, 0, 0, 0})
	}
	binary.Write(&raw, binary.BigEndian, uint32(len(content)))
	raw.WriteString(content)

	base := &stackDepthTransport{TTransport: NewStreamTransportR(&raw)}
	trans := NewTFramedTransport(base)

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
