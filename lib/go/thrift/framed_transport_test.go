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
	"io"
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
		for i := 0; i < n; i++ {
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
		for i := 0; i < n; i++ {
			if _, err := io.Copy(writer, strings.NewReader(content)); err != nil {
				t.Fatalf("Failed to write on #%d: %v", i, err)
			}
			if err := writer.Flush(context.Background()); err != nil {
				t.Fatalf("Failed to flush on #%d: %v", i, err)
			}
		}

		// read
		for i := 0; i < n; i++ {
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
