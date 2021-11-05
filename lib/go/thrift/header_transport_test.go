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
	"fmt"
	"io"
	"io/ioutil"
	"strings"
	"testing"
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
	read, err := ioutil.ReadAll(reader)
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
