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
	"io/ioutil"
	"testing"
)

func TestTHeaderHeadersReadWrite(t *testing.T) {
	trans := NewTMemoryBuffer()
	reader := NewTHeaderTransport(trans)
	writer := NewTHeaderTransport(trans)

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
	if err := reader.ReadFrame(); err != nil {
		t.Errorf("reader.ReadFrame returned error: %v", err)
	}
	if err := reader.ReadFrame(); err != nil {
		t.Errorf("reader.ReadFrame returned error: %v", err)
	}
	read, err := ioutil.ReadAll(reader)
	if err != nil {
		t.Errorf("Read returned error: %v", err)
	}
	if err := reader.ReadFrame(); err != nil && err != io.EOF {
		t.Errorf("reader.ReadFrame returned error: %v", err)
	}
	if string(read) != payload1+payload2 {
		t.Errorf(
			"Read content expected %q, got %q",
			payload1+payload2,
			read,
		)
	}
	if prot := reader.Protocol(); prot != THeaderProtocolBinary {
		t.Errorf(
			"reader.Protocol() expected %d, got %d",
			THeaderProtocolBinary,
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

func TestTHeaderTransportNoDoubleWrapping(t *testing.T) {
	trans := NewTMemoryBuffer()
	orig := NewTHeaderTransport(trans)
	wrapped := NewTHeaderTransport(orig)

	if wrapped != orig {
		t.Errorf("NewTHeaderTransport double wrapped THeaderTransport")
	}
}
