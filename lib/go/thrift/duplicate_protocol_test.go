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
	"encoding/json"
	"testing"
)

func TestDuplicateToJSONReadMakesEqualJSON(t *testing.T) {
	delegateBuf := NewTMemoryBuffer()
	iprot := NewTJSONProtocolFactory().GetProtocol(delegateBuf)

	s := NewMyTestStruct()
	ctx := context.Background()
	s.Write(ctx, iprot)

	iprot.Flush(ctx)
	jsonBytesWritten := delegateBuf.Bytes()

	if err := json.Unmarshal(jsonBytesWritten, NewMyTestStruct()); err != nil {
		t.Errorf("error unmarshaling written bytes: %s", err)
	}

	duplicateBuf := NewTMemoryBuffer()
	dupProto := NewTJSONProtocolFactory().GetProtocol(duplicateBuf)
	proto := &TDuplicateToProtocol{
		Delegate:    NewTJSONProtocolFactory().GetProtocol(delegateBuf),
		DuplicateTo: dupProto,
	}

	if err := s.Read(ctx, proto); err != nil {
		t.Errorf("error reading struct from DuplicateTo: %s", err)
	}
	dupProto.Flush(ctx)

	jsonBytesRead := duplicateBuf.Bytes()

	if !bytes.Equal(jsonBytesWritten, jsonBytesRead) {
		t.Errorf(`bytes read into duplicate do not equal bytes written
		read:
		%s
		written:
		%s
		`, jsonBytesRead, jsonBytesWritten)
	}

	dup := NewMyTestStruct()
	if err := dup.Read(ctx, dupProto); err != nil {
		t.Errorf("error reading struct from duplicated protocol: %s", err)
	}
}
