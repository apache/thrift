/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * 'License'); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package tests

import (
	"context"
	"testing"
	"testing/quick"

	"github.com/apache/thrift/lib/go/test/gopath/src/thrifttest"
	"github.com/apache/thrift/lib/go/thrift"
)

func TestDeserializerPoolJSONProtocol(t *testing.T) {
	ctx := context.Background()

	serializerPool := thrift.NewTSerializerPoolSizeFactory(1024, thrift.NewTJSONProtocolFactory())
	msg := &thrifttest.Bonk{
		Message: "foo",
		Type:    42,
	}
	valid, err := serializerPool.WriteString(ctx, msg)
	if err != nil {
		t.Fatal(err)
	}
	invalid := valid[:len(valid)-2]

	deserializerPool := thrift.NewTDeserializerPoolSizeFactory(1024, thrift.NewTJSONProtocolFactory())
	msg = new(thrifttest.Bonk)
	if err := deserializerPool.ReadString(ctx, msg, invalid); err == nil {
		t.Fatalf("Deserializing %q did not fail", invalid)
	}

	f := func() bool {
		msg := new(thrifttest.Bonk)
		if err := deserializerPool.ReadString(ctx, msg, valid); err != nil {
			t.Errorf("Deserializing string %q failed with %v", valid, err)
		}
		if err := deserializerPool.Read(ctx, msg, []byte(valid)); err != nil {
			t.Errorf("Deserializing bytes %q failed with %v", valid, err)
		}
		return !t.Failed()
	}
	if err := quick.Check(f, nil); err != nil {
		t.Error(err)
	}
}
