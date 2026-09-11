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

package tests

import (
	"context"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/uniondefaultvaluetest"
	"github.com/apache/thrift/lib/go/thrift"
)

func TestUnionDefaultValue(t *testing.T) {
	s := uniondefaultvaluetest.NewTestStruct()
	d := s.GetDescendant()
	if d == nil {
		t.Error("Default Union value not set!")
	}
}

func TestNilUnion(t *testing.T) {
	var d *uniondefaultvaluetest.Descendant
	if count := d.CountSetFieldsDescendant(); count != 0 {
		t.Errorf("Expected 0 set fields for nil union, got %d", count)
	}

	proto := thrift.NewTBinaryProtocolConf(thrift.NewTMemoryBuffer(), nil)
	err := d.Write(context.Background(), proto)
	if err == nil {
		t.Error("Expected error when writing nil union, got nil")
	}
}

func TestStructWithUnsetUnion(t *testing.T) {
	s := uniondefaultvaluetest.NewStructWithUnsetUnion()
	buf := thrift.NewTMemoryBuffer()
	proto := thrift.NewTBinaryProtocolConf(buf, nil)

	// Default requiredness means "write if set", so a nil union field is
	// left off the wire rather than reported as an error.
	if err := s.Write(context.Background(), proto); err != nil {
		t.Fatalf("Unexpected error writing a struct with an unset union: %v", err)
	}

	// Field 2 must not appear: the only field header on the wire is field 1.
	for _, b := range buf.Bytes() {
		if b == 0x0c {
			t.Errorf("Expected no struct field header on the wire, got % x", buf.Bytes())
			break
		}
	}

	readBack := uniondefaultvaluetest.NewStructWithUnsetUnion()
	if err := readBack.Read(context.Background(), thrift.NewTBinaryProtocolConf(buf, nil)); err != nil {
		t.Fatalf("Unexpected error reading the struct back: %v", err)
	}
	if readBack.F_2 != nil {
		t.Errorf("Expected readBack.F_2 to be nil, got %+v", readBack.F_2)
	}
}
