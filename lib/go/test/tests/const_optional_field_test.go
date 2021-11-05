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
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/constoptionalfielda"
	"github.com/apache/thrift/lib/go/test/gopath/src/constoptionalfieldb"
)

func TestConstOptionalField(t *testing.T) {
	c := constoptionalfieldb.CONSTANTS[0]

	t.Run("foo", func(t *testing.T) {
		const expected = constoptionalfielda.Foo_One
		if *c.OptFoo != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptFoo)
		}
		if *c.AFoo != constoptionalfielda.TypedefAFoo(expected) {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.AFoo)
		}
		if *c.BFoo != constoptionalfieldb.TypedefBFoo(expected) {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BFoo)
		}
	})

	t.Run("bool", func(t *testing.T) {
		const expected = true
		if *c.OptBool != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptBool)
		}
		if *c.ABool != constoptionalfielda.TypedefABool(expected) {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.ABool)
		}
		if *c.BBool != constoptionalfieldb.TypedefBBool(expected) {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BBool)
		}
	})

	t.Run("i8", func(t *testing.T) {
		const expected = 8
		if *c.OptI8 != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptI8)
		}
		if *c.AI8 != constoptionalfielda.TypedefAI8(expected) {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.AI8)
		}
		if *c.BI8 != constoptionalfieldb.TypedefBI8(expected) {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BI8)
		}
	})

	t.Run("i16", func(t *testing.T) {
		const expected = 16
		if *c.OptI16 != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptI16)
		}
		if *c.AI16 != constoptionalfielda.TypedefAI16(expected) {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.AI16)
		}
		if *c.BI16 != constoptionalfieldb.TypedefBI16(expected) {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BI16)
		}
	})

	t.Run("i32", func(t *testing.T) {
		const expected = 32
		if *c.OptI32 != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptI32)
		}
		if *c.AI32 != constoptionalfielda.TypedefAI32(expected) {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.AI32)
		}
		if *c.BI32 != constoptionalfieldb.TypedefBI32(expected) {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BI32)
		}
	})

	t.Run("i64", func(t *testing.T) {
		const expected = 64
		if *c.OptI64 != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptI64)
		}
		if *c.AI64 != constoptionalfielda.TypedefAI64(expected) {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.AI64)
		}
		if *c.BI64 != constoptionalfieldb.TypedefBI64(expected) {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BI64)
		}
	})

	t.Run("double", func(t *testing.T) {
		// To avoid the annoyance of comparing float numbers,
		// we convert all floats to int in this test.
		const expected = 1234
		if int(*c.OptDouble) != expected {
			t.Errorf("Expected %v, got %v", expected, *c.OptDouble)
		}
		if int(*c.ADouble) != expected {
			t.Errorf("Typedef a expected %v, got %v", expected, *c.ADouble)
		}
		if int(*c.BDouble) != expected {
			t.Errorf("Typedef b expected %v, got %v", expected, *c.BDouble)
		}
	})

	t.Run("string", func(t *testing.T) {
		const expected = "string"
		if *c.OptString != expected {
			t.Errorf("Expected %q, got %q", expected, *c.OptString)
		}
		if *c.AString != constoptionalfielda.TypedefAString(expected) {
			t.Errorf("Typedef a expected %q, got %q", expected, *c.AString)
		}
		if *c.BString != constoptionalfieldb.TypedefBString(expected) {
			t.Errorf("Typedef b expected %q, got %q", expected, *c.BString)
		}
	})

	t.Run("binary", func(t *testing.T) {
		const expected = "binary"
		if string(c.OptBinary) != expected {
			t.Errorf("Expected %q, got %q", expected, c.OptBinary)
		}
		if string(c.ABinary) != expected {
			t.Errorf("Typedef a expected %q, got %q", expected, c.ABinary)
		}
		if string(c.BBinary) != expected {
			t.Errorf("Typedef b expected %q, got %q", expected, c.BBinary)
		}
	})
}
