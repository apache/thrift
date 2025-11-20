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
	"encoding/json"
	"errors"
	"strconv"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/validatetest"
	thrift "github.com/apache/thrift/lib/go/thrift"
)

func TestBasicValidator(t *testing.T) {
	bt := validatetest.NewBasicTest()
	if err := bt.Validate(); err != nil {
		t.Error(err)
	}
	var ve *thrift.ValidationError
	bt = validatetest.NewBasicTest()
	bt.Bool1 = thrift.BoolPtr(false)
	if err := bt.Validate(); err == nil {
		t.Error("Expected vt.const error for Bool1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "Bool1" {
			t.Errorf("Expected error for Bool1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	bt.Byte1 = thrift.Int8Ptr(3)
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.lt error for Byte1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.lt" {
			t.Errorf("Expected vt.lt check error, but got %v", ve.Check())
		}
		if ve.Field() != "Byte1" {
			t.Errorf("Expected error for Byte1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	bt.Double1 = thrift.Float64Ptr(3.0)
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.lt error for Double1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.lt" {
			t.Errorf("Expected vt.lt check error, but got %v", ve.Check())
		}
		if ve.Field() != "Double1" {
			t.Errorf("Expected error for Double1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	bt.String1 = thrift.StringPtr("other string")
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.const error for String1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "String1" {
			t.Errorf("Expected error for String1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	bt.Binary1 = []byte("other binary")
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.const error for Binary1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "Binary1" {
			t.Errorf("Expected error for Binary1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	bt.Map1 = make(map[string]string)
	for i := range 11 {
		bt.Map1[strconv.Itoa(i)] = strconv.Itoa(i)
	}
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Map1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Map1" {
			t.Errorf("Expected error for Map1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt.Map1 = map[string]string{"012345678910": "0"}
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Map1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Map1" {
			t.Errorf("Expected error for Map1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt.Map1 = map[string]string{"0": "012345678910"}
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Map1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Map1" {
			t.Errorf("Expected error for Map1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	for range 11 {
		bt.Set1 = append(bt.Set1, "0")
	}
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Set1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Set1" {
			t.Errorf("Expected error for Set1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt.Set1 = []string{"0"}
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.min_size error for Set1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.min_size" {
			t.Errorf("Expected vt.min_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Set1" {
			t.Errorf("Expected error for Set1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	bt = validatetest.NewBasicTest()
	bt.Enum1 = (*validatetest.EnumFoo)(thrift.Int64Ptr(int64(validatetest.EnumFoo_e2)))
	if err := bt.Validate(); err == nil {
		t.Errorf("Expected vt.in error for Enum1")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.in" {
			t.Errorf("Expected vt.in check error, but got %v", ve.Check())
		}
		if ve.Field() != "Enum1" {
			t.Errorf("Expected error for Enum1, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
}

func TestFieldReference(t *testing.T) {
	frt := validatetest.NewFieldReferenceTest()
	if err := frt.Validate(); err != nil {
		t.Error(err)
	}
	var ve *thrift.ValidationError
	frt = validatetest.NewFieldReferenceTest()
	frt.Bool2 = true
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.const error for Bool0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "Bool0" {
			t.Errorf("Expected error for Bool0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.Byte4 = 9
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.lt error for Byte0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.lt" {
			t.Errorf("Expected vt.lt check error, but got %v", ve.Check())
		}
		if ve.Field() != "Byte0" {
			t.Errorf("Expected error for Byte0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.Double4 = 9
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.lt error for Double0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.lt" {
			t.Errorf("Expected vt.lt check error, but got %v", ve.Check())
		}
		if ve.Field() != "Double0" {
			t.Errorf("Expected error for Double0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.String2 = "other string"
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.const error for String0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "String0" {
			t.Errorf("Expected error for String0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.Binary2 = []byte("other string")
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.const error for Binary0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "Binary0" {
			t.Errorf("Expected error for Binary0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.MaxSize = 8
	frt.Map0 = make(map[string]string)
	for i := range 9 {
		frt.Map0[strconv.Itoa(i)] = strconv.Itoa(i)
	}
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Map0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Map0" {
			t.Errorf("Expected error for Map0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.MaxSize = 8
	for range 9 {
		frt.List0 = append(frt.List0, "0")
	}
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for List0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "List0" {
			t.Errorf("Expected error for List0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	frt = validatetest.NewFieldReferenceTest()
	frt.MaxSize = 8
	for range 9 {
		frt.Set0 = append(frt.Set0, "0")
	}
	if err := frt.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Set0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Set0" {
			t.Errorf("Expected error for Set0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
}

func TestValidationFunction(t *testing.T) {
	vft := validatetest.NewValidationFunctionTest()
	if err := vft.Validate(); err != nil {
		t.Error(err)
	}
	var ve *thrift.ValidationError
	vft = validatetest.NewValidationFunctionTest()
	vft.StringFoo = "some string"
	if err := vft.Validate(); err == nil {
		t.Errorf("Expected vt.in error for StringLength")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.in" {
			t.Errorf("Expected vt.in check error, but got %v", ve.Check())
		}
		if ve.Field() != "StringLength" {
			t.Errorf("Expected error for StringLength, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
}

func TestAnnotationCompatibleTest(t *testing.T) {
	act := validatetest.NewAnnotationCompatibleTest()
	if err := act.Validate(); err != nil {
		t.Error(err)
	}
	var ve *thrift.ValidationError
	act = validatetest.NewAnnotationCompatibleTest()
	act.Bool0 = false
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.const error for Bool0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "Bool0" {
			t.Errorf("Expected error for Bool0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.Byte0 = 3
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.lt error for Byte0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.lt" {
			t.Errorf("Expected vt.lt check error, but got %v", ve.Check())
		}
		if ve.Field() != "Byte0" {
			t.Errorf("Expected error for Byte0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.Double0 = 3
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.lt error for Double0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.lt" {
			t.Errorf("Expected vt.lt check error, but got %v", ve.Check())
		}
		if ve.Field() != "Double0" {
			t.Errorf("Expected error for Double0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.String0 = "other string"
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.const error for String0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "String0" {
			t.Errorf("Expected error for String0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.Binary0 = []byte("other string")
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.const error for Binary0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.const" {
			t.Errorf("Expected vt.const check error, but got %v", ve.Check())
		}
		if ve.Field() != "Binary0" {
			t.Errorf("Expected error for Binary0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.Map0 = map[string]string{"0": "0", "1": "1", "2": "2"}
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Map0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Map0" {
			t.Errorf("Expected error for Map0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.Set0 = []string{"0", "1", "2"}
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for Set0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "Set0" {
			t.Errorf("Expected error for Set0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.List0 = []string{"0", "1", "2"}
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.max_size error for List0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.max_size" {
			t.Errorf("Expected vt.max_size check error, but got %v", ve.Check())
		}
		if ve.Field() != "List0" {
			t.Errorf("Expected error for List0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	act = validatetest.NewAnnotationCompatibleTest()
	act.Enum0 = validatetest.EnumFoo_e1
	if err := act.Validate(); err == nil {
		t.Errorf("Expected vt.in error for Enum0")
	} else if errors.As(err, &ve) {
		if ve.Check() != "vt.in" {
			t.Errorf("Expected vt.in check error, but got %v", ve.Check())
		}
		if ve.Field() != "Enum0" {
			t.Errorf("Expected error for Enum0, but got %v", ve.Field())
		}
	} else {
		t.Errorf("Error cannot be unwrapped into *ValidationError: %v", err)
	}
	fields := []string{"bool1", "byte1", "double1", "string1", "binary1", "enum1", "struct1", "list1", "set1", "map1"}
	b, err := json.Marshal(act)
	if err != nil {
		t.Error(err)
	}
	jsonMap := make(map[string]interface{})
	if err = json.Unmarshal(b, &jsonMap); err != nil {
		t.Error(err)
	}
	for _, field := range fields {
		if _, ok := jsonMap[field]; !ok {
			t.Errorf("Expected field %s in JSON, but not found", field)
		}
	}
}
