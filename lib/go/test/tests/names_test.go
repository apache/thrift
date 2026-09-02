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
	"reflect"
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/namestest"
	"github.com/apache/thrift/lib/go/thrift"
)

func TestThatAttributeNameSubstituionDoesNotOccur(t *testing.T) {
	st := reflect.TypeFor[namestest.NamesTest]()
	_, ok := st.FieldByName("Type")
	if !ok {
		t.Error("Type attribute is missing!")
	}
}

func TestIsSetFieldDoesNotCollideWithAccessor(t *testing.T) {
	st := reflect.TypeFor[namestest.SetFlagNamesTest]()
	for _, name := range []string{"IsSetQueryParallelism_", "IsSetDefaultPoolPath_"} {
		if _, ok := st.FieldByName(name); !ok {
			t.Errorf("%s attribute is missing!", name)
		}
	}

	v := &namestest.SetFlagNamesTest{
		QueryParallelism:       thrift.Int32Ptr(4),
		IsSetQueryParallelism_: thrift.BoolPtr(false),
	}
	if !v.IsSetQueryParallelism() {
		t.Error("IsSetQueryParallelism() should report the queryParallelism field")
	}
	if !v.IsSetIsSetQueryParallelism_() {
		t.Error("IsSetIsSetQueryParallelism_() should report the isSetQueryParallelism field")
	}
	if v.GetIsSetQueryParallelism_() {
		t.Error("GetIsSetQueryParallelism_() should return the field value, not the accessor result")
	}
	if v.IsSetDefaultPoolPath() || v.IsSetIsSetDefaultPoolPath_() {
		t.Error("unset fields reported as set")
	}
}
