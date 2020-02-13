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
	"testing"
	"structenummarshallingtest"
)

func TestUnmarshaling(t *testing.T) {
	dummyStruct := &structenummarshallingtest.Dummy{}

	err := json.Unmarshal([]byte(`{"dummyEnum": "FIRST"}`), dummyStruct)
	if err != nil {
		t.Error(err)
	}

	if dummyStruct.DummyEnum != structenummarshallingtest.DummyEnum_FIRST {
		t.Errorf(
			"enums are not equals. actual: %d. expected: %d",
			dummyStruct.DummyEnum,
			structenummarshallingtest.DummyEnum_FIRST,
		)
	}
}

func TestMarshaling(t *testing.T) {
	dummyStruct := &structenummarshallingtest.Dummy{
		DummyEnum: structenummarshallingtest.DummyEnum_FIRST,
	}

	jsonBytes, err := json.Marshal(dummyStruct)
	if err != nil {
		t.Error(err)
	}

	expectedJson := `{"dummyEnum":"FIRST"}`
	actualJson := string(jsonBytes)

	if actualJson != expectedJson {
		t.Errorf(
			`json strings are not equal. expected: "%s". actual: "%s"`,
			expectedJson,
			actualJson,
		)
	}
}
