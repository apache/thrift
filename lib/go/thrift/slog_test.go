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
	"encoding/json"
	"strings"
	"testing"
)

func TestSlogTStructWrapperJSON(t *testing.T) {
	// This test just ensures that we don't have infinite recursion when
	// json encoding it. More comprehensive tests are under lib/go/test.
	v := SlogTStructWrapper{Type: "foo"}
	var sb strings.Builder
	if err := json.NewEncoder(&sb).Encode(v); err != nil {
		t.Fatal(err)
	}
	t.Log(strings.TrimSuffix(sb.String(), "\n"))
}
