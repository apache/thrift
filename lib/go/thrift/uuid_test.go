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

import "testing"

func TestUuidString(t *testing.T) {
	uuid, err := NewUuidFromString("12345678-1234-1234-1234-123456789012")
	if err != nil {
		t.Fatal(err)
	}
	if uuid.String() != "12345678-1234-1234-1234-123456789012" {
		t.Fatal("String() returned wrong value")
	}
}

func TestUuidFromString(t *testing.T) {
	uuid, err := NewUuidFromString("12345678-1234-1234-1234-123456789012")
	if err != nil {
		t.Fatal(err)
	}
	if uuid != [16]byte{0x12, 0x34, 0x56, 0x78, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x12, 0x34, 0x56, 0x78, 0x90, 0x12} {
		t.Fatal("NewUuidFromString returned wrong value")
	}
}

func TestUuidFromStringShortCase(t *testing.T) {
	_, err := NewUuidFromString("12345678-1234-1234-1234-12345678901")
	if err == nil {
		t.Fatal("NewUuidFromString should have returned an error")
	}
}

func TestUuidFromStringLongCase(t *testing.T) {
	_, err := NewUuidFromString("12345678-1234-1234-1234-1234567890123")
	if err == nil {
		t.Fatal("NewUuidFromString should have returned an error")
	}
}
