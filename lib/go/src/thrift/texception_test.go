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
  "io"
  "testing"
)

func TestTException(t *testing.T) {
  exc := NewTException("")
  if exc.Error() != "" {
    t.Fatalf("Expected empty string for exception but found '%s'", exc.Error())
  }
  exc = NewTExceptionFromOsError(io.EOF)
  if exc.Error() != io.EOF.Error() {
    t.Fatalf("Expected '%s', but found '%s'", io.EOF.Error(), exc.Error())
  }
}
