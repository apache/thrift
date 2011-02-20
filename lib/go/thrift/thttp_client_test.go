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

package thrift_test

import (
  . "thrift"
  "testing"
  "http"
  "net"
)

func TestHttpClient(t *testing.T) {
  addr, err := FindAvailableTCPServerPort(40000)
  if err != nil {
    t.Fatalf("Unable to find available tcp port addr: %s", err)
  }
  l, err := net.Listen(addr.Network(), addr.String())
  if err != nil {
    t.Fatalf("Unable to setup tcp listener on %s: %s", addr.String(), err)
  }
  go http.Serve(l, &HTTPEchoServer{})
  trans, err := NewTHttpPostClient("http://" + addr.String())
  if err != nil {
    l.Close()
    t.Fatalf("Unable to connect to %s: %s", addr.String(), err)
  }
  TransportTest(t, trans, trans)
}
