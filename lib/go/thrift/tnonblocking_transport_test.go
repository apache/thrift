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
  "net"
)

func TestNonblockingTransportServerToClient(t *testing.T) {

  addr, err := FindAvailableTCPServerPort(40000)
  if err != nil {
    t.Fatalf("Unable to find available tcp port addr: %s", err)
  }
  trans1, err := NewTNonblockingServerSocketAddr(addr)
  if err != nil {
    t.Fatalf("Unable to setup server socket listener: %s", err)
  }
  trans1.Open()
  trans2, err := NewTNonblockingSocketAddr(addr)
  if err != nil {
    t.Fatalf("Unable to setup client socket: %s", err)
  }
  trans1.SetTimeout(10)
  trans2.SetTimeout(10)
  err = trans2.Open()
  if err != nil {
    t.Fatalf("Unable to connect client to server: %s", err)
  }
  s, err := trans1.Accept()
  if err != nil {
    t.Fatalf("Unable to accept client connection from server: %s", err)
  }
  //s.SetTimeout(10)
  TransportTest(t, NewTFramedTransport(s), NewTFramedTransport(trans2))
  trans1.Close()
}

func TestNonblockingTransportClientToServer(t *testing.T) {
  addr, err := FindAvailableTCPServerPort(40000)
  if err != nil {
    t.Fatalf("Unable to find available tcp port addr: %s", err)
  }
  l, err := net.Listen(addr.Network(), addr.String())
  if err != nil {
    t.Fatalf("Unable to setup listener: %s", err)
  }
  trans1, err := NewTNonblockingServerSocketListener(l)
  if err != nil {
    t.Fatalf("Unable to setup server socket listener: %s", err)
  }
  trans2, err := NewTNonblockingSocketAddr(l.Addr())
  if err != nil {
    t.Fatalf("Unable to setup client socket: %s", err)
  }
  trans1.SetTimeout(10)
  trans2.SetTimeout(10)
  err = trans2.Open()
  if err != nil {
    t.Fatalf("Unable to connect client to server: %s", err)
  }
  s, err := trans1.Accept()
  if err != nil {
    t.Fatalf("Unable to accept client connection from server: %s", err)
  }
  //s.SetTimeout(10)
  TransportTest(t, NewTFramedTransport(trans2), NewTFramedTransport(s))
  trans1.Close()
}
