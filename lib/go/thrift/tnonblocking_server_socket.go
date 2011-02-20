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
  "net"
  "os"
)

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 */
type TNonblockingServerSocket struct {
  listener net.Listener
  addr     net.Addr
  /**
   * Socket timeout
   */
  nsecTimeout int64
}

type TNonblockingServerSocketTransportFactory struct {
  addr net.Addr
}

func (p *TNonblockingServerSocketTransportFactory) GetTransport(trans TTransport) TTransport {
  if trans != nil {
    t, ok := trans.(*TNonblockingServerSocket)
    if ok && t.addr != nil {
      s, _ := NewTNonblockingServerSocketAddr(t.addr)
      s.SetTimeout(t.nsecTimeout)
      return s
    }
  }
  s, _ := NewTNonblockingServerSocketAddr(p.addr)
  return s
}

func NewTNonblockingServerSocketTransportFactory(addr net.Addr) *TNonblockingServerSocketTransportFactory {
  return &TNonblockingServerSocketTransportFactory{addr: addr}
}


func NewTNonblockingServerSocketListener(listener net.Listener) (*TNonblockingServerSocket, TTransportException) {
  s := &TNonblockingServerSocket{listener: listener, addr: listener.Addr()}
  return s, nil
}

func NewTNonblockingServerSocketAddr(addr net.Addr) (*TNonblockingServerSocket, TTransportException) {
  s := &TNonblockingServerSocket{addr: addr}
  return s, nil
}

func (p *TNonblockingServerSocket) Listen() os.Error {
  return p.Open()
}

/**
 * Sets the socket timeout
 *
 * @param timeout Nanoseconds timeout
 */
func (p *TNonblockingServerSocket) SetTimeout(nsecTimeout int64) os.Error {
  p.nsecTimeout = nsecTimeout
  return nil
}

/**
 * Checks whether the socket is connected.
 */
func (p *TNonblockingServerSocket) IsOpen() bool {
  return p.listener != nil
}

/**
 * Connects the socket, creating a new socket object if necessary.
 */
func (p *TNonblockingServerSocket) Open() os.Error {
  if !p.IsOpen() {
    l, err := net.Listen(p.addr.Network(), p.addr.String())
    if err != nil {
      return err
    }
    p.listener = l
    return nil
  }
  return NewTTransportException(ALREADY_OPEN, "Server socket already open")
}

/**
 * Perform a nonblocking read into buffer.
 */
func (p *TNonblockingServerSocket) Read(buf []byte) (int, os.Error) {
  return 0, NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "TNonblockingServerSocket.Read([]byte) is not implemented")
}

func (p *TNonblockingServerSocket) ReadAll(buf []byte) (int, os.Error) {
  return ReadAllTransport(p, buf)
}

/**
 * Perform a nonblocking write of the data in buffer;
 */
func (p *TNonblockingServerSocket) Write(buf []byte) (int, os.Error) {
  return 0, NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "TNonblockingServerSocket.Write([]byte) is not implemented")
}

/**
 * Flushes the underlying output stream if not null.
 */
func (p *TNonblockingServerSocket) Flush() os.Error {
  return NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "TNonblockingServerSocket.Flush() is not implemented")
}

func (p *TNonblockingServerSocket) Addr() net.Addr {
  return p.addr
}

func (p *TNonblockingServerSocket) Accept() (TTransport, os.Error) {
  if !p.IsOpen() {
    return nil, NewTTransportException(NOT_OPEN, "No underlying server socket")
  }
  conn, err := p.listener.Accept()
  if err != nil {
    return nil, NewTTransportExceptionFromOsError(err)
  }
  conn.SetTimeout(p.nsecTimeout)
  return NewTSocketConn(conn)
}

func (p *TNonblockingServerSocket) Peek() bool {
  return p.IsOpen()
}

/**
 * Closes the socket.
 */
func (p *TNonblockingServerSocket) Close() (err os.Error) {
  if p.IsOpen() {
    err := p.listener.Close()
    if err != nil {
      return NewTTransportExceptionFromOsError(err)
    }
    p.listener = nil
  }
  return nil
}

func (p *TNonblockingServerSocket) Interrupt() os.Error {
  // probably not right
  return p.Close()
}
