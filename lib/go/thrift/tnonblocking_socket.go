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
type TNonblockingSocket struct {
  conn net.Conn
  addr net.Addr
  /**
   * Socket timeout
   */
  nsecTimeout int64
}

type TNonblockingSocketTransportFactory struct {
  addr net.Addr
}

func (p *TNonblockingSocketTransportFactory) GetTransport(trans TTransport) TTransport {
  if trans != nil {
    t, ok := trans.(*TNonblockingSocket)
    if ok {
      s, _ := NewTNonblockingSocketAddr(t.addr)
      s.SetTimeout(t.nsecTimeout)
      return s
    }
  }
  s, _ := NewTNonblockingSocketAddr(p.addr)
  return s
}

func NewTNonblockingSocketTransportFactory(addr net.Addr) *TNonblockingSocketTransportFactory {
  return &TNonblockingSocketTransportFactory{addr: addr}
}

func NewTNonblockingSocketConn(conn net.Conn) (*TNonblockingSocket, TTransportException) {
  s := &TNonblockingSocket{conn: conn, addr: conn.RemoteAddr()}
  return s, nil
}

func NewTNonblockingSocketAddr(addr net.Addr) (*TNonblockingSocket, TTransportException) {
  s := &TNonblockingSocket{addr: addr}
  return s, nil
}

/**
 * Sets the socket timeout
 *
 * @param nsecTimeout Nanoseconds timeout
 */
func (p *TNonblockingSocket) SetTimeout(nsecTimeout int64) os.Error {
  p.nsecTimeout = nsecTimeout
  if p.IsOpen() {
    if err := p.conn.SetTimeout(nsecTimeout); err != nil {
      LOGGER.Print("Could not set socket timeout.", err)
      return err
    }
  }
  return nil
}

/**
 * Checks whether the socket is connected.
 */
func (p *TNonblockingSocket) IsOpen() bool {
  return p.conn != nil
}

/**
 * Connects the socket, creating a new socket object if necessary.
 */
func (p *TNonblockingSocket) Open() os.Error {
  if p.IsOpen() {
    return NewTTransportException(ALREADY_OPEN, "Socket already connected.")
  }
  if p.addr == nil {
    return NewTTransportException(NOT_OPEN, "Cannot open nil address.")
  }
  if len(p.addr.Network()) == 0 {
    return NewTTransportException(NOT_OPEN, "Cannot open bad network name.")
  }
  if len(p.addr.String()) == 0 {
    return NewTTransportException(NOT_OPEN, "Cannot open bad address.")
  }

  var err os.Error
  if p.conn, err = net.Dial(p.addr.Network(), p.addr.String()); err != nil {
    LOGGER.Print("Could not open socket", err.String())
    return NewTTransportException(NOT_OPEN, err.String())
  }
  if p.conn != nil {
    p.conn.SetTimeout(p.nsecTimeout)
  }
  return nil
}

/**
 * Perform a nonblocking read into buffer.
 */
func (p *TNonblockingSocket) Read(buf []byte) (int, os.Error) {
  if !p.IsOpen() {
    return 0, NewTTransportException(NOT_OPEN, "Connection not open")
  }
  n, err := p.conn.Read(buf)
  return n, NewTTransportExceptionFromOsError(err)
}


func (p *TNonblockingSocket) ReadAll(buf []byte) (int, os.Error) {
  return ReadAllTransport(p, buf)
}

/**
 * Perform a nonblocking write of the data in buffer;
 */
func (p *TNonblockingSocket) Write(buf []byte) (int, os.Error) {
  if !p.IsOpen() {
    return 0, NewTTransportException(NOT_OPEN, "Connection not open")
  }
  return p.conn.Write(buf)
}

/**
 * Flushes the underlying output stream if not null.
 */
func (p *TNonblockingSocket) Flush() os.Error {
  if !p.IsOpen() {
    return NewTTransportException(NOT_OPEN, "Connection not open")
  }
  f, ok := p.conn.(Flusher)
  if ok {
    err := f.Flush()
    if err != nil {
      return NewTTransportExceptionFromOsError(err)
    }
  }
  return nil
}

func (p *TNonblockingSocket) Addr() net.Addr {
  return p.addr
}

func (p *TNonblockingSocket) Peek() bool {
  return p.IsOpen()
}

/**
 * Closes the socket.
 */
func (p *TNonblockingSocket) Close() os.Error {
  if p.conn != nil {
    if err := p.conn.Close(); err != nil {
      LOGGER.Print("Could not close socket.", err.String())
      return err
    }
    p.conn = nil
  }
  return nil
}

func (p *TNonblockingSocket) Interrupt() os.Error {
  if !p.IsOpen() {
    return nil
  }
  // TODO(pomack) fix Interrupt as it is probably not right
  return p.Close()
}
