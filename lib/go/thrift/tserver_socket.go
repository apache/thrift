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


type TServerSocket struct {
  /**
   * Underlying socket conection object
   */
  conn net.Conn
  /**
   * Underlying socket conection object
   */
  listener net.Listener

  /**
   * Address to listen on
   */
  addr net.Addr

  /**
   * Client timeout in nanoseconds
   */
  nsecClientTimeout int64
}

type TServerSocketTransportFactory struct {
  addr              net.Addr
  nsecClientTimeout int64
}

func (p *TServerSocketTransportFactory) GetTransport(trans TTransport) TTransport {
  if trans != nil {
    t, ok := trans.(*TServerSocket)
    if ok && t.addr != nil {
      s, _ := NewTServerSocketAddrTimeout(t.addr, t.nsecClientTimeout)
      return s
    }
  }
  s, _ := NewTServerSocketAddrTimeout(p.addr, p.nsecClientTimeout)
  return s
}

func NewTServerSocketTransportFactory(addr net.Addr, nsecClientTimeout int64) *TServerSocketTransportFactory {
  return &TServerSocketTransportFactory{addr: addr, nsecClientTimeout: nsecClientTimeout}
}

func NewTServerSocketConn(conn net.Conn) *TServerSocket {
  return NewTServerSocketConnTimeout(conn, 0)
}

func NewTServerSocketConnTimeout(conn net.Conn, nsecClientTimeout int64) *TServerSocket {
  v := &TServerSocket{conn: conn, addr: conn.LocalAddr(), nsecClientTimeout: nsecClientTimeout}
  conn.SetTimeout(nsecClientTimeout)
  return v
}

func NewTServerSocketAddr(addr net.Addr) (*TServerSocket, TTransportException) {
  return NewTServerSocketAddrTimeout(addr, 0)
}

func NewTServerSocketAddrTimeout(addr net.Addr, nsecClientTimeout int64) (*TServerSocket, TTransportException) {
  s := &TServerSocket{addr: addr, nsecClientTimeout: nsecClientTimeout}
  return s, nil
}

func (p *TServerSocket) Listen() (err os.Error) {
  if p.listener == nil {
    if p.listener, err = net.Listen("tcp", p.addr.String()); err != nil {
      return err
    }
  }
  return nil
}

func (p *TServerSocket) Accept() (TTransport, os.Error) {
  if p.listener == nil {
    if err := p.Listen(); err != nil {
      return nil, NewTTransportExceptionFromOsError(err)
    }
    if p.listener == nil {
      return nil, NewTTransportException(NOT_OPEN, "No underlying server socket")
    }
  }
  conn, err := p.listener.Accept()
  if err != nil {
    return nil, NewTTransportExceptionFromOsError(err)
  }
  conn.SetTimeout(p.nsecClientTimeout)
  return NewTSocketConn(conn)
}

/**
 * Checks whether the socket is connected.
 */
func (p *TServerSocket) IsOpen() bool {
  return p.listener != nil
}

/**
 * Connects the socket, creating a new socket object if necessary.
 */
func (p *TServerSocket) Open() os.Error {
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
func (p *TServerSocket) Read(buf []byte) (int, os.Error) {
  return 0, NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "TServerSocket.Read([]byte) is not implemented")
}

func (p *TServerSocket) ReadAll(buf []byte) (int, os.Error) {
  return ReadAllTransport(p, buf)
}

/**
 * Perform a nonblocking write of the data in buffer;
 */
func (p *TServerSocket) Write(buf []byte) (int, os.Error) {
  return 0, NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "TServerSocket.Write([]byte) is not implemented")
}

/**
 * Flushes the underlying output stream if not null.
 */
func (p *TServerSocket) Flush() os.Error {
  return NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, "TServerSocket.Flush() is not implemented")
}

func (p *TServerSocket) Addr() net.Addr {
  return p.addr
}

func (p *TServerSocket) Peek() bool {
  return p.IsOpen()
}

/**
 * Closes the socket.
 */
func (p *TServerSocket) Close() (err os.Error) {
  if p.IsOpen() {
    err := p.listener.Close()
    if err != nil {
      return NewTTransportExceptionFromOsError(err)
    }
    p.listener = nil
  }
  if p.conn != nil {
    err := p.conn.Close()
    if err != nil {
      return NewTTransportExceptionFromOsError(err)
    }
    p.conn = nil
  }
  return nil
}

func (p *TServerSocket) Interrupt() os.Error {
  // TODO(pomack) fix Interrupt as it is probably not right
  return NewTTransportExceptionFromOsError(p.Close())
}
