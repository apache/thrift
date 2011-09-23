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
  "bytes"
)

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 *
 */
type TSocket struct {
  writeBuffer *bytes.Buffer
  /**
   * Wrapped Socket object
   */
  conn net.Conn
  /**
   * Remote Addr
   */
  addr net.Addr
  /**
   * Socket timeout in nanoseconds
   */
  nsecTimeout int64
}

/**
 * Constructor that takes an already created socket.
 *
 * @param socket Already created socket object
 * @throws TTransportException if there is an error setting up the streams
 */
func NewTSocketConn(connection net.Conn) (*TSocket, TTransportException) {
  address := connection.RemoteAddr()
  if address == nil {
    address = connection.LocalAddr()
  }
  p := &TSocket{conn: connection, addr: address, nsecTimeout: 0, writeBuffer: bytes.NewBuffer(make([]byte, 0, 4096))}
  return p, nil
}

/**
 * Creates a new unconnected socket that will connect to the given host
 * on the given port.
 *
 * @param host Remote host
 * @param port Remote port
 */
func NewTSocketAddr(address net.Addr) *TSocket {
  return NewTSocket(address, 0)
}

/**
 * Creates a new unconnected socket that will connect to the given host
 * on the given port.
 *
 * @param host    Remote host
 * @param port    Remote port
 * @param nsecTimeout Socket timeout
 */
func NewTSocket(address net.Addr, nsecTimeout int64) *TSocket {
  sock := &TSocket{addr: address, nsecTimeout: nsecTimeout, writeBuffer: bytes.NewBuffer(make([]byte, 0, 4096))}
  return sock
}

/**
 * Sets the socket timeout
 *
 * @param timeout Nanoseconds timeout
 */
func (p *TSocket) SetTimeout(nsecTimeout int64) os.Error {
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
 * Returns a reference to the underlying socket.
 */
func (p *TSocket) Conn() net.Conn {
  return p.conn
}

/**
 * Checks whether the socket is connected.
 */
func (p *TSocket) IsOpen() bool {
  if p.conn == nil {
    return false
  }
  return true
}

/**
 * Connects the socket, creating a new socket object if necessary.
 */
func (p *TSocket) Open() os.Error {
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
 * Closes the socket.
 */
func (p *TSocket) Close() os.Error {
  // Close the socket
  if p.conn != nil {
    err := p.conn.Close()
    if err != nil {
      LOGGER.Print("Could not close socket. ", err.String())
      return err
    }
    p.conn = nil
  }
  return nil
}


func (p *TSocket) Read(buf []byte) (int, os.Error) {
  if !p.IsOpen() {
    return 0, NewTTransportException(NOT_OPEN, "Connection not open")
  }
  n, err := p.conn.Read(buf)
  return n, NewTTransportExceptionFromOsError(err)
}


func (p *TSocket) ReadAll(buf []byte) (int, os.Error) {
  return ReadAllTransport(p, buf)
}

func (p *TSocket) Write(buf []byte) (int, os.Error) {
  if !p.IsOpen() {
    return 0, NewTTransportException(NOT_OPEN, "Connection not open")
  }
  p.writeBuffer.Write(buf)
  return len(buf), nil
}

func (p *TSocket) Peek() bool {
  return p.IsOpen()
}

func (p *TSocket) Flush() os.Error {
  if !p.IsOpen() {
    return NewTTransportException(NOT_OPEN, "Connection not open")
  }
  _, err := p.writeBuffer.WriteTo(p.conn)
  return NewTTransportExceptionFromOsError(err)
}

func (p *TSocket) Interrupt() os.Error {
  if !p.IsOpen() {
    return nil
  }
  // TODO(pomack) fix Interrupt as this is probably wrong
  return p.conn.Close()
}
