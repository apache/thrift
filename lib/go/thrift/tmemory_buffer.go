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
  "bytes"
  "io"
  "os"
)

/**
 * Memory buffer-based implementation of the TTransport interface.
 *
 */
type TMemoryBuffer struct {
  buf  *bytes.Buffer
  size int
}

type TMemoryBufferTransportFactory struct {
  size int
}

func (p *TMemoryBufferTransportFactory) GetTransport(trans TTransport) TTransport {
  if trans != nil {
    t, ok := trans.(*TMemoryBuffer)
    if ok && t.size > 0 {
      return NewTMemoryBufferLen(t.size)
    }
  }
  return NewTMemoryBufferLen(p.size)
}

func NewTMemoryBufferTransportFactory(size int) *TMemoryBufferTransportFactory {
  return &TMemoryBufferTransportFactory{size: size}
}

func NewTMemoryBuffer() *TMemoryBuffer {
  return &TMemoryBuffer{buf: &bytes.Buffer{}, size: 0}
}

func NewTMemoryBufferLen(size int) *TMemoryBuffer {
  buf := make([]byte, 0, size)
  return &TMemoryBuffer{buf: bytes.NewBuffer(buf), size: size}
}

func (p *TMemoryBuffer) IsOpen() bool {
  return true
}

func (p *TMemoryBuffer) Open() os.Error {
  return nil
}

func (p *TMemoryBuffer) Peek() bool {
  return p.IsOpen()
}

func (p *TMemoryBuffer) Close() os.Error {
  p.buf.Reset()
  return nil
}

func (p *TMemoryBuffer) Read(buf []byte) (int, os.Error) {
  return p.buf.Read(buf)
}

func (p *TMemoryBuffer) ReadAll(buf []byte) (int, os.Error) {
  return ReadAllTransport(p, buf)
}

func (p *TMemoryBuffer) ReadByte() (byte, os.Error) {
  return p.buf.ReadByte()
}

func (p *TMemoryBuffer) ReadFrom(r io.Reader) (int64, os.Error) {
  return p.buf.ReadFrom(r)
}

func (p *TMemoryBuffer) Write(buf []byte) (int, os.Error) {
  return p.buf.Write(buf)
}

func (p *TMemoryBuffer) WriteString(buf string) (int, os.Error) {
  return p.buf.WriteString(buf)
}

func (p *TMemoryBuffer) WriteTo(w io.Writer) (int64, os.Error) {
  return p.buf.WriteTo(w)
}

func (p *TMemoryBuffer) Flush() os.Error {
  return nil
}

func (p *TMemoryBuffer) Reset() {
  p.buf.Reset()
}

func (p *TMemoryBuffer) Bytes() []byte {
  return p.buf.Bytes()
}

func (p *TMemoryBuffer) Len() int {
  return p.buf.Len()
}

func (p *TMemoryBuffer) String() string {
  return p.buf.String()
}
