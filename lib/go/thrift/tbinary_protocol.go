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
  "encoding/binary"
  "math"
  "strings"
  "io"
)

type TBinaryProtocol struct {
  //TProtocolBase;
  trans            TTransport
  _StrictRead      bool
  _StrictWrite     bool
  _ReadLength      int
  _CheckReadLength bool
}

type TBinaryProtocolFactory struct {
  _StrictRead  bool
  _StrictWrite bool
}

func NewTBinaryProtocolTransport(t TTransport) *TBinaryProtocol {
  return NewTBinaryProtocol(t, false, true)
}

func NewTBinaryProtocol(t TTransport, strictRead, strictWrite bool) *TBinaryProtocol {
  //return &TBinaryProtocol{TProtocolBase:TProtocolBase{trans:t}, _StrictRead:strictRead, _StrictWrite:strictWrite, _ReadLength:0, _CheckReadLength:false};
  return &TBinaryProtocol{trans: t, _StrictRead: strictRead, _StrictWrite: strictWrite, _ReadLength: 0, _CheckReadLength: false}
}

func NewTBinaryProtocolFactoryDefault() *TBinaryProtocolFactory {
  return NewTBinaryProtocolFactory(false, true)
}

func NewTBinaryProtocolFactory(strictRead, strictWrite bool) *TBinaryProtocolFactory {
  return &TBinaryProtocolFactory{_StrictRead: strictRead, _StrictWrite: strictWrite}
}

func (p *TBinaryProtocolFactory) GetProtocol(t TTransport) TProtocol {
  return NewTBinaryProtocol(t, p._StrictRead, p._StrictWrite)
}

/**
 * Writing Methods
 */

func (p *TBinaryProtocol) WriteMessageBegin(name string, typeId TMessageType, seqId int32) TProtocolException {
  if p._StrictWrite {
    version := uint32(VERSION_1) | uint32(typeId)
    e := p.WriteI32(int32(version))
    if e != nil {
      return e
    }
    e = p.WriteString(name)
    if e != nil {
      return e
    }
    e = p.WriteI32(seqId)
    return e
  } else {
    e := p.WriteString(name)
    if e != nil {
      return e
    }
    e = p.WriteByte(byte(typeId))
    if e != nil {
      return e
    }
    e = p.WriteI32(seqId)
    return e
  }
  return nil
}

func (p *TBinaryProtocol) WriteMessageEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteStructBegin(name string) TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteStructEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteFieldBegin(name string, typeId TType, id int16) TProtocolException {
  e := p.WriteByte(byte(typeId))
  if e != nil {
    return e
  }
  e = p.WriteI16(id)
  return e
}

func (p *TBinaryProtocol) WriteFieldEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteFieldStop() TProtocolException {
  e := p.WriteByte(STOP)
  return e
}

func (p *TBinaryProtocol) WriteMapBegin(keyType TType, valueType TType, size int) TProtocolException {
  e := p.WriteByte(byte(keyType))
  if e != nil {
    return e
  }
  e = p.WriteByte(byte(valueType))
  if e != nil {
    return e
  }
  e = p.WriteI32(int32(size))
  return e
}

func (p *TBinaryProtocol) WriteMapEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteListBegin(elemType TType, size int) TProtocolException {
  e := p.WriteByte(byte(elemType))
  if e != nil {
    return e
  }
  e = p.WriteI32(int32(size))
  return e
}

func (p *TBinaryProtocol) WriteListEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteSetBegin(elemType TType, size int) TProtocolException {
  e := p.WriteByte(byte(elemType))
  if e != nil {
    return e
  }
  e = p.WriteI32(int32(size))
  return e
}

func (p *TBinaryProtocol) WriteSetEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) WriteBool(value bool) TProtocolException {
  if value {
    return p.WriteByte(1)
  }
  return p.WriteByte(0)
}

func (p *TBinaryProtocol) WriteByte(value byte) TProtocolException {
  v := []byte{value}
  _, e := p.trans.Write(v)
  return NewTProtocolExceptionFromOsError(e)
}

func (p *TBinaryProtocol) WriteI16(value int16) TProtocolException {
  h := byte(0xff & (value >> 8))
  l := byte(0xff & value)
  v := []byte{h, l}
  _, e := p.trans.Write(v)
  return NewTProtocolExceptionFromOsError(e)
}

func (p *TBinaryProtocol) WriteI32(value int32) TProtocolException {
  a := byte(0xff & (value >> 24))
  b := byte(0xff & (value >> 16))
  c := byte(0xff & (value >> 8))
  d := byte(0xff & value)
  v := []byte{a, b, c, d}
  _, e := p.trans.Write(v)
  return NewTProtocolExceptionFromOsError(e)
}

func (p *TBinaryProtocol) WriteI64(value int64) TProtocolException {
  a := byte(0xff & (value >> 56))
  b := byte(0xff & (value >> 48))
  c := byte(0xff & (value >> 40))
  d := byte(0xff & (value >> 32))
  e := byte(0xff & (value >> 24))
  f := byte(0xff & (value >> 16))
  g := byte(0xff & (value >> 8))
  h := byte(0xff & value)
  v := []byte{a, b, c, d, e, f, g, h}
  _, err := p.trans.Write(v)
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TBinaryProtocol) WriteDouble(value float64) TProtocolException {
  return p.WriteI64(int64(math.Float64bits(value)))
}

func (p *TBinaryProtocol) WriteString(value string) TProtocolException {
  return p.WriteBinaryFromReader(strings.NewReader(value), len(value))
}

func (p *TBinaryProtocol) WriteBinary(value []byte) TProtocolException {
  e := p.WriteI32(int32(len(value)))
  if e != nil {
    return e
  }
  _, err := p.trans.Write(value)
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TBinaryProtocol) WriteBinaryFromReader(reader io.Reader, size int) TProtocolException {
  e := p.WriteI32(int32(size))
  if e != nil {
    return e
  }
  _, err := io.Copyn(p.trans, reader, int64(size))
  return NewTProtocolExceptionFromOsError(err)
}


/**
 * Reading methods
 */

func (p *TBinaryProtocol) ReadMessageBegin() (name string, typeId TMessageType, seqId int32, err TProtocolException) {
  size, e := p.ReadI32()
  if e != nil {
    return "", typeId, 0, NewTProtocolExceptionFromOsError(e)
  }
  if size < 0 {
    typeId = TMessageType(size & 0x0ff)
    version := int64(int64(size) & VERSION_MASK)
    if version != VERSION_1 {
      return name, typeId, seqId, NewTProtocolException(BAD_VERSION, "Bad version in ReadMessageBegin")
    }
    name, e = p.ReadString()
    if e != nil {
      return name, typeId, seqId, NewTProtocolExceptionFromOsError(e)
    }
    seqId, e = p.ReadI32()
    if e != nil {
      return name, typeId, seqId, NewTProtocolExceptionFromOsError(e)
    }
    return name, typeId, seqId, nil
  }
  if p._StrictRead {
    return name, typeId, seqId, NewTProtocolException(BAD_VERSION, "Missing version in ReadMessageBegin")
  }
  name, e2 := p.readStringBody(int(size))
  if e2 != nil {
    return name, typeId, seqId, e2
  }
  b, e3 := p.ReadByte()
  if e3 != nil {
    return name, typeId, seqId, e3
  }
  typeId = TMessageType(b)
  seqId, e4 := p.ReadI32()
  if e4 != nil {
    return name, typeId, seqId, e4
  }
  return name, typeId, seqId, nil
}

func (p *TBinaryProtocol) ReadMessageEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) ReadStructBegin() (name string, err TProtocolException) {
  return
}

func (p *TBinaryProtocol) ReadStructEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) ReadFieldBegin() (name string, typeId TType, seqId int16, err TProtocolException) {
  t, err := p.ReadByte()
  typeId = TType(t)
  if err != nil {
    return name, typeId, seqId, err
  }
  if t != STOP {
    seqId, err = p.ReadI16()
  }
  return name, typeId, seqId, err
}

func (p *TBinaryProtocol) ReadFieldEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) ReadMapBegin() (kType, vType TType, size int, err TProtocolException) {
  k, e := p.ReadByte()
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  kType = TType(k)
  v, e := p.ReadByte()
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  vType = TType(v)
  size32, e := p.ReadI32()
  size = int(size32)
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  return kType, vType, size, nil
}

func (p *TBinaryProtocol) ReadMapEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) ReadListBegin() (elemType TType, size int, err TProtocolException) {
  b, e := p.ReadByte()
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  elemType = TType(b)
  size32, e := p.ReadI32()
  size = int(size32)
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  return elemType, size, nil
}

func (p *TBinaryProtocol) ReadListEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) ReadSetBegin() (elemType TType, size int, err TProtocolException) {
  b, e := p.ReadByte()
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  elemType = TType(b)
  size32, e := p.ReadI32()
  size = int(size32)
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  return elemType, size, nil
}

func (p *TBinaryProtocol) ReadSetEnd() TProtocolException {
  return nil
}

func (p *TBinaryProtocol) ReadBool() (bool, TProtocolException) {
  b, e := p.ReadByte()
  v := true
  if b != 1 {
    v = false
  }
  return v, e
}

func (p *TBinaryProtocol) ReadByte() (value byte, err TProtocolException) {
  buf := []byte{0}
  err = p.readAll(buf)
  return buf[0], err
}

func (p *TBinaryProtocol) ReadI16() (value int16, err TProtocolException) {
  buf := []byte{0, 0}
  err = p.readAll(buf)
  value = int16(binary.BigEndian.Uint16(buf))
  return value, err
}

func (p *TBinaryProtocol) ReadI32() (value int32, err TProtocolException) {
  buf := []byte{0, 0, 0, 0}
  err = p.readAll(buf)
  value = int32(binary.BigEndian.Uint32(buf))
  return value, err
}

func (p *TBinaryProtocol) ReadI64() (value int64, err TProtocolException) {
  buf := []byte{0, 0, 0, 0, 0, 0, 0, 0}
  err = p.readAll(buf)
  value = int64(binary.BigEndian.Uint64(buf))
  return value, err
}

func (p *TBinaryProtocol) ReadDouble() (value float64, err TProtocolException) {
  buf := []byte{0, 0, 0, 0, 0, 0, 0, 0}
  err = p.readAll(buf)
  value = math.Float64frombits(binary.BigEndian.Uint64(buf))
  return value, err
}

func (p *TBinaryProtocol) ReadString() (value string, err TProtocolException) {
  size, e := p.ReadI32()
  if e != nil {
    return "", e
  }
  return p.readStringBody(int(size))
}

func (p *TBinaryProtocol) ReadBinary() ([]byte, TProtocolException) {
  size, e := p.ReadI32()
  if e != nil {
    return nil, e
  }
  isize := int(size)
  e = p.checkReadLength(isize)
  if e != nil {
    return nil, e
  }
  buf := make([]byte, isize)
  _, err := p.trans.ReadAll(buf)
  return buf, NewTProtocolExceptionFromOsError(err)
}

func (p *TBinaryProtocol) Flush() (err TProtocolException) {
  return NewTProtocolExceptionFromOsError(p.trans.Flush())
}

func (p *TBinaryProtocol) Skip(fieldType TType) (err TProtocolException) {
  return SkipDefaultDepth(p, fieldType)
}

func (p *TBinaryProtocol) Transport() TTransport {
  return p.trans
}

func (p *TBinaryProtocol) readAll(buf []byte) TProtocolException {
  e := p.checkReadLength(len(buf))
  if e != nil {
    return e
  }
  _, err := p.trans.ReadAll(buf)
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TBinaryProtocol) setReadLength(readLength int) {
  p._ReadLength = readLength
  p._CheckReadLength = true
}

func (p *TBinaryProtocol) checkReadLength(length int) TProtocolException {
  if p._CheckReadLength {
    p._ReadLength = p._ReadLength - length
    if p._ReadLength < 0 {
      return NewTProtocolException(UNKNOWN_PROTOCOL_EXCEPTION, "Message length exceeded: "+string(length))
    }
  }
  return nil
}

func (p *TBinaryProtocol) readStringBody(size int) (value string, err TProtocolException) {
  if size < 0 {
    return "", nil
  }
  err = p.checkReadLength(size)
  if err != nil {
    return "", err
  }
  isize := int(size)
  buf := make([]byte, isize)
  _, e := p.trans.ReadAll(buf)
  return string(buf), NewTProtocolExceptionFromOsError(e)
}
