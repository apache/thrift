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
	"fmt"
	"io"
	"math"
	"strings"
)

type TBinaryProtocol struct {
	trans            TTransport
	strictRead      bool
	strictWrite     bool
	readLength      int
	checkReadLength bool
}

type TBinaryProtocolFactory struct {
	strictRead  bool
	strictWrite bool
}

func NewTBinaryProtocolTransport(t TTransport) *TBinaryProtocol {
	return NewTBinaryProtocol(t, false, true)
}

func NewTBinaryProtocol(t TTransport, strictRead, strictWrite bool) *TBinaryProtocol {
	//return &TBinaryProtocol{TProtocolBase:TProtocolBase{trans:t}, strictRead:strictRead, strictWrite:strictWrite, readLength:0, checkReadLength:false};
	return &TBinaryProtocol{trans: t, strictRead: strictRead, strictWrite: strictWrite, readLength: 0, checkReadLength: false}
}

func NewTBinaryProtocolFactoryDefault() *TBinaryProtocolFactory {
	return NewTBinaryProtocolFactory(false, true)
}

func NewTBinaryProtocolFactory(strictRead, strictWrite bool) *TBinaryProtocolFactory {
	return &TBinaryProtocolFactory{strictRead: strictRead, strictWrite: strictWrite}
}

func (p *TBinaryProtocolFactory) GetProtocol(t TTransport) TProtocol {
	return NewTBinaryProtocol(t, p.strictRead, p.strictWrite)
}

/**
 * Writing Methods
 */

func (p *TBinaryProtocol) WriteMessageBegin(name string, typeId TMessageType, seqId int32) error {
	if p.strictWrite {
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

func (p *TBinaryProtocol) WriteMessageEnd() error {
	return nil
}

func (p *TBinaryProtocol) WriteStructBegin(name string) error {
	return nil
}

func (p *TBinaryProtocol) WriteStructEnd() error {
	return nil
}

func (p *TBinaryProtocol) WriteFieldBegin(name string, typeId TType, id int16) error {
	e := p.WriteByte(byte(typeId))
	if e != nil {
		return e
	}
	e = p.WriteI16(id)
	return e
}

func (p *TBinaryProtocol) WriteFieldEnd() error {
	return nil
}

func (p *TBinaryProtocol) WriteFieldStop() error {
	e := p.WriteByte(STOP)
	return e
}

func (p *TBinaryProtocol) WriteMapBegin(keyType TType, valueType TType, size int) error {
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

func (p *TBinaryProtocol) WriteMapEnd() error {
	return nil
}

func (p *TBinaryProtocol) WriteListBegin(elemType TType, size int) error {
	e := p.WriteByte(byte(elemType))
	if e != nil {
		return e
	}
	e = p.WriteI32(int32(size))
	return e
}

func (p *TBinaryProtocol) WriteListEnd() error {
	return nil
}

func (p *TBinaryProtocol) WriteSetBegin(elemType TType, size int) error {
	e := p.WriteByte(byte(elemType))
	if e != nil {
		return e
	}
	e = p.WriteI32(int32(size))
	return e
}

func (p *TBinaryProtocol) WriteSetEnd() error {
	return nil
}

func (p *TBinaryProtocol) WriteBool(value bool) error {
	if value {
		return p.WriteByte(1)
	}
	return p.WriteByte(0)
}

func (p *TBinaryProtocol) WriteByte(value byte) error {
	v := []byte{value}
	_, e := p.trans.Write(v)
	return NewTProtocolException(e)
}

func (p *TBinaryProtocol) WriteI16(value int16) error {
	h := byte(0xff & (value >> 8))
	l := byte(0xff & value)
	v := []byte{h, l}
	_, e := p.trans.Write(v)
	return NewTProtocolException(e)
}

func (p *TBinaryProtocol) WriteI32(value int32) error {
	a := byte(0xff & (value >> 24))
	b := byte(0xff & (value >> 16))
	c := byte(0xff & (value >> 8))
	d := byte(0xff & value)
	v := []byte{a, b, c, d}
	_, e := p.trans.Write(v)
	return NewTProtocolException(e)
}

func (p *TBinaryProtocol) WriteI64(value int64) error {
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
	return NewTProtocolException(err)
}

func (p *TBinaryProtocol) WriteDouble(value float64) error {
	return p.WriteI64(int64(math.Float64bits(value)))
}

func (p *TBinaryProtocol) WriteString(value string) error {
	return p.WriteBinaryFromReader(strings.NewReader(value), len(value))
}

func (p *TBinaryProtocol) WriteBinary(value []byte) error {
	e := p.WriteI32(int32(len(value)))
	if e != nil {
		return e
	}
	_, err := p.trans.Write(value)
	return NewTProtocolException(err)
}

func (p *TBinaryProtocol) WriteBinaryFromReader(reader io.Reader, size int) error {
	e := p.WriteI32(int32(size))
	if e != nil {
		return e
	}
	_, err := io.CopyN(p.trans, reader, int64(size))
	return NewTProtocolException(err)
}

/**
 * Reading methods
 */

func (p *TBinaryProtocol) ReadMessageBegin() (name string, typeId TMessageType, seqId int32, err error) {
	size, e := p.ReadI32()
	if e != nil {
		return "", typeId, 0, NewTProtocolException(e)
	}
	if size < 0 {
		typeId = TMessageType(size & 0x0ff)
		version := int64(int64(size) & VERSION_MASK)
		if version != VERSION_1 {
			return name, typeId, seqId, NewTProtocolExceptionWithType(BAD_VERSION, fmt.Errorf("Bad version in ReadMessageBegin"))
		}
		name, e = p.ReadString()
		if e != nil {
			return name, typeId, seqId, NewTProtocolException(e)
		}
		seqId, e = p.ReadI32()
		if e != nil {
			return name, typeId, seqId, NewTProtocolException(e)
		}
		return name, typeId, seqId, nil
	}
	if p.strictRead {
		return name, typeId, seqId, NewTProtocolExceptionWithType(BAD_VERSION, fmt.Errorf("Missing version in ReadMessageBegin"))
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

func (p *TBinaryProtocol) ReadMessageEnd() error {
	return nil
}

func (p *TBinaryProtocol) ReadStructBegin() (name string, err error) {
	return
}

func (p *TBinaryProtocol) ReadStructEnd() error {
	return nil
}

func (p *TBinaryProtocol) ReadFieldBegin() (name string, typeId TType, seqId int16, err error) {
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

func (p *TBinaryProtocol) ReadFieldEnd() error {
	return nil
}

func (p *TBinaryProtocol) ReadMapBegin() (kType, vType TType, size int, err error) {
	k, e := p.ReadByte()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	kType = TType(k)
	v, e := p.ReadByte()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	vType = TType(v)
	size32, e := p.ReadI32()
	size = int(size32)
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	return kType, vType, size, nil
}

func (p *TBinaryProtocol) ReadMapEnd() error {
	return nil
}

func (p *TBinaryProtocol) ReadListBegin() (elemType TType, size int, err error) {
	b, e := p.ReadByte()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	elemType = TType(b)
	size32, e := p.ReadI32()
	size = int(size32)
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	return elemType, size, nil
}

func (p *TBinaryProtocol) ReadListEnd() error {
	return nil
}

func (p *TBinaryProtocol) ReadSetBegin() (elemType TType, size int, err error) {
	b, e := p.ReadByte()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	elemType = TType(b)
	size32, e := p.ReadI32()
	size = int(size32)
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	return elemType, size, nil
}

func (p *TBinaryProtocol) ReadSetEnd() error {
	return nil
}

func (p *TBinaryProtocol) ReadBool() (bool, error) {
	b, e := p.ReadByte()
	v := true
	if b != 1 {
		v = false
	}
	return v, e
}

func (p *TBinaryProtocol) ReadByte() (value byte, err error) {
	buf := []byte{0}
	err = p.readAll(buf)
	return buf[0], err
}

func (p *TBinaryProtocol) ReadI16() (value int16, err error) {
	buf := []byte{0, 0}
	err = p.readAll(buf)
	value = int16(binary.BigEndian.Uint16(buf))
	return value, err
}

func (p *TBinaryProtocol) ReadI32() (value int32, err error) {
	buf := []byte{0, 0, 0, 0}
	err = p.readAll(buf)
	value = int32(binary.BigEndian.Uint32(buf))
	return value, err
}

func (p *TBinaryProtocol) ReadI64() (value int64, err error) {
	buf := []byte{0, 0, 0, 0, 0, 0, 0, 0}
	err = p.readAll(buf)
	value = int64(binary.BigEndian.Uint64(buf))
	return value, err
}

func (p *TBinaryProtocol) ReadDouble() (value float64, err error) {
	buf := []byte{0, 0, 0, 0, 0, 0, 0, 0}
	err = p.readAll(buf)
	value = math.Float64frombits(binary.BigEndian.Uint64(buf))
	return value, err
}

func (p *TBinaryProtocol) ReadString() (value string, err error) {
	size, e := p.ReadI32()
	if e != nil {
		return "", e
	}
	return p.readStringBody(int(size))
}

func (p *TBinaryProtocol) ReadBinary() ([]byte, error) {
	size, e := p.ReadI32()
	if e != nil {
		return nil, e
	}
	isize := int(size)
	if e = p.readLengthOk(isize); e != nil {
		return nil, e
	}
	buf := make([]byte, isize)
	_, err := io.ReadFull(p.trans, buf)
	return buf, NewTProtocolException(err)
}

func (p *TBinaryProtocol) Flush() (err error) {
	return NewTProtocolException(p.trans.Flush())
}

func (p *TBinaryProtocol) Skip(fieldType TType) (err error) {
	return SkipDefaultDepth(p, fieldType)
}

func (p *TBinaryProtocol) Transport() TTransport {
	return p.trans
}

func (p *TBinaryProtocol) readAll(buf []byte) error {
	if e := p.readLengthOk(len(buf)); e != nil {
		return e
	}
	_, err := io.ReadFull(p.trans, buf)
	return NewTProtocolException(err)
}

func (p *TBinaryProtocol) setReadLength(readLength int) {
	p.readLength = readLength
	p.checkReadLength = true
}

func (p *TBinaryProtocol) readLengthOk(length int) error {
	if p.checkReadLength {
		p.readLength = p.readLength - length
		if p.readLength < 0 {
			return NewTProtocolExceptionWithType(UNKNOWN_PROTOCOL_EXCEPTION, fmt.Errorf("Message length exceeded: %d", length))
		}
	}
	return nil
}

func (p *TBinaryProtocol) readStringBody(size int) (value string, err error) {
	if size < 0 {
		return "", nil
	}
	if err := p.readLengthOk(size); err != nil {
		return "", err
	}
	isize := int(size)
	buf := make([]byte, isize)
	_, e := io.ReadFull(p.trans, buf)
	return string(buf), NewTProtocolException(e)
}
