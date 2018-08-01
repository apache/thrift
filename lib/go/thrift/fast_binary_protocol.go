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
	"math"
)

var (
	limitReadBytes = 15 * (1 << 20)

	SafeBufferError = fmt.Errorf("Buffer is larger than the 15M")
)

type TFastFrameBinaryProtocol struct {
	origTransport TTransport
	frameBuf      *FastFrameBuffer
	strictRead    bool
	strictWrite   bool
	buffer        [64]byte
	useBuf        bool
}

type TFastFrameBinaryProtocolFactory struct {
	strictRead  bool
	strictWrite bool
	useBuf      bool
	bufSize     int
}

func NewTFastFrameBinaryProtocolTransport(t TTransport, bufSize int) *TFastFrameBinaryProtocol {
	return NewTFastFrameBinaryProtocol(t, false, true, bufSize)
}

func NewTFastFrameBinaryProtocol(t TTransport, strictRead, strictWrite bool, bufSize int) *TFastFrameBinaryProtocol {
	p := &TFastFrameBinaryProtocol{origTransport: t, strictRead: strictRead, strictWrite: strictWrite}
	frameTransport := t.(*TFramedTransport)
	p.frameBuf = NewFastBuffer(frameTransport, bufSize)
	return p
}

func NewTFastFrameBinaryProtocolFactoryDefault(bufSize int) *TFastFrameBinaryProtocolFactory {
	return NewTFastFrameBinaryProtocolFactory(false, true, bufSize)
}

func NewTFastFrameBinaryProtocolFactory(strictRead, strictWrite bool, bufSize int) *TFastFrameBinaryProtocolFactory {
	return &TFastFrameBinaryProtocolFactory{strictRead: strictRead, strictWrite: strictWrite, bufSize: bufSize}
}

func (p *TFastFrameBinaryProtocolFactory) GetProtocol(t TTransport) TProtocol {
	return NewTFastFrameBinaryProtocol(t, p.strictRead, p.strictWrite, p.bufSize)

}

/**
 * Writing Methods
 */

func (p *TFastFrameBinaryProtocol) WriteMessageBegin(name string, typeId TMessageType, seqId int32) error {
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
		e = p.WriteByte(int8(typeId))
		if e != nil {
			return e
		}
		e = p.WriteI32(seqId)
		return e
	}
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteMessageEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteStructBegin(name string) error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteStructEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteFieldBegin(name string, typeId TType, id int16) error {
	e := p.WriteByte(int8(typeId))
	if e != nil {
		return e
	}
	e = p.WriteI16(id)
	return e
}

func (p *TFastFrameBinaryProtocol) WriteFieldEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteFieldStop() error {
	e := p.WriteByte(STOP)
	return e
}

func (p *TFastFrameBinaryProtocol) WriteMapBegin(keyType TType, valueType TType, size int) error {
	e := p.WriteByte(int8(keyType))
	if e != nil {
		return e
	}
	e = p.WriteByte(int8(valueType))
	if e != nil {
		return e
	}
	e = p.WriteI32(int32(size))
	return e
}

func (p *TFastFrameBinaryProtocol) WriteMapEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteListBegin(elemType TType, size int) error {
	e := p.WriteByte(int8(elemType))
	if e != nil {
		return e
	}
	e = p.WriteI32(int32(size))
	return e
}

func (p *TFastFrameBinaryProtocol) WriteListEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteSetBegin(elemType TType, size int) error {
	e := p.WriteByte(int8(elemType))
	if e != nil {
		return e
	}
	e = p.WriteI32(int32(size))
	return e
}

func (p *TFastFrameBinaryProtocol) WriteSetEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteBool(value bool) error {
	if value {
		return p.WriteByte(1)
	}
	return p.WriteByte(0)
}

func (p *TFastFrameBinaryProtocol) WriteByte(value int8) error {
	p.frameBuf.WritByte(value)
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteI16(value int16) error {
	p.frameBuf.WriteI16(uint16(value))
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteI32(value int32) error {
	p.frameBuf.WriteI32(uint32(value))
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteI64(value int64) error {
	p.frameBuf.WriteI64(uint64(value))
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteDouble(value float64) error {
	return p.WriteI64(int64(math.Float64bits(value)))
}

func (p *TFastFrameBinaryProtocol) WriteString(value string) error {
	p.frameBuf.WriteI32(uint32(len(value)))
	p.frameBuf.WriteString(value)
	return nil
}

func (p *TFastFrameBinaryProtocol) WriteBinary(value []byte) error {
	p.WriteI32(int32(len(value)))
	p.frameBuf.WriteBytes(value)
	return nil
}

/**
 * Reading methods
 */

func (p *TFastFrameBinaryProtocol) ReadMessageBegin() (name string, typeId TMessageType, seqId int32, err error) {
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
	// TODO ?
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

func (p *TFastFrameBinaryProtocol) ReadMessageEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) ReadStructBegin() (name string, err error) {
	return
}

func (p *TFastFrameBinaryProtocol) ReadStructEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) ReadFieldBegin() (name string, typeId TType, seqId int16, err error) {
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

func (p *TFastFrameBinaryProtocol) ReadFieldEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) ReadMapBegin() (kType, vType TType, size int, err error) {
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
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	if size32 < 0 {
		err = invalidDataLength
		return
	}
	if size32 > int32(limitReadBytes) {
		err = SafeBufferError
		return
	}
	size = int(size32)
	return kType, vType, size, nil
}

func (p *TFastFrameBinaryProtocol) ReadMapEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) ReadListBegin() (elemType TType, size int, err error) {
	b, e := p.ReadByte()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	elemType = TType(b)
	size32, e := p.ReadI32()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	if size32 < 0 {
		err = invalidDataLength
		return
	}
	if size32 > int32(limitReadBytes) {
		err = SafeBufferError
		return
	}

	size = int(size32)

	return
}

func (p *TFastFrameBinaryProtocol) ReadListEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) ReadSetBegin() (elemType TType, size int, err error) {
	b, e := p.ReadByte()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	elemType = TType(b)
	size32, e := p.ReadI32()
	if e != nil {
		err = NewTProtocolException(e)
		return
	}
	if size32 < 0 {
		err = invalidDataLength
		return
	}
	if size32 > int32(limitReadBytes) {
		err = SafeBufferError
		return
	}
	size = int(size32)
	return elemType, size, nil
}

func (p *TFastFrameBinaryProtocol) ReadSetEnd() error {
	return nil
}

func (p *TFastFrameBinaryProtocol) ReadBool() (bool, error) {
	b, e := p.ReadByte()
	v := true
	if b != 1 {
		v = false
	}
	return v, e
}

func (p *TFastFrameBinaryProtocol) ReadByte() (value int8, err error) {
	c, err := p.frameBuf.ReadByte()
	return int8(c), err
}

func (p *TFastFrameBinaryProtocol) ReadI16() (value int16, err error) {
	return p.frameBuf.ReadI16()
}

func (p *TFastFrameBinaryProtocol) ReadI32() (value int32, err error) {
	return p.frameBuf.ReadI32()
}

func (p *TFastFrameBinaryProtocol) ReadI64() (value int64, err error) {
	return p.frameBuf.ReadI64()
}

func (p *TFastFrameBinaryProtocol) ReadDouble() (value float64, err error) {
	var buf []byte
	buf, err = p.frameBuf.ReadN(8)
	value = math.Float64frombits(binary.BigEndian.Uint64(buf))
	return value, err
}

func (p *TFastFrameBinaryProtocol) ReadString() (value string, err error) {
	var size int32
	var e error
	size, e = p.frameBuf.ReadI32()
	if e != nil {
		return "", e
	}
	if size < 0 {
		err = invalidDataLength
		return
	}
	dat, err := p.frameBuf.ReadN(int(size))
	//  TODO  more fast way
	return string(dat), err
}

func (p *TFastFrameBinaryProtocol) ReadBinary() ([]byte, error) {
	var size int32
	var e error
	size, e = p.frameBuf.ReadI32()
	if e != nil {
		return nil, e
	}
	if size < 0 {
		return nil, invalidDataLength
	}
	if size > int32(limitReadBytes) {
		return nil, SafeBufferError
	}
	dat, err := p.frameBuf.ReadN(int(size))
	return dat, err

}

func (p *TFastFrameBinaryProtocol) Flush() error {
	return p.frameBuf.Flush()
}

func (p *TFastFrameBinaryProtocol) Skip(fieldType TType) (err error) {
	return SkipDefaultDepth(p, fieldType)
}

func (p *TFastFrameBinaryProtocol) Transport() TTransport {
	return p.origTransport
}


func (p *TFastFrameBinaryProtocol) readStringBody(size int) (value string, err error) {
	if size < 0 {
		return "", nil
	}
	if size > int(limitReadBytes) {
		return "", SafeBufferError
	}
	buf, e := p.frameBuf.ReadN(size)
	return string(buf), e
}
