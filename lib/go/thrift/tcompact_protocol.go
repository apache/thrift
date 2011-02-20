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
  "container/vector"
  "encoding/binary"
  "fmt"
  "math"
  "os"
  "strings"
)

const (
  COMPACT_PROTOCOL_ID       = 0x082
  COMPACT_VERSION           = 1
  COMPACT_VERSION_MASK      = 0x1f
  COMPACT_TYPE_MASK         = 0x0E0
  COMPACT_TYPE_SHIFT_AMOUNT = 5
)

type TCompactType byte

const (
  COMPACT_BOOLEAN_TRUE  = 0x01
  COMPACT_BOOLEAN_FALSE = 0x02
  COMPACT_BYTE          = 0x03
  COMPACT_I16           = 0x04
  COMPACT_I32           = 0x05
  COMPACT_I64           = 0x06
  COMPACT_DOUBLE        = 0x07
  COMPACT_BINARY        = 0x08
  COMPACT_LIST          = 0x09
  COMPACT_SET           = 0x0A
  COMPACT_MAP           = 0x0B
  COMPACT_STRUCT        = 0x0C
)

var (
  _TTypeToCompactType []TCompactType
  _TSTOP              TField
)

func init() {
  _TSTOP = NewTField("", STOP, 0)
  _TTypeToCompactType = make([]TCompactType, int(UTF16)+1)
  _TTypeToCompactType[int(STOP)] = STOP
  _TTypeToCompactType[int(BOOL)] = COMPACT_BOOLEAN_TRUE
  _TTypeToCompactType[int(BYTE)] = COMPACT_BYTE
  _TTypeToCompactType[int(I16)] = COMPACT_I16
  _TTypeToCompactType[int(I32)] = COMPACT_I32
  _TTypeToCompactType[int(I64)] = COMPACT_I64
  _TTypeToCompactType[int(DOUBLE)] = COMPACT_DOUBLE
  _TTypeToCompactType[int(STRING)] = COMPACT_BINARY
  _TTypeToCompactType[int(LIST)] = COMPACT_LIST
  _TTypeToCompactType[int(SET)] = COMPACT_SET
  _TTypeToCompactType[int(MAP)] = COMPACT_MAP
  _TTypeToCompactType[int(STRUCT)] = COMPACT_STRUCT
}

type TCompactProtocolFactory struct{}

func NewTCompactProtocolFactory() *TCompactProtocolFactory {
  return &TCompactProtocolFactory{}
}

func (p *TCompactProtocolFactory) GetProtocol(trans TTransport) TProtocol {
  return NewTCompactProtocol(trans)
}

type TCompactProtocol struct {
  trans TTransport

  /** 
   * Used to keep track of the last field for the current and previous structs,
   * so we can do the delta stuff.
   */
  lastField   *vector.IntVector
  lastFieldId int

  /** 
   * If we encounter a boolean field begin, save the TField here so it can 
   * have the value incorporated.
   */
  booleanField TField

  /**
   * If we read a field header, and it's a boolean field, save the boolean 
   * value here so that readBool can use it.
   */
  boolValue          bool
  boolValueIsNotNull bool
}

/**
 * Create a TCompactProtocol.
 *
 * @param transport the TTransport object to read from or write to.
 */
func NewTCompactProtocol(trans TTransport) *TCompactProtocol {
  return &TCompactProtocol{trans: trans, lastField: &vector.IntVector{}}
}


//
// Public Writing methods.
//

/**
 * Write a message header to the wire. Compact Protocol messages contain the
 * protocol version so we can migrate forwards in the future if need be.
 */
func (p *TCompactProtocol) WriteMessageBegin(name string, typeId TMessageType, seqid int32) TProtocolException {
  _, err := p.writeByteDirect(COMPACT_PROTOCOL_ID)
  if err != nil {
    return NewTProtocolExceptionFromOsError(err)
  }
  _, err = p.writeByteDirect((COMPACT_VERSION & COMPACT_VERSION_MASK) | ((byte(typeId) << COMPACT_TYPE_SHIFT_AMOUNT) & COMPACT_TYPE_MASK))
  if err != nil {
    return NewTProtocolExceptionFromOsError(err)
  }
  _, err = p.writeVarint32(seqid)
  if err != nil {
    return NewTProtocolExceptionFromOsError(err)
  }
  e := p.WriteString(name)
  return e

}

func (p *TCompactProtocol) WriteMessageEnd() TProtocolException { return nil }

/**
 * Write a struct begin. This doesn't actually put anything on the wire. We 
 * use it as an opportunity to put special placeholder markers on the field
 * stack so we can get the field id deltas correct.
 */
func (p *TCompactProtocol) WriteStructBegin(name string) TProtocolException {
  p.lastField.Push(p.lastFieldId)
  p.lastFieldId = 0
  return nil
}

/**
 * Write a struct end. This doesn't actually put anything on the wire. We use
 * this as an opportunity to pop the last field from the current struct off
 * of the field stack.
 */
func (p *TCompactProtocol) WriteStructEnd() TProtocolException {
  p.lastFieldId = p.lastField.Pop()
  return nil
}

func (p *TCompactProtocol) WriteFieldBegin(name string, typeId TType, id int16) TProtocolException {
  if typeId == BOOL {
    // we want to possibly include the value, so we'll wait.
    p.booleanField = NewTField(name, typeId, int(id))
    return nil
  }
  _, err := p.writeFieldBeginInternal(name, typeId, id, 0xFF)
  return NewTProtocolExceptionFromOsError(err)
}


/**
 * The workhorse of writeFieldBegin. It has the option of doing a 
 * 'type override' of the type header. This is used specifically in the 
 * boolean field case.
 */
func (p *TCompactProtocol) writeFieldBeginInternal(name string, typeId TType, id int16, typeOverride byte) (int, os.Error) {
  // short lastField = lastField_.pop();

  // if there's a type override, use that.
  var typeToWrite byte
  if typeOverride == 0xFF {
    typeToWrite = byte(p.getCompactType(typeId))
  } else {
    typeToWrite = typeOverride
  }
  // check if we can use delta encoding for the field id
  fieldId := int(id)
  written := 0
  if fieldId > p.lastFieldId && fieldId-p.lastFieldId <= 15 {
    // write them together
    written, err := p.writeByteDirect(byte((fieldId-p.lastFieldId)<<4) | typeToWrite)
    if err != nil {
      return written, err
    }
  } else {
    // write them separate
    n, err := p.writeByteDirect(typeToWrite)
    if err != nil {
      return n, err
    }
    err = p.WriteI16(id)
    written = n + 2
    if err != nil {
      return written, err
    }
  }

  p.lastFieldId = fieldId
  // p.lastField.Push(field.id);
  return written, nil
}


func (p *TCompactProtocol) WriteFieldEnd() TProtocolException { return nil }

func (p *TCompactProtocol) WriteFieldStop() TProtocolException {
  _, err := p.writeByteDirect(STOP)
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TCompactProtocol) WriteMapBegin(keyType TType, valueType TType, size int) TProtocolException {
  if size == 0 {
    _, err := p.writeByteDirect(0)
    return NewTProtocolExceptionFromOsError(err)
  }
  _, err := p.writeVarint32(int32(size))
  if err != nil {
    return NewTProtocolExceptionFromOsError(err)
  }
  _, err = p.writeByteDirect(byte(p.getCompactType(keyType))<<4 | byte(p.getCompactType(valueType)))
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TCompactProtocol) WriteMapEnd() TProtocolException { return nil }

/**
 * Write a list header.
 */
func (p *TCompactProtocol) WriteListBegin(elemType TType, size int) TProtocolException {
  _, err := p.writeCollectionBegin(elemType, size)
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TCompactProtocol) WriteListEnd() TProtocolException { return nil }

/**
 * Write a set header.
 */
func (p *TCompactProtocol) WriteSetBegin(elemType TType, size int) TProtocolException {
  _, err := p.writeCollectionBegin(elemType, size)
  return NewTProtocolExceptionFromOsError(err)
}

func (p *TCompactProtocol) WriteSetEnd() TProtocolException { return nil }

func (p *TCompactProtocol) WriteBool(value bool) TProtocolException {
  v := byte(COMPACT_BOOLEAN_FALSE)
  if value {
    v = byte(COMPACT_BOOLEAN_TRUE)
  }
  if p.booleanField != nil {
    // we haven't written the field header yet
    _, err := p.writeFieldBeginInternal(p.booleanField.Name(), p.booleanField.TypeId(), int16(p.booleanField.Id()), v)
    p.booleanField = nil
    return NewTProtocolExceptionFromOsError(err)
  }
  // we're not part of a field, so just write the value.
  _, err := p.writeByteDirect(v)
  return NewTProtocolExceptionFromOsError(err)
}

/** 
 * Write a byte. Nothing to see here!
 */
func (p *TCompactProtocol) WriteByte(value byte) TProtocolException {
  _, err := p.writeByteDirect(value)
  return NewTProtocolExceptionFromOsError(err)
}

/**
 * Write an I16 as a zigzag varint.
 */
func (p *TCompactProtocol) WriteI16(value int16) TProtocolException {
  _, err := p.writeVarint32(p.int32ToZigzag(int32(value)))
  return NewTProtocolExceptionFromOsError(err)
}

/**
 * Write an i32 as a zigzag varint.
 */
func (p *TCompactProtocol) WriteI32(value int32) TProtocolException {
  _, err := p.writeVarint32(p.int32ToZigzag(value))
  return NewTProtocolExceptionFromOsError(err)
}

/**
 * Write an i64 as a zigzag varint.
 */
func (p *TCompactProtocol) WriteI64(value int64) TProtocolException {
  _, err := p.writeVarint64(p.int64ToZigzag(value))
  return NewTProtocolExceptionFromOsError(err)
}

/**
 * Write a double to the wire as 8 bytes.
 */
func (p *TCompactProtocol) WriteDouble(value float64) TProtocolException {
  buf := make([]byte, 8)
  binary.LittleEndian.PutUint64(buf, math.Float64bits(value))
  _, err := p.trans.Write(buf)
  return NewTProtocolExceptionFromOsError(err)
}

/**
 * Write a string to the wire with a varint size preceeding.
 */
func (p *TCompactProtocol) WriteString(value string) TProtocolException {
  buf := make([]byte, len(value))
  strings.NewReader(value).Read(buf)
  return p.WriteBinary(buf)
}

/**
 * Write a byte array, using a varint for the size. 
 */
func (p *TCompactProtocol) WriteBinary(bin []byte) TProtocolException {
  _, e := p.writeVarint32(int32(len(bin)))
  if e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  if len(bin) > 0 {
    _, e = p.trans.Write(bin)
    return NewTProtocolExceptionFromOsError(e)
  }
  return nil
}


// 
// Reading methods.
// 

/**
 * Read a message header. 
 */
func (p *TCompactProtocol) ReadMessageBegin() (name string, typeId TMessageType, seqId int32, err TProtocolException) {
  protocolId, err := p.ReadByte()
  if protocolId != COMPACT_PROTOCOL_ID {
    s := fmt.Sprintf("Expected protocol id %02x but got %02x", COMPACT_PROTOCOL_ID, protocolId)
    return "", typeId, seqId, NewTProtocolException(BAD_VERSION, s)
  }
  versionAndType, err := p.ReadByte()
  version := versionAndType & COMPACT_VERSION_MASK
  typeId = TMessageType((versionAndType >> COMPACT_TYPE_SHIFT_AMOUNT) & 0x03)
  if err != nil {
    return
  }
  if version != COMPACT_VERSION {
    s := fmt.Sprintf("Expected version %02x but got %02x", COMPACT_VERSION, version)
    err = NewTProtocolException(BAD_VERSION, s)
    return
  }
  seqId, e := p.readVarint32()
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  name, err = p.ReadString()
  return
}

func (p *TCompactProtocol) ReadMessageEnd() TProtocolException { return nil }

/**
 * Read a struct begin. There's nothing on the wire for this, but it is our
 * opportunity to push a new struct begin marker onto the field stack.
 */
func (p *TCompactProtocol) ReadStructBegin() (name string, err TProtocolException) {
  p.lastField.Push(p.lastFieldId)
  p.lastFieldId = 0
  return
}

/**
 * Doesn't actually consume any wire data, just removes the last field for 
 * this struct from the field stack.
 */
func (p *TCompactProtocol) ReadStructEnd() TProtocolException {
  // consume the last field we read off the wire.
  p.lastFieldId = p.lastField.Pop()
  return nil
}

/**
 * Read a field header off the wire. 
 */
func (p *TCompactProtocol) ReadFieldBegin() (name string, typeId TType, id int16, err TProtocolException) {
  t, err := p.ReadByte()
  if err != nil {
    return
  }

  // if it's a stop, then we can return immediately, as the struct is over.
  if (t & 0x0f) == STOP {
    return _TSTOP.Name(), _TSTOP.TypeId(), int16(_TSTOP.Id()), nil
  }

  // mask off the 4 MSB of the type header. it could contain a field id delta.
  modifier := int16((t & 0xf0) >> 4)
  if modifier == 0 {
    // not a delta. look ahead for the zigzag varint field id.
    id, err = p.ReadI16()
    if err != nil {
      return
    }
  } else {
    // has a delta. add the delta to the last read field id.
    id = int16(p.lastFieldId) + modifier
  }
  typeId, e := p.getTType(TCompactType(t & 0x0f))
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }

  // if this happens to be a boolean field, the value is encoded in the type
  if p.isBoolType(t) {
    // save the boolean value in a special instance variable.
    p.boolValue = (byte(t)&0x0f == COMPACT_BOOLEAN_TRUE)
    p.boolValueIsNotNull = true
  }

  // push the new field onto the field stack so we can keep the deltas going.
  p.lastFieldId = int(id)
  return
}

func (p *TCompactProtocol) ReadFieldEnd() TProtocolException { return nil }

/** 
 * Read a map header off the wire. If the size is zero, skip reading the key
 * and value type. This means that 0-length maps will yield TMaps without the
 * "correct" types.
 */
func (p *TCompactProtocol) ReadMapBegin() (keyType TType, valueType TType, size int, err TProtocolException) {
  size32, e := p.readVarint32()
  size = int(size32)
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  keyAndValueType := byte(STOP)
  if size != 0 {
    keyAndValueType, err = p.ReadByte()
    if err != nil {
      return
    }
  }
  keyType, _ = p.getTType(TCompactType(keyAndValueType >> 4))
  valueType, _ = p.getTType(TCompactType(keyAndValueType & 0xf))
  return
}

func (p *TCompactProtocol) ReadMapEnd() TProtocolException { return nil }

/**
 * Read a list header off the wire. If the list size is 0-14, the size will 
 * be packed into the element type header. If it's a longer list, the 4 MSB
 * of the element type header will be 0xF, and a varint will follow with the
 * true size.
 */
func (p *TCompactProtocol) ReadListBegin() (elemType TType, size int, err TProtocolException) {
  size_and_type, err := p.ReadByte()
  if err != nil {
    return
  }
  size = int((size_and_type >> 4) & 0x0f)
  if size == 15 {
    size2, e := p.readVarint32()
    if e != nil {
      err = NewTProtocolExceptionFromOsError(e)
      return
    }
    size = int(size2)
  }
  elemType, e := p.getTType(TCompactType(size_and_type))
  if e != nil {
    err = NewTProtocolExceptionFromOsError(e)
    return
  }
  return
}

func (p *TCompactProtocol) ReadListEnd() TProtocolException { return nil }

/**
 * Read a set header off the wire. If the set size is 0-14, the size will 
 * be packed into the element type header. If it's a longer set, the 4 MSB
 * of the element type header will be 0xF, and a varint will follow with the
 * true size.
 */
func (p *TCompactProtocol) ReadSetBegin() (elemType TType, size int, err TProtocolException) {
  return p.ReadListBegin()
}

func (p *TCompactProtocol) ReadSetEnd() TProtocolException { return nil }

/**
 * Read a boolean off the wire. If this is a boolean field, the value should
 * already have been read during readFieldBegin, so we'll just consume the
 * pre-stored value. Otherwise, read a byte.
 */
func (p *TCompactProtocol) ReadBool() (value bool, err TProtocolException) {
  if p.boolValueIsNotNull {
    p.boolValueIsNotNull = false
    return p.boolValue, nil
  }
  v, err := p.ReadByte()
  return v == COMPACT_BOOLEAN_TRUE, err
}

/**
 * Read a single byte off the wire. Nothing interesting here.
 */
func (p *TCompactProtocol) ReadByte() (value byte, err TProtocolException) {
  buf := []byte{0}
  _, e := p.trans.ReadAll(buf)
  if e != nil {
    return 0, NewTProtocolExceptionFromOsError(e)
  }
  return buf[0], nil
}

/**
 * Read an i16 from the wire as a zigzag varint.
 */
func (p *TCompactProtocol) ReadI16() (value int16, err TProtocolException) {
  v, err := p.ReadI32()
  return int16(v), err
}

/**
 * Read an i32 from the wire as a zigzag varint.
 */
func (p *TCompactProtocol) ReadI32() (value int32, err TProtocolException) {
  v, e := p.readVarint32()
  if e != nil {
    return 0, NewTProtocolExceptionFromOsError(e)
  }
  value = p.zigzagToInt32(v)
  return value, nil
}

/**
 * Read an i64 from the wire as a zigzag varint.
 */
func (p *TCompactProtocol) ReadI64() (value int64, err TProtocolException) {
  v, e := p.readVarint64()
  if e != nil {
    return 0, NewTProtocolExceptionFromOsError(e)
  }
  value = p.zigzagToInt64(v)
  return value, nil
}

/**
 * No magic here - just read a double off the wire.
 */
func (p *TCompactProtocol) ReadDouble() (value float64, err TProtocolException) {
  longBits := make([]byte, 8)
  _, e := p.trans.ReadAll(longBits)
  if e != nil {
    return 0.0, NewTProtocolExceptionFromOsError(e)
  }
  return math.Float64frombits(p.bytesToUint64(longBits)), nil
}

/**
 * Reads a []byte (via readBinary), and then UTF-8 decodes it.
 */
func (p *TCompactProtocol) ReadString() (value string, err TProtocolException) {
  v, e := p.ReadBinary()
  return string(v), NewTProtocolExceptionFromOsError(e)
}

/**
 * Read a []byte from the wire. 
 */
func (p *TCompactProtocol) ReadBinary() (value []byte, err TProtocolException) {
  length, e := p.readVarint32()
  if e != nil {
    return []byte{}, NewTProtocolExceptionFromOsError(e)
  }
  if length == 0 {
    return []byte{}, nil
  }

  buf := make([]byte, length)
  p.trans.ReadAll(buf)
  return buf, nil
}

func (p *TCompactProtocol) Flush() (err TProtocolException) {
  return NewTProtocolExceptionFromOsError(p.trans.Flush())
}

func (p *TCompactProtocol) Skip(fieldType TType) (err TProtocolException) {
  return SkipDefaultDepth(p, fieldType)
}

func (p *TCompactProtocol) Transport() TTransport {
  return p.trans
}

//
// Internal writing methods
//

/**
 * Abstract method for writing the start of lists and sets. List and sets on 
 * the wire differ only by the type indicator.
 */
func (p *TCompactProtocol) writeCollectionBegin(elemType TType, size int) (int, os.Error) {
  if size <= 14 {
    return p.writeByteDirect(byte(int32(size<<4) | int32(p.getCompactType(elemType))))
  }
  n, err := p.writeByteDirect(0xf0 | byte(p.getCompactType(elemType)))
  if err != nil {
    return n, err
  }
  m, err := p.writeVarint32(int32(size))
  return n + m, err
}

/**
 * Write an i32 as a varint. Results in 1-5 bytes on the wire.
 * TODO(pomack): make a permanent buffer like writeVarint64?
 */
func (p *TCompactProtocol) writeVarint32(n int32) (int, os.Error) {
  i32buf := make([]byte, 5)
  idx := 0
  for {
    if (n & ^0x7F) == 0 {
      i32buf[idx] = byte(n)
      idx++
      // p.writeByteDirect(byte(n));
      break
      // return;
    } else {
      i32buf[idx] = byte((n & 0x7F) | 0x80)
      idx++
      // p.writeByteDirect(byte(((n & 0x7F) | 0x80)));
      u := uint32(n)
      n = int32(u >> 7)
    }
  }
  return p.trans.Write(i32buf[0:idx])
}

/**
 * Write an i64 as a varint. Results in 1-10 bytes on the wire.
 */
func (p *TCompactProtocol) writeVarint64(n int64) (int, os.Error) {
  varint64out := make([]byte, 10)
  idx := 0
  for {
    if (n & ^0x7F) == 0 {
      varint64out[idx] = byte(n)
      idx++
      break
    } else {
      varint64out[idx] = byte((n & 0x7F) | 0x80)
      idx++
      u := uint64(n)
      n = int64(u >> 7)
    }
  }
  return p.trans.Write(varint64out[0:idx])
}

/**
 * Convert l into a zigzag long. This allows negative numbers to be 
 * represented compactly as a varint.
 */
func (p *TCompactProtocol) int64ToZigzag(l int64) int64 {
  return (l << 1) ^ (l >> 63)
}

/**
 * Convert l into a zigzag long. This allows negative numbers to be 
 * represented compactly as a varint.
 */
func (p *TCompactProtocol) int32ToZigzag(n int32) int32 {
  return (n << 1) ^ (n >> 31)
}

func (p *TCompactProtocol) fixedUint64ToBytes(n uint64, buf []byte) {
  binary.LittleEndian.PutUint64(buf, n)
}

func (p *TCompactProtocol) fixedInt64ToBytes(n int64, buf []byte) {
  binary.LittleEndian.PutUint64(buf, uint64(n))
}

/** 
 * Writes a byte without any possiblity of all that field header nonsense. 
 * Used internally by other writing methods that know they need to write a byte.
 */
func (p *TCompactProtocol) writeByteDirect(b byte) (int, os.Error) {
  return p.trans.Write([]byte{b})
}

/** 
 * Writes a byte without any possiblity of all that field header nonsense.
 */
func (p *TCompactProtocol) writeIntAsByteDirect(n int) (int, os.Error) {
  return p.writeByteDirect(byte(n))
}


//
// Internal reading methods
//

/**
 * Read an i32 from the wire as a varint. The MSB of each byte is set
 * if there is another byte to follow. This can read up to 5 bytes.
 */
func (p *TCompactProtocol) readVarint32() (int32, os.Error) {
  // if the wire contains the right stuff, this will just truncate the i64 we
  // read and get us the right sign.
  v, err := p.readVarint64()
  return int32(v), err
}


/**
 * Read an i64 from the wire as a proper varint. The MSB of each byte is set 
 * if there is another byte to follow. This can read up to 10 bytes.
 */
func (p *TCompactProtocol) readVarint64() (int64, os.Error) {
  shift := uint(0)
  result := int64(0)
  for {
    b, err := p.ReadByte()
    if err != nil {
      return 0, err
    }
    result |= int64(b&0x7f) << shift
    if (b & 0x80) != 0x80 {
      break
    }
    shift += 7
  }
  return result, nil
}


//
// encoding helpers
//

/**
 * Convert from zigzag int to int.
 */
func (p *TCompactProtocol) zigzagToInt32(n int32) int32 {
  u := uint32(n)
  return int32(u>>1) ^ -(n & 1)
}

/** 
 * Convert from zigzag long to long.
 */
func (p *TCompactProtocol) zigzagToInt64(n int64) int64 {
  u := uint64(n)
  return int64(u>>1) ^ -(n & 1)
}

/**
 * Note that it's important that the mask bytes are long literals, 
 * otherwise they'll default to ints, and when you shift an int left 56 bits,
 * you just get a messed up int.
 */
func (p *TCompactProtocol) bytesToInt64(b []byte) int64 {
  return int64(binary.LittleEndian.Uint64(b))
}

/**
 * Note that it's important that the mask bytes are long literals, 
 * otherwise they'll default to ints, and when you shift an int left 56 bits,
 * you just get a messed up int.
 */
func (p *TCompactProtocol) bytesToUint64(b []byte) uint64 {
  return binary.LittleEndian.Uint64(b)
}

//
// type testing and converting
//

func (p *TCompactProtocol) isBoolType(b byte) bool {
  return (b&0x0f) == COMPACT_BOOLEAN_TRUE || (b&0x0f) == COMPACT_BOOLEAN_FALSE
}

/**
 * Given a TCompactType constant, convert it to its corresponding 
 * TType value.
 */
func (p *TCompactProtocol) getTType(t TCompactType) (TType, os.Error) {
  switch byte(t) & 0x0f {
  case STOP:
    return STOP, nil
  case COMPACT_BOOLEAN_FALSE:
  case COMPACT_BOOLEAN_TRUE:
    return BOOL, nil
  case COMPACT_BYTE:
    return BYTE, nil
  case COMPACT_I16:
    return I16, nil
  case COMPACT_I32:
    return I32, nil
  case COMPACT_I64:
    return I64, nil
  case COMPACT_DOUBLE:
    return DOUBLE, nil
  case COMPACT_BINARY:
    return STRING, nil
  case COMPACT_LIST:
    return LIST, nil
  case COMPACT_SET:
    return SET, nil
  case COMPACT_MAP:
    return MAP, nil
  case COMPACT_STRUCT:
    return STRUCT, nil
  }
  return STOP, NewTException("don't know what type: " + string(t&0x0f))
}

/**
 * Given a TType value, find the appropriate TCompactProtocol.Types constant.
 */
func (p *TCompactProtocol) getCompactType(t TType) TCompactType {
  return _TTypeToCompactType[int(t)]
}
