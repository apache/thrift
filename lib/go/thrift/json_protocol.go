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
	"encoding/base64"
	"fmt"
)

const (
	THRIFT_JSON_PROTOCOL_VERSION = 1
)

// for references to _ParseContext see tsimplejson_protocol.go

// JSON protocol implementation for thrift.
//
// This protocol produces/consumes a simple output format
// suitable for parsing by scripting languages.  It should not be
// confused with the full-featured TJSONProtocol.
//
type TJSONProtocol struct {
	*TSimpleJSONProtocol
}

// Constructor
func NewTJSONProtocol(t TTransport) *TJSONProtocol {
	v := &TJSONProtocol{TSimpleJSONProtocol: NewTSimpleJSONProtocol(t)}
	v.parseContextStack = append(v.parseContextStack, int(_CONTEXT_IN_TOPLEVEL))
	v.dumpContext = append(v.dumpContext, int(_CONTEXT_IN_TOPLEVEL))
	return v
}

// Factory
type TJSONProtocolFactory struct{}

func (p *TJSONProtocolFactory) GetProtocol(trans TTransport) TProtocol {
	return NewTJSONProtocol(trans)
}

func NewTJSONProtocolFactory() *TJSONProtocolFactory {
	return &TJSONProtocolFactory{}
}

func (p *TJSONProtocol) WriteMessageBegin(name string, typeId TMessageType, seqId int32) error {
	if e := p.OutputListBegin(); e != nil {
		return e
	}
	if e := p.WriteI32(THRIFT_JSON_PROTOCOL_VERSION); e != nil {
		return e
	}
	if e := p.WriteString(name); e != nil {
		return e
	}
	if e := p.WriteByte(byte(typeId)); e != nil {
		return e
	}
	if e := p.WriteI32(seqId); e != nil {
		return e
	}
	return nil
}

func (p *TJSONProtocol) WriteMessageEnd() error {
	return p.OutputListEnd()
}

func (p *TJSONProtocol) WriteStructBegin(name string) error {
	if e := p.OutputObjectBegin(); e != nil {
		return e
	}
	return nil
}

func (p *TJSONProtocol) WriteStructEnd() error {
	return p.OutputObjectEnd()
}

func (p *TJSONProtocol) WriteFieldBegin(name string, typeId TType, id int16) error {
	if e := p.WriteI16(id); e != nil {
		return e
	}
	if e := p.OutputObjectBegin(); e != nil {
		return e
	}
	if e := p.WriteString(p.TypeIdToString(typeId)); e != nil {
		return e
	}
	return nil
}

func (p *TJSONProtocol) WriteFieldEnd() error {
	return p.OutputObjectEnd()
}

func (p *TJSONProtocol) WriteFieldStop() error { return nil }

func (p *TJSONProtocol) WriteMapBegin(keyType TType, valueType TType, size int) error {
	if e := p.OutputListBegin(); e != nil {
		return e
	}
	if e := p.WriteString(p.TypeIdToString(keyType)); e != nil {
		return e
	}
	if e := p.WriteString(p.TypeIdToString(valueType)); e != nil {
		return e
	}
	return p.WriteI64(int64(size))
}

func (p *TJSONProtocol) WriteMapEnd() error {
	return p.OutputListEnd()
}

func (p *TJSONProtocol) WriteListBegin(elemType TType, size int) error {
	return p.OutputElemListBegin(elemType, size)
}

func (p *TJSONProtocol) WriteListEnd() error {
	return p.OutputListEnd()
}

func (p *TJSONProtocol) WriteSetBegin(elemType TType, size int) error {
	return p.OutputElemListBegin(elemType, size)
}

func (p *TJSONProtocol) WriteSetEnd() error {
	return p.OutputListEnd()
}

func (p *TJSONProtocol) WriteBool(b bool) error {
	return p.OutputBool(b)
}

func (p *TJSONProtocol) WriteByte(b byte) error {
	return p.WriteI32(int32(b))
}

func (p *TJSONProtocol) WriteI16(v int16) error {
	return p.WriteI32(int32(v))
}

func (p *TJSONProtocol) WriteI32(v int32) error {
	return p.OutputI64(int64(v))
}

func (p *TJSONProtocol) WriteI64(v int64) error {
	return p.OutputI64(int64(v))
}

func (p *TJSONProtocol) WriteDouble(v float64) error {
	return p.OutputF64(v)
}

func (p *TJSONProtocol) WriteString(v string) error {
	return p.OutputString(v)
}

func (p *TJSONProtocol) WriteBinary(v []byte) error {
	// JSON library only takes in a string,
	// not an arbitrary byte array, to ensure bytes are transmitted
	// efficiently we must convert this into a valid JSON string
	// therefore we use base64 encoding to avoid excessive escaping/quoting
	if e := p.OutputPreValue(); e != nil {
		return e
	}
	p.writer.Write(JSON_QUOTE_BYTES)
	writer := base64.NewEncoder(base64.StdEncoding, p.writer)
	if _, e := writer.Write(v); e != nil {
		return NewTProtocolException(e)
	}
	writer.Close()
	p.writer.Write(JSON_QUOTE_BYTES)
	return p.OutputPostValue()
}

// Reading methods.

func (p *TJSONProtocol) ReadMessageBegin() (name string, typeId TMessageType, seqId int32, err error) {
	if isNull, err := p.ParseListBegin(); isNull || err != nil {
		return name, typeId, seqId, err
	}
	version, err := p.ReadI32()
	if err != nil {
		return name, typeId, seqId, err
	}
	if version != THRIFT_JSON_PROTOCOL_VERSION {
		e := fmt.Errorf("Unknown Protocol version %d, expected version %d", version, THRIFT_JSON_PROTOCOL_VERSION)
		return name, typeId, seqId, NewTProtocolExceptionWithType(INVALID_DATA, e)

	}
	if name, err = p.ReadString(); err != nil {
		return name, typeId, seqId, err
	}
	bTypeId, err := p.ReadByte()
	typeId = TMessageType(bTypeId)
	if err != nil {
		return name, typeId, seqId, err
	}
	if seqId, err = p.ReadI32(); err != nil {
		return name, typeId, seqId, err
	}
	return name, typeId, seqId, nil
}

func (p *TJSONProtocol) ReadMessageEnd() error {
	err := p.ParseListEnd()
	return err
}

func (p *TJSONProtocol) ReadStructBegin() (name string, err error) {
	_, err = p.ParseObjectStart()
	return "", err
}

func (p *TJSONProtocol) ReadStructEnd() error {
	return p.ParseObjectEnd()
}

func (p *TJSONProtocol) ReadFieldBegin() (string, TType, int16, error) {
	if p.reader.Buffered() < 1 {
		return "", STOP, -1, nil
	}
	b, _ := p.reader.Peek(1)
	if len(b) < 1 || b[0] == JSON_RBRACE[0] || b[0] == JSON_RBRACKET[0] {
		return "", STOP, -1, nil
	}
	fieldId, err := p.ReadI16()
	if err != nil {
		return "", STOP, fieldId, err
	}
	if _, err = p.ParseObjectStart(); err != nil {
		return "", STOP, fieldId, err
	}
	sType, err := p.ReadString()
	fType := p.StringToTypeId(sType)
	return "", fType, fieldId, err
}

func (p *TJSONProtocol) ReadFieldEnd() error {
	return p.ParseObjectEnd()
}

func (p *TJSONProtocol) ReadMapBegin() (keyType TType, valueType TType, size int, e error) {
	if isNull, e := p.ParseListBegin(); isNull || e != nil {
		return VOID, VOID, 0, e
	}

	// read keyType
	sKeyType, e := p.ReadString()
	keyType = p.StringToTypeId(sKeyType)
	if e != nil {
		return keyType, valueType, size, e
	}

	// read valueType
	sValueType, e := p.ReadString()
	valueType = p.StringToTypeId(sValueType)
	if e != nil {
		return keyType, valueType, size, e
	}

	// read size
	iSize, err := p.ReadI64()
	size = int(iSize)
	return keyType, valueType, size, err
}

func (p *TJSONProtocol) ReadMapEnd() error {
	return p.ParseListEnd()
}

func (p *TJSONProtocol) ReadListBegin() (elemType TType, size int, e error) {
	return p.ParseElemListBegin()
}

func (p *TJSONProtocol) ReadListEnd() error {
	return p.ParseListEnd()
}

func (p *TJSONProtocol) ReadSetBegin() (elemType TType, size int, e error) {
	return p.ParseElemListBegin()
}

func (p *TJSONProtocol) ReadSetEnd() error {
	return p.ParseListEnd()
}

func (p *TJSONProtocol) ReadBool() (bool, error) {
	var value bool
	if err := p.ParsePreValue(); err != nil {
		return value, err
	}
	b, _ := p.reader.Peek(len(JSON_FALSE))
	if len(b) > 0 {
		switch b[0] {
		case JSON_TRUE[0]:
			if string(b[0:len(JSON_TRUE)]) == string(JSON_TRUE) {
				p.reader.Read(b[0:len(JSON_TRUE)])
				value = true
			} else {
				e := fmt.Errorf("Expected \"true\" but found: %s", string(b))
				return value, NewTProtocolExceptionWithType(INVALID_DATA, e)
			}
			break
		case JSON_FALSE[0]:
			if string(b[0:len(JSON_FALSE)]) == string(JSON_FALSE) {
				p.reader.Read(b[0:len(JSON_FALSE)])
				value = false
			} else {
				e := fmt.Errorf("Expected \"false\" but found: %s", string(b))
				return value, NewTProtocolExceptionWithType(INVALID_DATA, e)
			}
			break
		case JSON_NULL[0]:
			if string(b[0:len(JSON_NULL)]) == string(JSON_NULL) {
				p.reader.Read(b[0:len(JSON_NULL)])
				value = false
			} else {
				e := fmt.Errorf("Expected \"null\" but found: %s", string(b))
				return value, NewTProtocolExceptionWithType(INVALID_DATA, e)
			}
		default:
			e := fmt.Errorf("Expected \"true\", \"false\", or \"null\" but found: %s", string(b))
			return value, NewTProtocolExceptionWithType(INVALID_DATA, e)
		}
	}
	return value, p.ParsePostValue()
}

func (p *TJSONProtocol) ReadByte() (byte, error) {
	v, err := p.ReadI64()
	return byte(v), err
}

func (p *TJSONProtocol) ReadI16() (int16, error) {
	v, err := p.ReadI64()
	return int16(v), err
}

func (p *TJSONProtocol) ReadI32() (int32, error) {
	v, err := p.ReadI64()
	return int32(v), err
}

func (p *TJSONProtocol) ReadI64() (int64, error) {
	v, _, err := p.ParseI64()
	return v, err
}

func (p *TJSONProtocol) ReadDouble() (float64, error) {
	v, _, err := p.ParseF64()
	return v, err
}

func (p *TJSONProtocol) ReadString() (string, error) {
	var v string
	if err := p.ParsePreValue(); err != nil {
		return v, err
	}
	b, _ := p.reader.Peek(len(JSON_NULL))
	if len(b) > 0 && b[0] == JSON_QUOTE {
		p.reader.ReadByte()
		value, err := p.ParseStringBody()
		v = value
		if err != nil {
			return v, err
		}
	} else if len(b) >= len(JSON_NULL) && string(b[0:len(JSON_NULL)]) == string(JSON_NULL) {
		_, err := p.reader.Read(b[0:len(JSON_NULL)])
		if err != nil {
			return v, NewTProtocolException(err)
		}
	} else {
		e := fmt.Errorf("Expected a JSON string, found %s", string(b))
		return v, NewTProtocolExceptionWithType(INVALID_DATA, e)
	}
	return v, p.ParsePostValue()
}

func (p *TJSONProtocol) ReadBinary() ([]byte, error) {
	var v []byte
	if err := p.ParsePreValue(); err != nil {
		return nil, err
	}
	b, _ := p.reader.Peek(len(JSON_NULL))
	if len(b) > 0 && b[0] == JSON_QUOTE {
		p.reader.ReadByte()
		value, err := p.ParseBase64EncodedBody()
		v = value
		if err != nil {
			return v, err
		}
	} else if len(b) >= len(JSON_NULL) && string(b[0:len(JSON_NULL)]) == string(JSON_NULL) {
		_, err := p.reader.Read(b[0:len(JSON_NULL)])
		if err != nil {
			return v, NewTProtocolException(err)
		}
	} else {
		e := fmt.Errorf("Expected a JSON string, found %s", string(b))
		return v, NewTProtocolExceptionWithType(INVALID_DATA, e)
	}
	return v, p.ParsePostValue()
}

func (p *TJSONProtocol) Flush() (err error) {
	return NewTProtocolException(p.writer.Flush())
}

func (p *TJSONProtocol) Skip(fieldType TType) (err error) {
	return SkipDefaultDepth(p, fieldType)
}

func (p *TJSONProtocol) Transport() TTransport {
	return p.trans
}

func (p *TJSONProtocol) OutputElemListBegin(elemType TType, size int) error {
	if e := p.OutputListBegin(); e != nil {
		return e
	}
	if e := p.WriteString(p.TypeIdToString(elemType)); e != nil {
		return e
	}
	if e := p.WriteI64(int64(size)); e != nil {
		return e
	}
	return nil
}


func (p *TJSONProtocol) ParseElemListBegin() (elemType TType, size int, e error) {
	if isNull, e := p.ParseListBegin(); isNull || e != nil {
		return VOID, 0, e
	}
	sElemType, err := p.ReadString()
	elemType = p.StringToTypeId(sElemType)
	if err != nil {
		return elemType, size, err
	}
	nSize, err2 := p.ReadI64()
	size = int(nSize)
	return elemType, size, err2
}

func (p *TJSONProtocol) readElemListBegin() (elemType TType, size int, e error) {
	if isNull, e := p.ParseListBegin(); isNull || e != nil {
		return VOID, 0, e
	}
	sElemType, err := p.ReadString()
	elemType = p.StringToTypeId(sElemType)
	if err != nil {
		return elemType, size, err
	}
	nSize, err2 := p.ReadI64()
	size = int(nSize)
	return elemType, size, err2
}

func (p *TJSONProtocol) writeElemListBegin(elemType TType, size int) error {
	if e := p.OutputListBegin(); e != nil {
		return e
	}
	if e := p.OutputString(p.TypeIdToString(elemType)); e != nil {
		return e
	}
	if e := p.OutputI64(int64(size)); e != nil {
		return e
	}
	return nil
}

func (p *TJSONProtocol) TypeIdToString(fieldType TType) string {
	switch byte(fieldType) {
	case STOP:
		return "stp"
	case VOID:
		return "v"
	case BOOL:
		return "tf"
	case BYTE:
		return "i8"
	case DOUBLE:
		return "dbl"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case STRING:
		return "str"
	case STRUCT:
		return "rec"
	case MAP:
		return "map"
	case SET:
		return "set"
	case LIST:
		return "lst"
	case UTF16:
		return "str"
	}
	return ""
}

func (p *TJSONProtocol) StringToTypeId(fieldType string) TType {
	switch fieldType {
	case "stp":
		return TType(STOP)
	case "v":
		return TType(VOID)
	case "tf":
		return TType(BOOL)
	case "i8":
		return TType(BYTE)
	case "dbl":
		return TType(DOUBLE)
	case "16":
		return TType(I16)
	case "i32":
		return TType(I32)
	case "i64":
		return TType(I64)
	case "str":
		return TType(STRING)
	case "rec":
		return TType(STRUCT)
	case "map":
		return TType(MAP)
	case "set":
		return TType(SET)
	case "lst":
		return TType(LIST)
	case "u16":
		return TType(UTF16)
	}
	return TType(STOP)
}
