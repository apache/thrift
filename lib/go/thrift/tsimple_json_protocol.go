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
  "bufio"
  "bytes"
  "container/vector"
  "encoding/base64"
  "fmt"
  "io"
  "json"
  "math"
  "os"
  "strconv"
  "strings"
)

type _ParseContext int

const (
  _CONTEXT_IN_TOPLEVEL          _ParseContext = 1
  _CONTEXT_IN_LIST_FIRST        _ParseContext = 2
  _CONTEXT_IN_LIST              _ParseContext = 3
  _CONTEXT_IN_OBJECT_FIRST      _ParseContext = 4
  _CONTEXT_IN_OBJECT_NEXT_KEY   _ParseContext = 5
  _CONTEXT_IN_OBJECT_NEXT_VALUE _ParseContext = 6
)

func (p _ParseContext) String() string {
  switch p {
  case _CONTEXT_IN_TOPLEVEL:
    return "TOPLEVEL"
  case _CONTEXT_IN_LIST_FIRST:
    return "LIST-FIRST"
  case _CONTEXT_IN_LIST:
    return "LIST"
  case _CONTEXT_IN_OBJECT_FIRST:
    return "OBJECT-FIRST"
  case _CONTEXT_IN_OBJECT_NEXT_KEY:
    return "OBJECT-NEXT-KEY"
  case _CONTEXT_IN_OBJECT_NEXT_VALUE:
    return "OBJECT-NEXT-VALUE"
  }
  return "UNKNOWN-PARSE-CONTEXT"
}

/**
 * JSON protocol implementation for thrift.
 *
 * This protocol produces/consumes a simple output format
 * suitable for parsing by scripting languages.  It should not be
 * confused with the full-featured TJSONProtocol.
 *
 */
type TSimpleJSONProtocol struct {
  //TProtocolBase;
  trans TTransport

  /**
   * Stack of nested contexts that we may be in.
   */
  parseContextStack vector.IntVector
  /**
   * Stack of nested contexts that we may be in.
   */
  dumpContext vector.IntVector

  /**
   * Current context that we are in
   */
  writer TTransport
  reader *bufio.Reader
}

/**
 * Constructor
 */
func NewTSimpleJSONProtocol(t TTransport) *TSimpleJSONProtocol {
  v := &TSimpleJSONProtocol{trans: t,
    writer: t,
    reader: bufio.NewReader(t),
  }
  v.parseContextStack.Push(int(_CONTEXT_IN_TOPLEVEL))
  v.dumpContext.Push(int(_CONTEXT_IN_TOPLEVEL))
  return v
}

/**
 * Factory
 */
type TSimpleJSONProtocolFactory struct{}

func (p *TSimpleJSONProtocolFactory) GetProtocol(trans TTransport) TProtocol {
  return NewTSimpleJSONProtocol(trans)
}

func NewTSimpleJSONProtocolFactory() *TSimpleJSONProtocolFactory {
  return &TSimpleJSONProtocolFactory{}
}

var (
  JSON_COMMA                   []byte
  JSON_COLON                   []byte
  JSON_LBRACE                  []byte
  JSON_RBRACE                  []byte
  JSON_LBRACKET                []byte
  JSON_RBRACKET                []byte
  JSON_QUOTE                   byte
  JSON_QUOTE_BYTES             []byte
  JSON_NULL                    []byte
  JSON_TRUE                    []byte
  JSON_FALSE                   []byte
  JSON_INFINITY                string
  JSON_NEGATIVE_INFINITY       string
  JSON_NAN                     string
  JSON_INFINITY_BYTES          []byte
  JSON_NEGATIVE_INFINITY_BYTES []byte
  JSON_NAN_BYTES               []byte
  json_nonbase_map_elem_bytes  []byte
)

func init() {
  JSON_COMMA = []byte{','}
  JSON_COLON = []byte{':'}
  JSON_LBRACE = []byte{'{'}
  JSON_RBRACE = []byte{'}'}
  JSON_LBRACKET = []byte{'['}
  JSON_RBRACKET = []byte{']'}
  JSON_QUOTE = '"'
  JSON_QUOTE_BYTES = []byte{'"'}
  JSON_NULL = []byte{'n', 'u', 'l', 'l'}
  JSON_TRUE = []byte{'t', 'r', 'u', 'e'}
  JSON_FALSE = []byte{'f', 'a', 'l', 's', 'e'}
  JSON_INFINITY = "Infinity"
  JSON_NEGATIVE_INFINITY = "-Infinity"
  JSON_NAN = "NaN"
  JSON_INFINITY_BYTES = []byte{'I', 'n', 'f', 'i', 'n', 'i', 't', 'y'}
  JSON_NEGATIVE_INFINITY_BYTES = []byte{'-', 'I', 'n', 'f', 'i', 'n', 'i', 't', 'y'}
  JSON_NAN_BYTES = []byte{'N', 'a', 'N'}
  json_nonbase_map_elem_bytes = []byte{']', ',', '['}
}

func JsonQuote(s string) string {
  b, _ := json.Marshal(s)
  s1 := string(b)
  return s1
}

func JsonUnquote(s string) (string, bool) {
  s1 := new(string)
  err := json.Unmarshal([]byte(s), s1)
  return *s1, err == nil
}


func (p *TSimpleJSONProtocol) WriteMessageBegin(name string, typeId TMessageType, seqId int32) TProtocolException {
  if e := p.OutputListBegin(); e != nil {
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

func (p *TSimpleJSONProtocol) WriteMessageEnd() TProtocolException {
  return p.OutputListEnd()
}

func (p *TSimpleJSONProtocol) WriteStructBegin(name string) TProtocolException {
  if e := p.OutputObjectBegin(); e != nil {
    return e
  }
  return nil
}

func (p *TSimpleJSONProtocol) WriteStructEnd() TProtocolException {
  return p.OutputObjectEnd()
}

func (p *TSimpleJSONProtocol) WriteFieldBegin(name string, typeId TType, id int16) TProtocolException {
  if e := p.WriteString(name); e != nil {
    return e
  }
  return nil
  /*
    	if e := p.OutputListBegin(); e != nil {
        return e
      }
      if e := p.WriteByte(byte(typeId)); e != nil {
        return e
      }
      return p.WriteI16(id)
  */
}

func (p *TSimpleJSONProtocol) WriteFieldEnd() TProtocolException {
  //return p.OutputListEnd()
  return nil
}

func (p *TSimpleJSONProtocol) WriteFieldStop() TProtocolException { return nil }

func (p *TSimpleJSONProtocol) WriteMapBegin(keyType TType, valueType TType, size int) TProtocolException {
  if e := p.OutputListBegin(); e != nil {
    return e
  }
  if e := p.WriteByte(byte(keyType)); e != nil {
    return e
  }
  if e := p.WriteByte(byte(valueType)); e != nil {
    return e
  }
  return p.WriteI32(int32(size))
}

func (p *TSimpleJSONProtocol) WriteMapEnd() TProtocolException {
  return p.OutputListEnd()
}

func (p *TSimpleJSONProtocol) WriteListBegin(elemType TType, size int) TProtocolException {
  return p.OutputElemListBegin(elemType, size)
}

func (p *TSimpleJSONProtocol) WriteListEnd() TProtocolException {
  return p.OutputListEnd()
}

func (p *TSimpleJSONProtocol) WriteSetBegin(elemType TType, size int) TProtocolException {
  return p.OutputElemListBegin(elemType, size)
}

func (p *TSimpleJSONProtocol) WriteSetEnd() TProtocolException {
  return p.OutputListEnd()
}

func (p *TSimpleJSONProtocol) WriteBool(b bool) TProtocolException {
  return p.OutputBool(b)
}

func (p *TSimpleJSONProtocol) WriteByte(b byte) TProtocolException {
  return p.WriteI32(int32(b))
}

func (p *TSimpleJSONProtocol) WriteI16(v int16) TProtocolException {
  return p.WriteI32(int32(v))
}

func (p *TSimpleJSONProtocol) WriteI32(v int32) TProtocolException {
  return p.OutputI64(int64(v))
}

func (p *TSimpleJSONProtocol) WriteI64(v int64) TProtocolException {
  return p.OutputI64(int64(v))
}

func (p *TSimpleJSONProtocol) WriteDouble(v float64) TProtocolException {
  return p.OutputF64(v)
}

func (p *TSimpleJSONProtocol) WriteString(v string) TProtocolException {
  return p.OutputString(v)
}

func (p *TSimpleJSONProtocol) WriteBinary(v []byte) TProtocolException {
  // JSON library only takes in a string, 
  // not an arbitrary byte array, to ensure bytes are transmitted
  // efficiently we must convert this into a valid JSON string
  // therefore we use base64 encoding to avoid excessive escaping/quoting
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  if _, e := p.writer.Write(JSON_QUOTE_BYTES); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  writer := base64.NewEncoder(base64.StdEncoding, p.writer)
  if _, e := writer.Write(v); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  if e := writer.Close(); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  if _, e := p.writer.Write(JSON_QUOTE_BYTES); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  return p.OutputPostValue()
}

/**
 * Reading methods.
 */

func (p *TSimpleJSONProtocol) ReadMessageBegin() (name string, typeId TMessageType, seqId int32, err TProtocolException) {
  if isNull, err := p.ParseListBegin(); isNull || err != nil {
    return name, typeId, seqId, err
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

func (p *TSimpleJSONProtocol) ReadMessageEnd() TProtocolException {
  return p.ParseListEnd()
}

func (p *TSimpleJSONProtocol) ReadStructBegin() (name string, err TProtocolException) {
  _, err = p.ParseObjectStart()
  return "", err
}

func (p *TSimpleJSONProtocol) ReadStructEnd() TProtocolException {
  return p.ParseObjectEnd()
}

func (p *TSimpleJSONProtocol) ReadFieldBegin() (string, TType, int16, TProtocolException) {
  if err := p.ParsePreValue(); err != nil {
    return "", STOP, 0, err
  }
  if p.reader.Buffered() < 1 {
    return "", STOP, 0, nil
  }
  b, _ := p.reader.Peek(1)
  if len(b) > 0 {
    switch b[0] {
    case JSON_RBRACE[0]:
      return "", STOP, 0, nil
    case JSON_QUOTE:
      p.reader.ReadByte()
      name, err := p.ParseStringBody()
      if err != nil {
        return name, STOP, 0, err
      }
      return name, GENERIC, -1, p.ParsePostValue()
      /*
         if err = p.ParsePostValue(); err != nil {
           return name, STOP, 0, err
         }
         if isNull, err := p.ParseListBegin(); isNull || err != nil {
           return name, STOP, 0, err
         }
         bType, err := p.ReadByte()
         thetype := TType(bType)
         if err != nil {
           return name, thetype, 0, err
         }
         id, err := p.ReadI16()
         return name, thetype, id, err
      */
    }
    return "", STOP, 0, NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected \"}\" or '\"', but found: '", string(b), "'"))
  }
  return "", STOP, 0, NewTProtocolExceptionFromOsError(os.EOF)
}

func (p *TSimpleJSONProtocol) ReadFieldEnd() TProtocolException {
  return nil
  //return p.ParseListEnd()
}

func (p *TSimpleJSONProtocol) ReadMapBegin() (keyType TType, valueType TType, size int, e TProtocolException) {
  if isNull, e := p.ParseListBegin(); isNull || e != nil {
    return VOID, VOID, 0, e
  }

  // read keyType
  bKeyType, e := p.ReadByte()
  keyType = TType(bKeyType)
  if e != nil {
    return keyType, valueType, size, e
  }

  // read valueType
  bValueType, e := p.ReadByte()
  valueType = TType(bValueType)
  if e != nil {
    return keyType, valueType, size, e
  }

  // read size
  iSize, err := p.ReadI64()
  size = int(iSize)
  return keyType, valueType, size, err
}

func (p *TSimpleJSONProtocol) ReadMapEnd() TProtocolException {
  return p.ParseListEnd()
}

func (p *TSimpleJSONProtocol) ReadListBegin() (elemType TType, size int, e TProtocolException) {
  return p.ParseElemListBegin()
}

func (p *TSimpleJSONProtocol) ReadListEnd() TProtocolException {
  return p.ParseListEnd()
}

func (p *TSimpleJSONProtocol) ReadSetBegin() (elemType TType, size int, e TProtocolException) {
  return p.ParseElemListBegin()
}

func (p *TSimpleJSONProtocol) ReadSetEnd() TProtocolException {
  return p.ParseListEnd()
}

func (p *TSimpleJSONProtocol) ReadBool() (bool, TProtocolException) {
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
        return value, NewTProtocolException(INVALID_DATA, "Expected \"true\" but found: "+string(b))
      }
      break
    case JSON_FALSE[0]:
      if string(b[0:len(JSON_FALSE)]) == string(JSON_FALSE) {
        p.reader.Read(b[0:len(JSON_FALSE)])
        value = false
      } else {
        return value, NewTProtocolException(INVALID_DATA, "Expected \"false\" but found: "+string(b))
      }
      break
    case JSON_NULL[0]:
      if string(b[0:len(JSON_NULL)]) == string(JSON_NULL) {
        p.reader.Read(b[0:len(JSON_NULL)])
        value = false
      } else {
        return value, NewTProtocolException(INVALID_DATA, "Expected \"null\" but found: "+string(b))
      }
    default:
      return value, NewTProtocolException(INVALID_DATA, "Expected \"true\", \"false\", or \"null\" but found: "+string(b))
    }
  }
  return value, p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) ReadByte() (byte, TProtocolException) {
  v, err := p.ReadI64()
  return byte(v), err
}

func (p *TSimpleJSONProtocol) ReadI16() (int16, TProtocolException) {
  v, err := p.ReadI64()
  return int16(v), err
}

func (p *TSimpleJSONProtocol) ReadI32() (int32, TProtocolException) {
  v, err := p.ReadI64()
  return int32(v), err
}

func (p *TSimpleJSONProtocol) ReadI64() (int64, TProtocolException) {
  v, _, err := p.ParseI64()
  return v, err
}

func (p *TSimpleJSONProtocol) ReadDouble() (float64, TProtocolException) {
  v, _, err := p.ParseF64()
  return v, err
}

func (p *TSimpleJSONProtocol) ReadString() (string, TProtocolException) {
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
      return v, NewTProtocolExceptionFromOsError(err)
    }
  } else {
    return v, NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected a JSON string, found ", string(b)))
  }
  return v, p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) ReadBinary() ([]byte, TProtocolException) {
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
      return v, NewTProtocolExceptionFromOsError(err)
    }
  } else {
    return v, NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected a JSON string, found ", string(b)))
  }
  return v, p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) Flush() (err TProtocolException) {
  return NewTProtocolExceptionFromOsError(p.writer.Flush())
}

func (p *TSimpleJSONProtocol) Skip(fieldType TType) (err TProtocolException) {
  return SkipDefaultDepth(p, fieldType)
}

func (p *TSimpleJSONProtocol) Transport() TTransport {
  return p.trans
}


func (p *TSimpleJSONProtocol) OutputPreValue() TProtocolException {
  cxt := _ParseContext(p.dumpContext.Last())
  switch cxt {
  case _CONTEXT_IN_LIST, _CONTEXT_IN_OBJECT_NEXT_KEY:
    if _, e := p.writer.Write(JSON_COMMA); e != nil {
      return NewTProtocolExceptionFromOsError(e)
    }
    break
  case _CONTEXT_IN_OBJECT_NEXT_VALUE:
    if _, e := p.writer.Write(JSON_COLON); e != nil {
      return NewTProtocolExceptionFromOsError(e)
    }
    break
  }
  return nil
}

func (p *TSimpleJSONProtocol) OutputPostValue() TProtocolException {
  cxt := _ParseContext(p.dumpContext.Last())
  switch cxt {
  case _CONTEXT_IN_LIST_FIRST:
    p.dumpContext.Pop()
    p.dumpContext.Push(int(_CONTEXT_IN_LIST))
    break
  case _CONTEXT_IN_OBJECT_FIRST:
    p.dumpContext.Pop()
    p.dumpContext.Push(int(_CONTEXT_IN_OBJECT_NEXT_VALUE))
    break
  case _CONTEXT_IN_OBJECT_NEXT_KEY:
    p.dumpContext.Pop()
    p.dumpContext.Push(int(_CONTEXT_IN_OBJECT_NEXT_VALUE))
    break
  case _CONTEXT_IN_OBJECT_NEXT_VALUE:
    p.dumpContext.Pop()
    p.dumpContext.Push(int(_CONTEXT_IN_OBJECT_NEXT_KEY))
    break
  }
  return nil
}

func (p *TSimpleJSONProtocol) OutputBool(value bool) TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  var v string
  if value {
    v = string(JSON_TRUE)
  } else {
    v = string(JSON_FALSE)
  }
  switch _ParseContext(p.dumpContext.Last()) {
  case _CONTEXT_IN_OBJECT_FIRST, _CONTEXT_IN_OBJECT_NEXT_KEY:
    v = JsonQuote(v)
  default:
  }
  if e := p.OutputStringData(v); e != nil {
    return e
  }
  return p.OutputPostValue()
}

func (p *TSimpleJSONProtocol) OutputNull() TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  if _, e := p.writer.Write(JSON_NULL); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  return p.OutputPostValue()
}

func (p *TSimpleJSONProtocol) OutputF64(value float64) TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  var v string
  if math.IsNaN(value) {
    v = string(JSON_QUOTE) + JSON_NAN + string(JSON_QUOTE)
  } else if math.IsInf(value, 1) {
    v = string(JSON_QUOTE) + JSON_INFINITY + string(JSON_QUOTE)
  } else if math.IsInf(value, -1) {
    v = string(JSON_QUOTE) + JSON_NEGATIVE_INFINITY + string(JSON_QUOTE)
  } else {
    v = strconv.Ftoa64(value, 'g', -1)
    switch _ParseContext(p.dumpContext.Last()) {
    case _CONTEXT_IN_OBJECT_FIRST, _CONTEXT_IN_OBJECT_NEXT_KEY:
      v = string(JSON_QUOTE) + v + string(JSON_QUOTE)
    default:
    }
  }
  if e := p.OutputStringData(v); e != nil {
    return e
  }
  return p.OutputPostValue()
}

func (p *TSimpleJSONProtocol) OutputI64(value int64) TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  v := strconv.Itoa64(value)
  switch _ParseContext(p.dumpContext.Last()) {
  case _CONTEXT_IN_OBJECT_FIRST, _CONTEXT_IN_OBJECT_NEXT_KEY:
    v = JsonQuote(v)
  default:
  }
  if e := p.OutputStringData(v); e != nil {
    return e
  }
  return p.OutputPostValue()
}

func (p *TSimpleJSONProtocol) OutputString(s string) TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  if e := p.OutputStringData(JsonQuote(s)); e != nil {
    return e
  }
  return p.OutputPostValue()
}

func (p *TSimpleJSONProtocol) OutputStringData(s string) TProtocolException {
  _, e := io.Copyn(p.writer, strings.NewReader(s), int64(len(s)))
  return NewTProtocolExceptionFromOsError(e)
}

func (p *TSimpleJSONProtocol) OutputObjectBegin() TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  if _, e := p.writer.Write(JSON_LBRACE); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  p.dumpContext.Push(int(_CONTEXT_IN_OBJECT_FIRST))
  return nil
}

func (p *TSimpleJSONProtocol) OutputObjectEnd() TProtocolException {
  if _, e := p.writer.Write(JSON_RBRACE); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  p.dumpContext.Pop()
  if e := p.OutputPostValue(); e != nil {
    return e
  }
  return nil
}

func (p *TSimpleJSONProtocol) OutputListBegin() TProtocolException {
  if e := p.OutputPreValue(); e != nil {
    return e
  }
  if _, e := p.writer.Write(JSON_LBRACKET); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  p.dumpContext.Push(int(_CONTEXT_IN_LIST_FIRST))
  return nil
}

func (p *TSimpleJSONProtocol) OutputListEnd() TProtocolException {
  if _, e := p.writer.Write(JSON_RBRACKET); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  p.dumpContext.Pop()
  if e := p.OutputPostValue(); e != nil {
    return e
  }
  return nil
}

func (p *TSimpleJSONProtocol) OutputElemListBegin(elemType TType, size int) TProtocolException {
  if e := p.OutputListBegin(); e != nil {
    return e
  }
  if e := p.WriteByte(byte(elemType)); e != nil {
    return e
  }
  if e := p.WriteI64(int64(size)); e != nil {
    return e
  }
  return nil
}

func (p *TSimpleJSONProtocol) ParsePreValue() TProtocolException {
  if e := p.readNonSignificantWhitespace(); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  cxt := _ParseContext(p.parseContextStack.Last())
  if p.reader.Buffered() < 1 {
    return nil
  }
  b, _ := p.reader.Peek(1)
  switch cxt {
  case _CONTEXT_IN_LIST:
    if len(b) > 0 {
      switch b[0] {
      case JSON_RBRACKET[0]:
        return nil
      case JSON_COMMA[0]:
        p.reader.ReadByte()
        if e := p.readNonSignificantWhitespace(); e != nil {
          return NewTProtocolExceptionFromOsError(e)
        }
        return nil
      default:
        return NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected \"]\" or \",\" in list context, but found \"", string(b), "\""))
      }
    }
    break
  case _CONTEXT_IN_OBJECT_NEXT_KEY:
    if len(b) > 0 {
      switch b[0] {
      case JSON_RBRACE[0]:
        return nil
      case JSON_COMMA[0]:
        p.reader.ReadByte()
        if e := p.readNonSignificantWhitespace(); e != nil {
          return NewTProtocolExceptionFromOsError(e)
        }
        return nil
      default:
        return NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected \"}\" or \",\" in object context, but found \"", string(b), "\""))
      }
    }
    break
  case _CONTEXT_IN_OBJECT_NEXT_VALUE:
    if len(b) > 0 {
      switch b[0] {
      case JSON_COLON[0]:
        p.reader.ReadByte()
        if e := p.readNonSignificantWhitespace(); e != nil {
          return NewTProtocolExceptionFromOsError(e)
        }
        return nil
      default:
        return NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected \":\" in object context, but found \"", string(b), "\""))
      }
    }
    break
  }
  return nil
}

func (p *TSimpleJSONProtocol) ParsePostValue() TProtocolException {
  if e := p.readNonSignificantWhitespace(); e != nil {
    return NewTProtocolExceptionFromOsError(e)
  }
  cxt := _ParseContext(p.parseContextStack.Last())
  switch cxt {
  case _CONTEXT_IN_LIST_FIRST:
    p.parseContextStack.Pop()
    p.parseContextStack.Push(int(_CONTEXT_IN_LIST))
    break
  case _CONTEXT_IN_OBJECT_FIRST, _CONTEXT_IN_OBJECT_NEXT_KEY:
    p.parseContextStack.Pop()
    p.parseContextStack.Push(int(_CONTEXT_IN_OBJECT_NEXT_VALUE))
    break
  case _CONTEXT_IN_OBJECT_NEXT_VALUE:
    p.parseContextStack.Pop()
    p.parseContextStack.Push(int(_CONTEXT_IN_OBJECT_NEXT_KEY))
    break
  }
  return nil
}

func (p *TSimpleJSONProtocol) readNonSignificantWhitespace() os.Error {
  for p.reader.Buffered() > 0 {
    b, _ := p.reader.Peek(1)
    if len(b) < 1 {
      return nil
    }
    switch b[0] {
    case ' ', '\r', '\n', '\t':
      p.reader.ReadByte()
      continue
    default:
      break
    }
    break
  }
  return nil
}

func (p *TSimpleJSONProtocol) ParseStringBody() (string, TProtocolException) {
  line, err := p.reader.ReadString(JSON_QUOTE)
  if err != nil {
    return "", NewTProtocolExceptionFromOsError(err)
  }
  l := len(line)
  // count number of escapes to see if we need to keep going
  i := 1
  for ; i < l; i++ {
    if line[l-i-1] != '\\' {
      break
    }
  }
  if i&0x01 == 1 {
    v, ok := JsonUnquote(string(JSON_QUOTE) + line)
    if !ok {
      return "", NewTProtocolExceptionFromOsError(err)
    }
    return v, nil
  }
  s, err := p.ParseQuotedStringBody()
  if err != nil {
    return "", NewTProtocolExceptionFromOsError(err)
  }
  str := string(JSON_QUOTE) + line + s
  v, ok := JsonUnquote(str)
  if !ok {
    return "", NewTProtocolException(INVALID_DATA, "Unable to parse as JSON string "+str)
  }
  return v, nil
}

func (p *TSimpleJSONProtocol) ParseQuotedStringBody() (string, TProtocolException) {
  line, err := p.reader.ReadString(JSON_QUOTE)
  if err != nil {
    return "", NewTProtocolExceptionFromOsError(err)
  }
  l := len(line)
  // count number of escapes to see if we need to keep going
  i := 1
  for ; i < l; i++ {
    if line[l-i-1] != '\\' {
      break
    }
  }
  if i&0x01 == 1 {
    return line, nil
  }
  s, err := p.ParseQuotedStringBody()
  if err != nil {
    return "", NewTProtocolExceptionFromOsError(err)
  }
  v := line + s
  return v, nil
}

func (p *TSimpleJSONProtocol) ParseBase64EncodedBody() ([]byte, TProtocolException) {
  line, err := p.reader.ReadBytes(JSON_QUOTE)
  if err != nil {
    return line, NewTProtocolExceptionFromOsError(err)
  }
  line2 := line[0 : len(line)-1]
  l := len(line2)
  output := make([]byte, base64.StdEncoding.DecodedLen(l))
  n, err := base64.StdEncoding.Decode(output, line2)
  return output[0:n], NewTProtocolExceptionFromOsError(err)
}

func (p *TSimpleJSONProtocol) ParseI64() (int64, bool, TProtocolException) {
  if err := p.ParsePreValue(); err != nil {
    return 0, false, err
  }
  var value int64
  var isnull bool
  b, _ := p.reader.Peek(len(JSON_NULL))
  if len(b) >= len(JSON_NULL) && string(b) == string(JSON_NULL) {
    p.reader.Read(b[0:len(JSON_NULL)])
    isnull = true
  } else {
    num, err := p.readNumeric()
    isnull = (num == nil)
    if !isnull {
      value = num.Int64()
    }
    if err != nil {
      return value, isnull, err
    }
  }
  return value, isnull, p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) ParseF64() (float64, bool, TProtocolException) {
  if err := p.ParsePreValue(); err != nil {
    return 0, false, err
  }
  var value float64
  var isnull bool
  b, _ := p.reader.Peek(len(JSON_NULL))
  if len(b) >= len(JSON_NULL) && string(b) == string(JSON_NULL) {
    p.reader.Read(b[0:len(JSON_NULL)])
    isnull = true
  } else {
    num, err := p.readNumeric()
    isnull = (num == nil)
    if !isnull {
      value = num.Float64()
    }
    if err != nil {
      return value, isnull, err
    }
  }
  return value, isnull, p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) ParseObjectStart() (bool, TProtocolException) {
  if err := p.ParsePreValue(); err != nil {
    return false, err
  }
  b, _ := p.reader.Peek(len(JSON_NULL))
  if len(b) > 0 && b[0] == JSON_LBRACE[0] {
    p.reader.ReadByte()
    p.parseContextStack.Push(int(_CONTEXT_IN_OBJECT_FIRST))
    return false, nil
  } else if len(b) >= len(JSON_NULL) && string(b[0:len(JSON_NULL)]) == string(JSON_NULL) {
    return true, nil
  }
  return false, NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected '{' or null, but found '", string(b), "'"))
}

func (p *TSimpleJSONProtocol) ParseObjectEnd() TProtocolException {
  if isNull, err := p.readIfNull(); isNull || err != nil {
    return err
  }
  cxt := _ParseContext(p.parseContextStack.Last())
  if cxt != _CONTEXT_IN_OBJECT_FIRST && cxt != _CONTEXT_IN_OBJECT_NEXT_KEY {
    return NewTProtocolException(INVALID_DATA, fmt.Sprint("Expected to be in the Object Context, but not in Object Context"))
  }
  line, err := p.reader.ReadString(JSON_RBRACE[0])
  if err != nil {
    return NewTProtocolExceptionFromOsError(err)
  }
  for _, char := range line {
    switch char {
    default:
      return NewTProtocolException(INVALID_DATA, fmt.Sprint("Expecting end of object \"}\", but found: \"", line, "\""))
    case ' ', '\n', '\r', '\t', '}':
      break
    }
  }
  p.parseContextStack.Pop()
  return p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) ParseListBegin() (bool, TProtocolException) {
  if e := p.ParsePreValue(); e != nil {
    return false, e
  }
  b, e := p.reader.Peek(len(JSON_NULL))
  if e == nil && len(b) >= 1 && b[0] == JSON_LBRACKET[0] {
    p.parseContextStack.Push(int(_CONTEXT_IN_LIST_FIRST))
    p.reader.ReadByte()
    return false, nil
  } else if e == nil && len(b) >= len(JSON_NULL) && string(b) == string(JSON_NULL) {
    return true, nil
  }
  return false, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Expected 'null' or '{', received '%q'", b))
}

func (p *TSimpleJSONProtocol) ParseElemListBegin() (elemType TType, size int, e TProtocolException) {
  if isNull, e := p.ParseListBegin(); isNull || e != nil {
    return VOID, 0, e
  }
  bElemType, err := p.ReadByte()
  elemType = TType(bElemType)
  if err != nil {
    return elemType, size, err
  }
  nSize, err2 := p.ReadI64()
  size = int(nSize)
  return elemType, size, err2
}

func (p *TSimpleJSONProtocol) ParseListEnd() TProtocolException {
  if isNull, err := p.readIfNull(); isNull || err != nil {
    return err
  }
  if _ParseContext(p.parseContextStack.Last()) != _CONTEXT_IN_LIST {
    return NewTProtocolException(INVALID_DATA, "Expected to be in the List Context, but not in List Context")
  }
  line, err := p.reader.ReadString(JSON_RBRACKET[0])
  if err != nil {
    return NewTProtocolExceptionFromOsError(err)
  }
  for _, char := range line {
    switch char {
    default:
      return NewTProtocolException(INVALID_DATA, fmt.Sprint("Expecting end of list \"]\", but found: \"", line, "\""))
    case ' ', '\n', '\r', '\t', int(JSON_RBRACKET[0]):
      break
    }
  }
  p.parseContextStack.Pop()
  return p.ParsePostValue()
}

func (p *TSimpleJSONProtocol) readSingleValue() (interface{}, TType, TProtocolException) {
  e := p.readNonSignificantWhitespace()
  if e != nil {
    return nil, VOID, NewTProtocolExceptionFromOsError(e)
  }
  b, e := p.reader.Peek(10)
  if len(b) > 0 {
    c := b[0]
    switch c {
    case JSON_NULL[0]:
      buf := make([]byte, len(JSON_NULL))
      _, e := p.reader.Read(buf)
      if e != nil {
        return nil, VOID, NewTProtocolExceptionFromOsError(e)
      }
      if string(JSON_NULL) != string(buf) {
        e := NewTProtocolException(INVALID_DATA, "Expected '"+string(JSON_NULL)+"' but found '"+string(buf)+"' while parsing JSON.")
        return nil, VOID, e
      }
      return nil, VOID, nil
    case JSON_QUOTE:
      p.reader.ReadByte()
      v, e := p.ParseStringBody()
      if e != nil {
        return v, UTF8, NewTProtocolExceptionFromOsError(e)
      }
      if v == JSON_INFINITY {
        return INFINITY, DOUBLE, nil
      } else if v == JSON_NEGATIVE_INFINITY {
        return NEGATIVE_INFINITY, DOUBLE, nil
      } else if v == JSON_NAN {
        return NAN, DOUBLE, nil
      }
      return v, UTF8, nil
    case JSON_TRUE[0]:
      buf := make([]byte, len(JSON_TRUE))
      _, e := p.reader.Read(buf)
      if e != nil {
        return true, BOOL, NewTProtocolExceptionFromOsError(e)
      }
      if string(JSON_TRUE) != string(buf) {
        e := NewTProtocolException(INVALID_DATA, "Expected '"+string(JSON_TRUE)+"' but found '"+string(buf)+"' while parsing JSON.")
        return true, BOOL, NewTProtocolExceptionFromOsError(e)
      }
      return true, BOOL, nil
    case JSON_FALSE[0]:
      buf := make([]byte, len(JSON_FALSE))
      _, e := p.reader.Read(buf)
      if e != nil {
        return false, BOOL, NewTProtocolExceptionFromOsError(e)
      }
      if string(JSON_FALSE) != string(buf) {
        e := NewTProtocolException(INVALID_DATA, "Expected '"+string(JSON_FALSE)+"' but found '"+string(buf)+"' while parsing JSON.")
        return false, BOOL, NewTProtocolExceptionFromOsError(e)
      }
      return false, BOOL, nil
    case JSON_LBRACKET[0]:
      _, e := p.reader.ReadByte()
      return make([]interface{}, 0), LIST, NewTProtocolExceptionFromOsError(e)
    case JSON_LBRACE[0]:
      _, e := p.reader.ReadByte()
      return make(map[string]interface{}), STRUCT, NewTProtocolExceptionFromOsError(e)
    case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'e', 'E', '.', '+', '-', JSON_INFINITY[0], JSON_NAN[0]:
      // assume numeric
      v, e := p.readNumeric()
      return v, DOUBLE, e
    default:
      return nil, VOID, NewTProtocolException(INVALID_DATA, "Expected element in list but found '"+string(c)+"' while parsing JSON.")
    }
  }
  return nil, VOID, NewTProtocolException(INVALID_DATA, "Cannot read a single element while parsing JSON.")

}


func (p *TSimpleJSONProtocol) readIfNull() (bool, TProtocolException) {
  cont := true
  for p.reader.Buffered() > 0 && cont {
    b, _ := p.reader.Peek(1)
    if len(b) < 1 {
      return false, nil
    }
    switch b[0] {
    default:
      return false, nil
    case JSON_NULL[0]:
      cont = false
      break
    case ' ', '\n', '\r', '\t':
      p.reader.ReadByte()
      break
    }
  }
  if p.reader.Buffered() == 0 {
    return false, nil
  }
  b, _ := p.reader.Peek(len(JSON_NULL))
  if string(b) == string(JSON_NULL) {
    p.reader.Read(b[0:len(JSON_NULL)])
    return true, nil
  }
  return false, nil
}

func (p *TSimpleJSONProtocol) readQuoteIfNext() {
  if p.reader.Buffered() < 1 {
    return
  }
  b, _ := p.reader.Peek(1)
  if len(b) > 0 && b[0] == JSON_QUOTE {
    p.reader.ReadByte()
  }
}

func (p *TSimpleJSONProtocol) readNumeric() (Numeric, TProtocolException) {
  isNull, err := p.readIfNull()
  if isNull || err != nil {
    return NUMERIC_NULL, err
  }
  hasDecimalPoint := false
  nextCanBeSign := true
  hasE := false
  MAX_LEN := 40
  buf := bytes.NewBuffer(make([]byte, 0, MAX_LEN))
  continueFor := true
  inQuotes := false
  for continueFor {
    c, err := p.reader.ReadByte()
    if err != nil {
      if err == os.EOF {
        break
      }
      return NUMERIC_NULL, NewTProtocolExceptionFromOsError(err)
    }
    switch c {
    case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
      buf.WriteByte(c)
      nextCanBeSign = false
    case '.':
      if hasDecimalPoint {
        return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Unable to parse number with multiple decimal points '%s.'", buf.String()))
      }
      if hasE {
        return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Unable to parse number with decimal points in the exponent '%s.'", buf.String()))
      }
      buf.WriteByte(c)
      hasDecimalPoint, nextCanBeSign = true, false
    case 'e', 'E':
      if hasE {
        return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Unable to parse number with multiple exponents '%s%c'", buf.String(), c))
      }
      buf.WriteByte(c)
      hasE, nextCanBeSign = true, true
    case '-', '+':
      if !nextCanBeSign {
        return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprint("Negative sign within number"))
      }
      buf.WriteByte(c)
      nextCanBeSign = false
    case ' ', 0, '\t', '\n', '\r', JSON_RBRACE[0], JSON_RBRACKET[0], JSON_COMMA[0], JSON_COLON[0]:
      p.reader.UnreadByte()
      continueFor = false
    case JSON_NAN[0]:
      if buf.Len() == 0 {
        buffer := make([]byte, len(JSON_NAN))
        buffer[0] = c
        _, e := p.reader.Read(buffer[1:])
        if e != nil {
          return NUMERIC_NULL, NewTProtocolExceptionFromOsError(e)
        }
        if JSON_NAN != string(buffer) {
          e := NewTProtocolException(INVALID_DATA, "Expected '"+JSON_NAN+"' but found '"+string(buffer)+"' while parsing JSON.")
          return NUMERIC_NULL, e
        }
        if inQuotes {
          p.readQuoteIfNext()
        }
        return NAN, nil
      } else {
        return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Unable to parse number starting with character '%c'", c))
      }
    case JSON_INFINITY[0]:
      if buf.Len() == 0 || (buf.Len() == 1 && buf.Bytes()[0] == '+') {
        buffer := make([]byte, len(JSON_INFINITY))
        buffer[0] = c
        _, e := p.reader.Read(buffer[1:])
        if e != nil {
          return NUMERIC_NULL, NewTProtocolExceptionFromOsError(e)
        }
        if JSON_INFINITY != string(buffer) {
          e := NewTProtocolException(INVALID_DATA, "Expected '"+JSON_INFINITY+"' but found '"+string(buffer)+"' while parsing JSON.")
          return NUMERIC_NULL, e
        }
        if inQuotes {
          p.readQuoteIfNext()
        }
        return INFINITY, nil
      } else if buf.Len() == 1 && buf.Bytes()[0] == JSON_NEGATIVE_INFINITY[0] {
        buffer := make([]byte, len(JSON_NEGATIVE_INFINITY))
        buffer[0] = JSON_NEGATIVE_INFINITY[0]
        buffer[1] = c
        _, e := p.reader.Read(buffer[2:])
        if e != nil {
          return NUMERIC_NULL, NewTProtocolExceptionFromOsError(e)
        }
        if JSON_NEGATIVE_INFINITY != string(buffer) {
          e := NewTProtocolException(INVALID_DATA, "Expected '"+JSON_NEGATIVE_INFINITY+"' but found '"+string(buffer)+"' while parsing JSON.")
          return NUMERIC_NULL, e
        }
        if inQuotes {
          p.readQuoteIfNext()
        }
        return NEGATIVE_INFINITY, nil
      } else {
        return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Unable to parse number starting with character '%c' due to existing buffer %s", c, buf.String()))
      }
    case JSON_QUOTE:
      if !inQuotes {
        inQuotes = true
      } else {
        break
      }
    default:
      return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprintf("Unable to parse number starting with character '%c'", c))
    }
  }
  if buf.Len() == 0 {
    return NUMERIC_NULL, NewTProtocolException(INVALID_DATA, fmt.Sprint("Unable to parse number from empty string ''"))
  }
  return NewNumericFromJSONString(buf.String(), false), nil
}
