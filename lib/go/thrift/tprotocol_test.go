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

package thrift_test

import (
  . "thrift"
  "testing"
  "http"
  "math"
  "net"
  "io"
  "os"
  "bytes"
  "fmt"
)

const PROTOCOL_BINARY_DATA_SIZE = 155

var (
  data           string // test data for writing
  protocol_bdata []byte // test data for writing; same as data
  BOOL_VALUES    []bool
  BYTE_VALUES    []byte
  INT16_VALUES   []int16
  INT32_VALUES   []int32
  INT64_VALUES   []int64
  DOUBLE_VALUES  []float64
  STRING_VALUES  []string
)


func init() {
  protocol_bdata = make([]byte, PROTOCOL_BINARY_DATA_SIZE)
  for i := 0; i < PROTOCOL_BINARY_DATA_SIZE; i++ {
    protocol_bdata[i] = byte((i + 'a') % 255)
  }
  data = string(protocol_bdata)
  BOOL_VALUES = []bool{false, true, false, false, true}
  BYTE_VALUES = []byte{117, 0, 1, 32, 127, 128, 255}
  INT16_VALUES = []int16{459, 0, 1, -1, -128, 127, 32767, -32768}
  INT32_VALUES = []int32{459, 0, 1, -1, -128, 127, 32767, 2147483647, -2147483535}
  INT64_VALUES = []int64{459, 0, 1, -1, -128, 127, 32767, 2147483647, -2147483535, 34359738481, -35184372088719, -9223372036854775808, 9223372036854775807}
  DOUBLE_VALUES = []float64{459.3, 0.0, -1.0, 1.0, 0.5, 0.3333, 3.14159, 1.537e-38, 1.673e25, 6.02214179e23, -6.02214179e23, INFINITY.Float64(), NEGATIVE_INFINITY.Float64(), NAN.Float64()}
  STRING_VALUES = []string{"", "a", "st[uf]f", "st,u:ff with spaces", "stuff\twith\nescape\\characters'...\"lots{of}fun</xml>"}
}

type HTTPEchoServer struct{}

func (p *HTTPEchoServer) ServeHTTP(w http.ResponseWriter, req *http.Request) {
  w.WriteHeader(http.StatusOK)
  io.Copy(w, req.Body)
}

func HttpClientSetupForTest(t *testing.T) (net.Listener, net.Addr) {
  addr, err := FindAvailableTCPServerPort(40000)
  if err != nil {
    t.Fatalf("Unable to find available tcp port addr: %s", err)
  }
  l, err := net.Listen(addr.Network(), addr.String())
  if err != nil {
    t.Fatalf("Unable to setup tcp listener on %s: %s", addr.String(), err)
  }
  go http.Serve(l, &HTTPEchoServer{})
  return l, addr
}


func ReadWriteProtocolTest(t *testing.T, protocolFactory TProtocolFactory) {
  buf := bytes.NewBuffer(make([]byte, 0, 1024))
  l, addr := HttpClientSetupForTest(t)
  transports := []TTransportFactory{
    NewTMemoryBufferTransportFactory(1024),
    NewTIOStreamTransportFactory(buf, buf, true),
    NewTFramedTransportFactory(NewTMemoryBufferTransportFactory(1024)),
    NewTHttpPostClientTransportFactory("http://" + addr.String()),
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteBool(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteByte(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteI16(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteI32(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteI64(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteDouble(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteString(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteBinary(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteWork(t, p, trans)
    trans.Close()
  }
  for _, tf := range transports {
    trans := tf.GetTransport(nil)
    p := protocolFactory.GetProtocol(trans)
    ReadWriteCalculate(t, p, trans)
    trans.Close()
  }

  // this test doesn't work in all cases due to EOF issues between
  // buffer read and buffer write when using the same bufio for both
  //for _, tf := range transports {
  //  trans := tf.GetTransport(nil)
  //  p := GetProtocol(trans);
  //  ReadWriteI64(t, p, trans);
  //  ReadWriteDouble(t, p, trans);
  //  ReadWriteBinary(t, p, trans);
  //  ReadWriteByte(t, p, trans);
  //  trans.Close()
  //}

  l.Close()
}

func ReadWriteBool(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(BOOL)
  thelen := len(BOOL_VALUES)
  err := p.WriteListBegin(thetype, thelen)
  if err != nil {
    t.Errorf("%s: %T %T %q Error writing list begin: %q", "ReadWriteBool", p, trans, err, thetype)
  }
  for k, v := range BOOL_VALUES {
    err = p.WriteBool(v)
    if err != nil {
      t.Errorf("%s: %T %T %q Error writing bool in list at index %d: %q", "ReadWriteBool", p, trans, err, k, v)
    }
  }
  p.WriteListEnd()
  if err != nil {
    t.Errorf("%s: %T %T %q Error writing list end: %q", "ReadWriteBool", p, trans, err, BOOL_VALUES)
  }
  p.Flush()
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q", "ReadWriteBool", p, trans, err, BOOL_VALUES)
  }
  _, ok := p.(*TSimpleJSONProtocol)
  if !ok {
    if thetype != thetype2 {
      t.Errorf("%s: %T %T type %s != type %s", "ReadWriteBool", p, trans, thetype, thetype2)
    }
    if thelen != thelen2 {
      t.Errorf("%s: %T %T len %s != len %s", "ReadWriteBool", p, trans, thelen, thelen2)
    }
  }
  for k, v := range BOOL_VALUES {
    value, err := p.ReadBool()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading bool at index %d: %q", "ReadWriteBool", p, trans, err, k, v)
    }
    if v != value {
      t.Errorf("%s: index %d %q %q %q != %q", "ReadWriteBool", k, p, trans, v, value)
    }
  }
  err = p.ReadListEnd()
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteBool", p, trans, err)
  }
}

func ReadWriteByte(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(BYTE)
  thelen := len(BYTE_VALUES)
  err := p.WriteListBegin(thetype, thelen)
  if err != nil {
    t.Errorf("%s: %T %T %q Error writing list begin: %q", "ReadWriteByte", p, trans, err, thetype)
  }
  for k, v := range BYTE_VALUES {
    err = p.WriteByte(v)
    if err != nil {
      t.Errorf("%s: %T %T %q Error writing byte in list at index %d: %q", "ReadWriteByte", p, trans, err, k, v)
    }
  }
  err = p.WriteListEnd()
  if err != nil {
    t.Errorf("%s: %T %T %q Error writing list end: %q", "ReadWriteByte", p, trans, err, BYTE_VALUES)
  }
  err = p.Flush()
  if err != nil {
    t.Errorf("%s: %T %T %q Error flushing list of bytes: %q", "ReadWriteByte", p, trans, err, BYTE_VALUES)
  }
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q", "ReadWriteByte", p, trans, err, BYTE_VALUES)
  }
  _, ok := p.(*TSimpleJSONProtocol)
  if !ok {
    if thetype != thetype2 {
      t.Errorf("%s: %T %T type %s != type %s", "ReadWriteByte", p, trans, thetype, thetype2)
    }
    if thelen != thelen2 {
      t.Errorf("%s: %T %T len %s != len %s", "ReadWriteByte", p, trans, thelen, thelen2)
    }
  }
  for k, v := range BYTE_VALUES {
    value, err := p.ReadByte()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading byte at index %d: %q", "ReadWriteByte", p, trans, err, k, v)
    }
    if v != value {
      t.Errorf("%s: %T %T %d != %d", "ReadWriteByte", p, trans, v, value)
    }
  }
  err = p.ReadListEnd()
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteByte", p, trans, err)
  }
}

func ReadWriteI16(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(I16)
  thelen := len(INT16_VALUES)
  p.WriteListBegin(thetype, thelen)
  for _, v := range INT16_VALUES {
    p.WriteI16(v)
  }
  p.WriteListEnd()
  p.Flush()
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q", "ReadWriteI16", p, trans, err, INT16_VALUES)
  }
  _, ok := p.(*TSimpleJSONProtocol)
  if !ok {
    if thetype != thetype2 {
      t.Errorf("%s: %T %T type %s != type %s", "ReadWriteI16", p, trans, thetype, thetype2)
    }
    if thelen != thelen2 {
      t.Errorf("%s: %T %T len %s != len %s", "ReadWriteI16", p, trans, thelen, thelen2)
    }
  }
  for k, v := range INT16_VALUES {
    value, err := p.ReadI16()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading int16 at index %d: %q", "ReadWriteI16", p, trans, err, k, v)
    }
    if v != value {
      t.Errorf("%s: %T %T %d != %d", "ReadWriteI16", p, trans, v, value)
    }
  }
  err = p.ReadListEnd()
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteI16", p, trans, err)
  }
}

func ReadWriteI32(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(I32)
  thelen := len(INT32_VALUES)
  p.WriteListBegin(thetype, thelen)
  for _, v := range INT32_VALUES {
    p.WriteI32(v)
  }
  p.WriteListEnd()
  p.Flush()
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q", "ReadWriteI32", p, trans, err, INT32_VALUES)
  }
  _, ok := p.(*TSimpleJSONProtocol)
  if !ok {
    if thetype != thetype2 {
      t.Errorf("%s: %T %T type %s != type %s", "ReadWriteI32", p, trans, thetype, thetype2)
    }
    if thelen != thelen2 {
      t.Errorf("%s: %T %T len %s != len %s", "ReadWriteI32", p, trans, thelen, thelen2)
    }
  }
  for k, v := range INT32_VALUES {
    value, err := p.ReadI32()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading int32 at index %d: %q", "ReadWriteI32", p, trans, err, k, v)
    }
    if v != value {
      t.Errorf("%s: %T %T %d != %d", "ReadWriteI32", p, trans, v, value)
    }
  }
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteI32", p, trans, err)
  }
}

func ReadWriteI64(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(I64)
  thelen := len(INT64_VALUES)
  p.WriteListBegin(thetype, thelen)
  for _, v := range INT64_VALUES {
    p.WriteI64(v)
  }
  p.WriteListEnd()
  p.Flush()
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q", "ReadWriteI64", p, trans, err, INT64_VALUES)
  }
  _, ok := p.(*TSimpleJSONProtocol)
  if !ok {
    if thetype != thetype2 {
      t.Errorf("%s: %T %T type %s != type %s", "ReadWriteI64", p, trans, thetype, thetype2)
    }
    if thelen != thelen2 {
      t.Errorf("%s: %T %T len %s != len %s", "ReadWriteI64", p, trans, thelen, thelen2)
    }
  }
  for k, v := range INT64_VALUES {
    value, err := p.ReadI64()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading int64 at index %d: %q", "ReadWriteI64", p, trans, err, k, v)
    }
    if v != value {
      t.Errorf("%s: %T %T %q != %q", "ReadWriteI64", p, trans, v, value)
    }
  }
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteI64", p, trans, err)
  }
}

func ReadWriteDouble(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(DOUBLE)
  thelen := len(DOUBLE_VALUES)
  p.WriteListBegin(thetype, thelen)
  for _, v := range DOUBLE_VALUES {
    p.WriteDouble(v)
  }
  p.WriteListEnd()
  p.Flush()
  wrotebuffer := ""
  if memtrans, ok := trans.(*TMemoryBuffer); ok {
    wrotebuffer = memtrans.String()
  }
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q, wrote: %v", "ReadWriteDouble", p, trans, err, DOUBLE_VALUES, wrotebuffer)
  }
  if thetype != thetype2 {
    t.Errorf("%s: %T %T type %s != type %s", "ReadWriteDouble", p, trans, thetype, thetype2)
  }
  if thelen != thelen2 {
    t.Errorf("%s: %T %T len %s != len %s", "ReadWriteDouble", p, trans, thelen, thelen2)
  }
  for k, v := range DOUBLE_VALUES {
    value, err := p.ReadDouble()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading double at index %d: %q", "ReadWriteDouble", p, trans, err, k, v)
    }
    if math.IsNaN(v) {
      if !math.IsNaN(value) {
        t.Errorf("%s: %T %T math.IsNaN(%q) != math.IsNaN(%q)", "ReadWriteDouble", p, trans, v, value)
      }
    } else if v != value {
      t.Errorf("%s: %T %T %v != %q", "ReadWriteDouble", p, trans, v, value)
    }
  }
  err = p.ReadListEnd()
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteDouble", p, trans, err)
  }
}

func ReadWriteString(t *testing.T, p TProtocol, trans TTransport) {
  thetype := TType(STRING)
  thelen := len(STRING_VALUES)
  p.WriteListBegin(thetype, thelen)
  for _, v := range STRING_VALUES {
    p.WriteString(v)
  }
  p.WriteListEnd()
  p.Flush()
  thetype2, thelen2, err := p.ReadListBegin()
  if err != nil {
    t.Errorf("%s: %T %T %q Error reading list: %q", "ReadWriteString", p, trans, err, STRING_VALUES)
  }
  _, ok := p.(*TSimpleJSONProtocol)
  if !ok {
    if thetype != thetype2 {
      t.Errorf("%s: %T %T type %s != type %s", "ReadWriteString", p, trans, thetype, thetype2)
    }
    if thelen != thelen2 {
      t.Errorf("%s: %T %T len %s != len %s", "ReadWriteString", p, trans, thelen, thelen2)
    }
  }
  for k, v := range STRING_VALUES {
    value, err := p.ReadString()
    if err != nil {
      t.Errorf("%s: %T %T %q Error reading string at index %d: %q", "ReadWriteString", p, trans, err, k, v)
    }
    if v != value {
      t.Errorf("%s: %T %T %d != %d", "ReadWriteString", p, trans, v, value)
    }
  }
  if err != nil {
    t.Errorf("%s: %T %T Unable to read list end: %q", "ReadWriteString", p, trans, err)
  }
}


func ReadWriteBinary(t *testing.T, p TProtocol, trans TTransport) {
  v := protocol_bdata
  p.WriteBinary(v)
  p.Flush()
  value, err := p.ReadBinary()
  if err != nil {
    t.Errorf("%s: %T %T Unable to read binary: %s", "ReadWriteBinary", p, trans, err.String())
  }
  if len(v) != len(value) {
    t.Errorf("%s: %T %T len(v) != len(value)... %d != %d", "ReadWriteBinary", p, trans, len(v), len(value))
  } else {
    for i := 0; i < len(v); i++ {
      if v[i] != value[i] {
        t.Errorf("%s: %T %T %s != %s", "ReadWriteBinary", p, trans, v, value)
      }
    }
  }
}


func ReadWriteWork(t *testing.T, p TProtocol, trans TTransport) {
  thetype := "struct"
  orig := NewWork()
  orig.Num1 = 25
  orig.Num2 = 102
  orig.Op = ADD
  orig.Comment = "Add: 25 + 102"
  return
  if e := orig.Write(p); e != nil {
    t.Fatalf("Unable to write %s value %#v due to error: %s", thetype, orig, e.String())
  }
  read := NewWork()
  e := read.Read(p)
  if e != nil {
    t.Fatalf("Unable to read %s due to error: %s", thetype, e.String())
  }
  if !orig.Equals(read) {
    t.Fatalf("Original Write != Read: %#v != %#v ", orig, read)
  }
}


func ReadWriteCalculate(t *testing.T, p TProtocol, trans TTransport) {
  messageName := "calculate"
  logid := int32(12)
  seqId := int32(35)
  w := NewWork()
  w.Num1 = 25
  w.Num2 = 102
  w.Op = ADD
  w.Comment = "Add: 25 + 102"

  args31 := NewCalculateArgs()
  args31.Logid = logid
  args31.W = w
  p.WriteMessageBegin(messageName, CALL, seqId)
  if err := args31.Write(p); err != nil {
    t.Fatalf("%s: %T %T Unable to write message: %s", messageName, p, trans, err.String())
  }
  p.WriteMessageEnd()
  p.Transport().Flush()

  name, ttype, seqid, err1 := p.ReadMessageBegin()
  if err1 != nil {
    t.Fatalf("%s: %T %T Unable to read message begin: %s", messageName, p, trans, err1.String())
  }
  if name != messageName {
    t.Errorf("%s: %T %T Expected message named \"%s\", but was: \"%s\"", messageName, p, trans, messageName, name)
  }
  if ttype != CALL {
    t.Errorf("%s: %T %T Expected message type \"%s\", but was: \"%s\"", messageName, p, trans, CALL, ttype)
  }
  if seqid != seqId {
    t.Errorf("%s: %T %T Expected message type \"%s\", but was: \"%s\"", messageName, p, trans, seqId, seqid)
  }
  calcArgs := NewCalculateArgs()
  err2 := calcArgs.Read(p)
  if !args31.Equals(calcArgs) {
    //cmp1, _ := args31.W.CompareTo(calcArgs.W)
    cmp2, ok := args31.CompareTo(calcArgs)
    t.Errorf("%s: %T %T Calculate args not as expected, %T vs %T, cmp: %#v, ok: %#v, equals: %#v", messageName, p, trans, args31, calcArgs, cmp2, ok, args31.Equals(calcArgs))
  }
  if err2 != nil {
    t.Fatalf("%s: %T %T Unable to read message end: %s", messageName, p, trans, err2.String())
  }
  err3 := p.ReadMessageEnd()
  if err3 != nil {
    t.Fatalf("%s: %T %T Unable to read message end: %s", messageName, p, trans, err3.String())
  }
}


/**
 *You can define enums, which are just 32 bit integers. Values are optional
 *and start at 1 if not supplied, C style again.
 */
type Operation int

const (
  ADD      Operation = 1
  SUBTRACT Operation = 2
  MULTIPLY Operation = 3
  DIVIDE   Operation = 4
)

func (p Operation) String() string {
  switch p {
  case ADD:
    return "ADD"
  case SUBTRACT:
    return "SUBTRACT"
  case MULTIPLY:
    return "MULTIPLY"
  case DIVIDE:
    return "DIVIDE"
  }
  return ""
}

func FromOperationString(s string) Operation {
  switch s {
  case "ADD":
    return ADD
  case "SUBTRACT":
    return SUBTRACT
  case "MULTIPLY":
    return MULTIPLY
  case "DIVIDE":
    return DIVIDE
  }
  return Operation(-10000)
}

func (p Operation) Value() int {
  return int(p)
}

func (p Operation) IsEnum() bool {
  return true
}

/**
 *Thrift lets you do typedefs to get pretty names for your types. Standard
 *C style here.
 */
type MyInteger int32

const INT32CONSTANT = 9853

var MAPCONSTANT TMap
/**
 * Structs are the basic complex data structures. They are comprised of fields
 * which each have an integer identifier, a type, a symbolic name, and an
 * optional default value.
 * 
 * Fields can be declared "optional", which ensures they will not be included
 * in the serialized output if they aren't set.  Note that this requires some
 * manual management in some languages.
 * 
 * Attributes:
 *  - Num1
 *  - Num2
 *  - Op
 *  - Comment
 */
type Work struct {
  TStruct
  _       interface{} "num1"    // nil # 0
  Num1    int32       "num1"    // 1
  Num2    int32       "num2"    // 2
  Op      Operation   "op"      // 3
  Comment string      "comment" // 4
}

func NewWork() *Work {
  output := &Work{
    TStruct: NewTStruct("Work", []TField{
      NewTField("num1", I32, 1),
      NewTField("num2", I32, 2),
      NewTField("op", I32, 3),
      NewTField("comment", STRING, 4),
    }),
  }
  {
    output.Num1 = 0
  }
  return output
}

func (p *Work) Read(iprot TProtocol) (err TProtocolException) {
  _, err = iprot.ReadStructBegin()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  for {
    fieldName, fieldTypeId, fieldId, err := iprot.ReadFieldBegin()
    if fieldId < 0 {
      fieldId = int16(p.FieldIdFromFieldName(fieldName))
    } else if fieldName == "" {
      fieldName = p.FieldNameFromFieldId(int(fieldId))
    }
    if fieldTypeId == GENERIC {
      fieldTypeId = p.FieldFromFieldId(int(fieldId)).TypeId()
    }
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
    if fieldTypeId == STOP {
      break
    }
    if fieldId == 1 || fieldName == "num1" {
      if fieldTypeId == I32 {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else if fieldId == 2 || fieldName == "num2" {
      if fieldTypeId == I32 {
        err = p.ReadField2(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField2(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else if fieldId == 3 || fieldName == "op" {
      if fieldTypeId == I32 {
        err = p.ReadField3(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField3(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else if fieldId == 4 || fieldName == "comment" {
      if fieldTypeId == STRING {
        err = p.ReadField4(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField4(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else {
      err = iprot.Skip(fieldTypeId)
      if err != nil {
        return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
      }
    }
    err = iprot.ReadFieldEnd()
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
  }
  err = iprot.ReadStructEnd()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  return err
}

func (p *Work) ReadField1(iprot TProtocol) (err TProtocolException) {
  v4, err5 := iprot.ReadI32()
  if err5 != nil {
    return NewTProtocolExceptionReadField(1, "num1", p.ThriftName(), err5)
  }
  p.Num1 = v4
  return err
}

func (p *Work) ReadFieldNum1(iprot TProtocol) TProtocolException {
  return p.ReadField1(iprot)
}

func (p *Work) ReadField2(iprot TProtocol) (err TProtocolException) {
  v6, err7 := iprot.ReadI32()
  if err7 != nil {
    return NewTProtocolExceptionReadField(2, "num2", p.ThriftName(), err7)
  }
  p.Num2 = v6
  return err
}

func (p *Work) ReadFieldNum2(iprot TProtocol) TProtocolException {
  return p.ReadField2(iprot)
}

func (p *Work) ReadField3(iprot TProtocol) (err TProtocolException) {
  v8, err9 := iprot.ReadI32()
  if err9 != nil {
    return NewTProtocolExceptionReadField(3, "op", p.ThriftName(), err9)
  }
  p.Op = Operation(v8)
  return err
}

func (p *Work) ReadFieldOp(iprot TProtocol) TProtocolException {
  return p.ReadField3(iprot)
}

func (p *Work) ReadField4(iprot TProtocol) (err TProtocolException) {
  v10, err11 := iprot.ReadString()
  if err11 != nil {
    return NewTProtocolExceptionReadField(4, "comment", p.ThriftName(), err11)
  }
  p.Comment = v10
  return err
}

func (p *Work) ReadFieldComment(iprot TProtocol) TProtocolException {
  return p.ReadField4(iprot)
}

func (p *Work) Write(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteStructBegin("Work")
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  err = p.WriteField1(oprot)
  if err != nil {
    return err
  }
  err = p.WriteField2(oprot)
  if err != nil {
    return err
  }
  err = p.WriteField3(oprot)
  if err != nil {
    return err
  }
  err = p.WriteField4(oprot)
  if err != nil {
    return err
  }
  err = oprot.WriteFieldStop()
  if err != nil {
    return NewTProtocolExceptionWriteField(-1, "STOP", p.ThriftName(), err)
  }
  err = oprot.WriteStructEnd()
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  return err
}

func (p *Work) WriteField1(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("num1", I32, 1)
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "num1", p.ThriftName(), err)
  }
  err = oprot.WriteI32(int32(p.Num1))
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "num1", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "num1", p.ThriftName(), err)
  }
  return err
}

func (p *Work) WriteFieldNum1(oprot TProtocol) TProtocolException {
  return p.WriteField1(oprot)
}

func (p *Work) WriteField2(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("num2", I32, 2)
  if err != nil {
    return NewTProtocolExceptionWriteField(2, "num2", p.ThriftName(), err)
  }
  err = oprot.WriteI32(int32(p.Num2))
  if err != nil {
    return NewTProtocolExceptionWriteField(2, "num2", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(2, "num2", p.ThriftName(), err)
  }
  return err
}

func (p *Work) WriteFieldNum2(oprot TProtocol) TProtocolException {
  return p.WriteField2(oprot)
}

func (p *Work) WriteField3(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("op", I32, 3)
  if err != nil {
    return NewTProtocolExceptionWriteField(3, "op", p.ThriftName(), err)
  }
  err = oprot.WriteI32(int32(p.Op))
  if err != nil {
    return NewTProtocolExceptionWriteField(3, "op", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(3, "op", p.ThriftName(), err)
  }
  return err
}

func (p *Work) WriteFieldOp(oprot TProtocol) TProtocolException {
  return p.WriteField3(oprot)
}

func (p *Work) WriteField4(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("comment", STRING, 4)
  if err != nil {
    return NewTProtocolExceptionWriteField(4, "comment", p.ThriftName(), err)
  }
  err = oprot.WriteString(string(p.Comment))
  if err != nil {
    return NewTProtocolExceptionWriteField(4, "comment", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(4, "comment", p.ThriftName(), err)
  }
  return err
}

func (p *Work) WriteFieldComment(oprot TProtocol) TProtocolException {
  return p.WriteField4(oprot)
}

func (p *Work) TStructName() string {
  return "Work"
}

func (p *Work) ThriftName() string {
  return "Work"
}

func (p *Work) String() string {
  if p == nil {
    return "<nil>"
  }
  return fmt.Sprintf("Work(%+v)", *p)
}

func (p *Work) CompareTo(other interface{}) (int, bool) {
  if other == nil {
    return 1, true
  }
  data, ok := other.(*Work)
  if !ok {
    return 0, false
  }
  if p.Num1 != data.Num1 {
    if p.Num1 < data.Num1 {
      return -1, true
    }
    return 1, true
  }
  if p.Num2 != data.Num2 {
    if p.Num2 < data.Num2 {
      return -1, true
    }
    return 1, true
  }
  if p.Op != data.Op {
    if p.Op < data.Op {
      return -1, true
    }
    return 1, true
  }
  if p.Comment != data.Comment {
    if p.Comment < data.Comment {
      return -1, true
    }
    return 1, true
  }
  return 0, true
}

func (p *Work) AttributeByFieldId(id int) interface{} {
  switch id {
  default:
    return nil
  case 1:
    return p.Num1
  case 2:
    return p.Num2
  case 3:
    return p.Op
  case 4:
    return p.Comment
  }
  return nil
}

func (p *Work) TStructFields() TFieldContainer {
  return NewTFieldContainer([]TField{
    NewTField("num1", I32, 1),
    NewTField("num2", I32, 2),
    NewTField("op", I32, 3),
    NewTField("comment", STRING, 4),
  })
}


type ICalculator interface {
  /**
   * Parameters:
   *  - Key
   */
  Calculate(logid int32, w *Work) (retval30 int32, ouch *InvalidOperation, err os.Error)
}

type CalculatorClient struct {
  Transport       TTransport
  ProtocolFactory TProtocolFactory
  InputProtocol   TProtocol
  OutputProtocol  TProtocol
  SeqId           int32
}

func NewCalculatorClientFactory(t TTransport, f TProtocolFactory) *CalculatorClient {
  return &CalculatorClient{Transport: t,
    ProtocolFactory: f,
    InputProtocol:   f.GetProtocol(t),
    OutputProtocol:  f.GetProtocol(t),
    SeqId:           0,
  }
}

func NewCalculatorClientProtocol(t TTransport, iprot TProtocol, oprot TProtocol) *CalculatorClient {
  return &CalculatorClient{Transport: t,
    ProtocolFactory: nil,
    InputProtocol:   iprot,
    OutputProtocol:  oprot,
    SeqId:           0,
  }
}


/**
 * Parameters:
 *  - Logid
 *  - W
 */
func (p *CalculatorClient) Calculate(logid int32, w *Work) (retval30 int32, ouch *InvalidOperation, err os.Error) {
  err = p.SendCalculate(logid, w)
  if err != nil {
    return
  }
  return p.RecvCalculate()
}

func (p *CalculatorClient) SendCalculate(logid int32, w *Work) (err os.Error) {
  oprot := p.OutputProtocol
  if oprot != nil {
    oprot = p.ProtocolFactory.GetProtocol(p.Transport)
    p.OutputProtocol = oprot
  }
  oprot.WriteMessageBegin("calculate", CALL, p.SeqId)
  args31 := NewCalculateArgs()
  args31.Logid = logid
  args31.W = w
  err = args31.Write(oprot)
  oprot.WriteMessageEnd()
  oprot.Transport().Flush()
  return
}


func (p *CalculatorClient) RecvCalculate() (value int32, ouch *InvalidOperation, err os.Error) {
  iprot := p.InputProtocol
  if iprot == nil {
    iprot = p.ProtocolFactory.GetProtocol(p.Transport)
    p.InputProtocol = iprot
  }
  _, mTypeId, _, err := iprot.ReadMessageBegin()
  if err != nil {
    return
  }
  if mTypeId == EXCEPTION {
    error33 := NewTApplicationExceptionDefault()
    error34, err := error33.Read(iprot)
    if err != nil {
      return
    }
    if err = iprot.ReadMessageEnd(); err != nil {
      return
    }
    err = error34
    return
  }
  result32 := NewCalculateResult()
  err = result32.Read(iprot)
  iprot.ReadMessageEnd()
  value = result32.Success
  if result32.Ouch != nil {
    ouch = result32.Ouch
  }
  return
}


/**
 * Attributes:
 *  - Logid
 *  - W
 */
type CalculateArgs struct {
  TStruct
  _     interface{} "logid" // nil # 0
  Logid int32       "logid" // 1
  W     *Work       "w"     // 2
}

func NewCalculateArgs() *CalculateArgs {
  output := &CalculateArgs{
    TStruct: NewTStruct("calculate_args", []TField{
      NewTField("logid", I32, 1),
      NewTField("w", STRUCT, 2),
    }),
  }
  {
  }
  return output
}

func (p *CalculateArgs) Read(iprot TProtocol) (err TProtocolException) {
  _, err = iprot.ReadStructBegin()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  for {
    fieldName, fieldTypeId, fieldId, err := iprot.ReadFieldBegin()
    if fieldId < 0 {
      fieldId = int16(p.FieldIdFromFieldName(fieldName))
    } else if fieldName == "" {
      fieldName = p.FieldNameFromFieldId(int(fieldId))
    }
    if fieldTypeId == GENERIC {
      fieldTypeId = p.FieldFromFieldId(int(fieldId)).TypeId()
    }
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
    if fieldTypeId == STOP {
      break
    }
    if fieldId == 1 || fieldName == "logid" {
      if fieldTypeId == I32 {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else if fieldId == 2 || fieldName == "w" {
      if fieldTypeId == STRUCT {
        err = p.ReadField2(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField2(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else {
      err = iprot.Skip(fieldTypeId)
      if err != nil {
        return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
      }
    }
    err = iprot.ReadFieldEnd()
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
  }
  err = iprot.ReadStructEnd()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  return err
}

func (p *CalculateArgs) ReadField1(iprot TProtocol) (err TProtocolException) {
  v47, err48 := iprot.ReadI32()
  if err48 != nil {
    return NewTProtocolExceptionReadField(1, "logid", p.ThriftName(), err48)
  }
  p.Logid = v47
  return err
}

func (p *CalculateArgs) ReadFieldLogid(iprot TProtocol) TProtocolException {
  return p.ReadField1(iprot)
}

func (p *CalculateArgs) ReadField2(iprot TProtocol) (err TProtocolException) {
  p.W = NewWork()
  err51 := p.W.Read(iprot)
  if err51 != nil {
    return NewTProtocolExceptionReadStruct("p.WWork", err51)
  }
  return err
}

func (p *CalculateArgs) ReadFieldW(iprot TProtocol) TProtocolException {
  return p.ReadField2(iprot)
}

func (p *CalculateArgs) Write(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteStructBegin("calculate_args")
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  err = p.WriteField1(oprot)
  if err != nil {
    return err
  }
  err = p.WriteField2(oprot)
  if err != nil {
    return err
  }
  err = oprot.WriteFieldStop()
  if err != nil {
    return NewTProtocolExceptionWriteField(-1, "STOP", p.ThriftName(), err)
  }
  err = oprot.WriteStructEnd()
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  return err
}

func (p *CalculateArgs) WriteField1(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("logid", I32, 1)
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "logid", p.ThriftName(), err)
  }
  err = oprot.WriteI32(int32(p.Logid))
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "logid", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "logid", p.ThriftName(), err)
  }
  return err
}

func (p *CalculateArgs) WriteFieldLogid(oprot TProtocol) TProtocolException {
  return p.WriteField1(oprot)
}

func (p *CalculateArgs) WriteField2(oprot TProtocol) (err TProtocolException) {
  if p.W != nil {
    err = oprot.WriteFieldBegin("w", STRUCT, 2)
    if err != nil {
      return NewTProtocolExceptionWriteField(2, "w", p.ThriftName(), err)
    }
    err = p.W.Write(oprot)
    if err != nil {
      return NewTProtocolExceptionWriteStruct("Work", err)
    }
    err = oprot.WriteFieldEnd()
    if err != nil {
      return NewTProtocolExceptionWriteField(2, "w", p.ThriftName(), err)
    }
  }
  return err
}

func (p *CalculateArgs) WriteFieldW(oprot TProtocol) TProtocolException {
  return p.WriteField2(oprot)
}

func (p *CalculateArgs) TStructName() string {
  return "CalculateArgs"
}

func (p *CalculateArgs) ThriftName() string {
  return "calculate_args"
}

func (p *CalculateArgs) String() string {
  if p == nil {
    return "<nil>"
  }
  return fmt.Sprintf("CalculateArgs(%+v)", *p)
}

func (p *CalculateArgs) CompareTo(other interface{}) (int, bool) {
  if other == nil {
    return 1, true
  }
  data, ok := other.(*CalculateArgs)
  if !ok {
    return 0, false
  }
  if p.Logid != data.Logid {
    if p.Logid < data.Logid {
      return -1, true
    }
    return 1, true
  }
  if cmp, ok := p.W.CompareTo(data.W); !ok || cmp != 0 {
    return cmp, ok
  }
  return 0, true
}

func (p *CalculateArgs) AttributeByFieldId(id int) interface{} {
  switch id {
  default:
    return nil
  case 1:
    return p.Logid
  case 2:
    return p.W
  }
  return nil
}

func (p *CalculateArgs) TStructFields() TFieldContainer {
  return NewTFieldContainer([]TField{
    NewTField("logid", I32, 1),
    NewTField("w", STRUCT, 2),
  })
}

/**
 * Attributes:
 *  - Success
 *  - Ouch
 */
type CalculateResult struct {
  TStruct
  Success int32             "success" // 0
  Ouch    *InvalidOperation "ouch"    // 1
}

func NewCalculateResult() *CalculateResult {
  output := &CalculateResult{
    TStruct: NewTStruct("calculate_result", []TField{
      NewTField("success", I32, 0),
      NewTField("ouch", STRUCT, 1),
    }),
  }
  {
  }
  return output
}

func (p *CalculateResult) Read(iprot TProtocol) (err TProtocolException) {
  _, err = iprot.ReadStructBegin()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  for {
    fieldName, fieldTypeId, fieldId, err := iprot.ReadFieldBegin()
    if fieldId < 0 {
      fieldId = int16(p.FieldIdFromFieldName(fieldName))
    } else if fieldName == "" {
      fieldName = p.FieldNameFromFieldId(int(fieldId))
    }
    if fieldTypeId == GENERIC {
      fieldTypeId = p.FieldFromFieldId(int(fieldId)).TypeId()
    }
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
    if fieldTypeId == STOP {
      break
    }
    if fieldId == 0 || fieldName == "success" {
      if fieldTypeId == I32 {
        err = p.ReadField0(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField0(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else if fieldId == 1 || fieldName == "ouch" {
      if fieldTypeId == STRUCT {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else {
      err = iprot.Skip(fieldTypeId)
      if err != nil {
        return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
      }
    }
    err = iprot.ReadFieldEnd()
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
  }
  err = iprot.ReadStructEnd()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  return err
}

func (p *CalculateResult) ReadField0(iprot TProtocol) (err TProtocolException) {
  v52, err53 := iprot.ReadI32()
  if err53 != nil {
    return NewTProtocolExceptionReadField(0, "success", p.ThriftName(), err53)
  }
  p.Success = v52
  return err
}

func (p *CalculateResult) ReadFieldSuccess(iprot TProtocol) TProtocolException {
  return p.ReadField0(iprot)
}

func (p *CalculateResult) ReadField1(iprot TProtocol) (err TProtocolException) {
  p.Ouch = NewInvalidOperation()
  err56 := p.Ouch.Read(iprot)
  if err56 != nil {
    return NewTProtocolExceptionReadStruct("p.OuchInvalidOperation", err56)
  }
  return err
}

func (p *CalculateResult) ReadFieldOuch(iprot TProtocol) TProtocolException {
  return p.ReadField1(iprot)
}

func (p *CalculateResult) Write(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteStructBegin("calculate_result")
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  err = p.WriteField0(oprot)
  if err != nil {
    return err
  }
  err = p.WriteField1(oprot)
  if err != nil {
    return err
  }
  err = oprot.WriteFieldStop()
  if err != nil {
    return NewTProtocolExceptionWriteField(-1, "STOP", p.ThriftName(), err)
  }
  err = oprot.WriteStructEnd()
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  return err
}

func (p *CalculateResult) WriteField0(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("success", I32, 0)
  if err != nil {
    return NewTProtocolExceptionWriteField(0, "success", p.ThriftName(), err)
  }
  err = oprot.WriteI32(int32(p.Success))
  if err != nil {
    return NewTProtocolExceptionWriteField(0, "success", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(0, "success", p.ThriftName(), err)
  }
  return err
}

func (p *CalculateResult) WriteFieldSuccess(oprot TProtocol) TProtocolException {
  return p.WriteField0(oprot)
}

func (p *CalculateResult) WriteField1(oprot TProtocol) (err TProtocolException) {
  if p.Ouch != nil {
    err = oprot.WriteFieldBegin("ouch", STRUCT, 1)
    if err != nil {
      return NewTProtocolExceptionWriteField(1, "ouch", p.ThriftName(), err)
    }
    err = p.Ouch.Write(oprot)
    if err != nil {
      return NewTProtocolExceptionWriteStruct("InvalidOperation", err)
    }
    err = oprot.WriteFieldEnd()
    if err != nil {
      return NewTProtocolExceptionWriteField(1, "ouch", p.ThriftName(), err)
    }
  }
  return err
}

func (p *CalculateResult) WriteFieldOuch(oprot TProtocol) TProtocolException {
  return p.WriteField1(oprot)
}

func (p *CalculateResult) TStructName() string {
  return "CalculateResult"
}

func (p *CalculateResult) ThriftName() string {
  return "calculate_result"
}

func (p *CalculateResult) String() string {
  if p == nil {
    return "<nil>"
  }
  return fmt.Sprintf("CalculateResult(%+v)", *p)
}

func (p *CalculateResult) CompareTo(other interface{}) (int, bool) {
  if other == nil {
    return 1, true
  }
  data, ok := other.(*CalculateResult)
  if !ok {
    return 0, false
  }
  if p.Success != data.Success {
    if p.Success < data.Success {
      return -1, true
    }
    return 1, true
  }
  if cmp, ok := p.Ouch.CompareTo(data.Ouch); !ok || cmp != 0 {
    return cmp, ok
  }
  return 0, true
}

func (p *CalculateResult) AttributeByFieldId(id int) interface{} {
  switch id {
  default:
    return nil
  case 0:
    return p.Success
  case 1:
    return p.Ouch
  }
  return nil
}

func (p *CalculateResult) TStructFields() TFieldContainer {
  return NewTFieldContainer([]TField{
    NewTField("success", I32, 0),
    NewTField("ouch", STRUCT, 1),
  })
}


/**
 * Structs can also be exceptions, if they are nasty.
 * 
 * Attributes:
 *  - What
 *  - Why
 */
type InvalidOperation struct {
  TStruct
  _    interface{} "what" // nil # 0
  What int32       "what" // 1
  Why  string      "why"  // 2
}

func NewInvalidOperation() *InvalidOperation {
  output := &InvalidOperation{
    TStruct: NewTStruct("InvalidOperation", []TField{
      NewTField("what", I32, 1),
      NewTField("why", STRING, 2),
    }),
  }
  {
  }
  return output
}

func (p *InvalidOperation) Read(iprot TProtocol) (err TProtocolException) {
  _, err = iprot.ReadStructBegin()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  for {
    fieldName, fieldTypeId, fieldId, err := iprot.ReadFieldBegin()
    if fieldId < 0 {
      fieldId = int16(p.FieldIdFromFieldName(fieldName))
    } else if fieldName == "" {
      fieldName = p.FieldNameFromFieldId(int(fieldId))
    }
    if fieldTypeId == GENERIC {
      fieldTypeId = p.FieldFromFieldId(int(fieldId)).TypeId()
    }
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
    if fieldTypeId == STOP {
      break
    }
    if fieldId == 1 || fieldName == "what" {
      if fieldTypeId == I32 {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField1(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else if fieldId == 2 || fieldName == "why" {
      if fieldTypeId == STRING {
        err = p.ReadField2(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else if fieldTypeId == VOID {
        err = iprot.Skip(fieldTypeId)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      } else {
        err = p.ReadField2(iprot)
        if err != nil {
          return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
        }
      }
    } else {
      err = iprot.Skip(fieldTypeId)
      if err != nil {
        return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
      }
    }
    err = iprot.ReadFieldEnd()
    if err != nil {
      return NewTProtocolExceptionReadField(int(fieldId), fieldName, p.ThriftName(), err)
    }
  }
  err = iprot.ReadStructEnd()
  if err != nil {
    return NewTProtocolExceptionReadStruct(p.ThriftName(), err)
  }
  return err
}

func (p *InvalidOperation) ReadField1(iprot TProtocol) (err TProtocolException) {
  v12, err13 := iprot.ReadI32()
  if err13 != nil {
    return NewTProtocolExceptionReadField(1, "what", p.ThriftName(), err13)
  }
  p.What = v12
  return err
}

func (p *InvalidOperation) ReadFieldWhat(iprot TProtocol) TProtocolException {
  return p.ReadField1(iprot)
}

func (p *InvalidOperation) ReadField2(iprot TProtocol) (err TProtocolException) {
  v14, err15 := iprot.ReadString()
  if err15 != nil {
    return NewTProtocolExceptionReadField(2, "why", p.ThriftName(), err15)
  }
  p.Why = v14
  return err
}

func (p *InvalidOperation) ReadFieldWhy(iprot TProtocol) TProtocolException {
  return p.ReadField2(iprot)
}

func (p *InvalidOperation) Write(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteStructBegin("InvalidOperation")
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  err = p.WriteField1(oprot)
  if err != nil {
    return err
  }
  err = p.WriteField2(oprot)
  if err != nil {
    return err
  }
  err = oprot.WriteFieldStop()
  if err != nil {
    return NewTProtocolExceptionWriteField(-1, "STOP", p.ThriftName(), err)
  }
  err = oprot.WriteStructEnd()
  if err != nil {
    return NewTProtocolExceptionWriteStruct(p.ThriftName(), err)
  }
  return err
}

func (p *InvalidOperation) WriteField1(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("what", I32, 1)
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "what", p.ThriftName(), err)
  }
  err = oprot.WriteI32(int32(p.What))
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "what", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(1, "what", p.ThriftName(), err)
  }
  return err
}

func (p *InvalidOperation) WriteFieldWhat(oprot TProtocol) TProtocolException {
  return p.WriteField1(oprot)
}

func (p *InvalidOperation) WriteField2(oprot TProtocol) (err TProtocolException) {
  err = oprot.WriteFieldBegin("why", STRING, 2)
  if err != nil {
    return NewTProtocolExceptionWriteField(2, "why", p.ThriftName(), err)
  }
  err = oprot.WriteString(string(p.Why))
  if err != nil {
    return NewTProtocolExceptionWriteField(2, "why", p.ThriftName(), err)
  }
  err = oprot.WriteFieldEnd()
  if err != nil {
    return NewTProtocolExceptionWriteField(2, "why", p.ThriftName(), err)
  }
  return err
}

func (p *InvalidOperation) WriteFieldWhy(oprot TProtocol) TProtocolException {
  return p.WriteField2(oprot)
}

func (p *InvalidOperation) TStructName() string {
  return "InvalidOperation"
}

func (p *InvalidOperation) ThriftName() string {
  return "InvalidOperation"
}

func (p *InvalidOperation) String() string {
  if p == nil {
    return "<nil>"
  }
  return fmt.Sprintf("InvalidOperation(%+v)", *p)
}

func (p *InvalidOperation) CompareTo(other interface{}) (int, bool) {
  if other == nil {
    return 1, true
  }
  data, ok := other.(*InvalidOperation)
  if !ok {
    return 0, false
  }
  if p.What != data.What {
    if p.What < data.What {
      return -1, true
    }
    return 1, true
  }
  if p.Why != data.Why {
    if p.Why < data.Why {
      return -1, true
    }
    return 1, true
  }
  return 0, true
}

func (p *InvalidOperation) AttributeByFieldId(id int) interface{} {
  switch id {
  default:
    return nil
  case 1:
    return p.What
  case 2:
    return p.Why
  }
  return nil
}

func (p *InvalidOperation) TStructFields() TFieldContainer {
  return NewTFieldContainer([]TField{
    NewTField("what", I32, 1),
    NewTField("why", STRING, 2),
  })
}
