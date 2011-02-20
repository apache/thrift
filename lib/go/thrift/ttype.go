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
  "container/list"
  "container/vector"
  "strconv"
)

/**
 * Type constants in the Thrift protocol.
 */
type TType byte

const (
  STOP    = 0
  VOID    = 1
  BOOL    = 2
  BYTE    = 3
  I08     = 3
  DOUBLE  = 4
  I16     = 6
  I32     = 8
  I64     = 10
  STRING  = 11
  UTF7    = 11
  STRUCT  = 12
  MAP     = 13
  SET     = 14
  LIST    = 15
  ENUM    = 16
  UTF8    = 16
  UTF16   = 17
  GENERIC = 127
)

func (p TType) String() string {
  switch p {
  case STOP:
    return "STOP"
  case VOID:
    return "VOID"
  case BOOL:
    return "BOOL"
  case BYTE:
    return "BYTE"
  case DOUBLE:
    return "DOUBLE"
  case I16:
    return "I16"
  case I32:
    return "I32"
  case I64:
    return "I64"
  case STRING:
    return "STRING"
  case STRUCT:
    return "STRUCT"
  case MAP:
    return "MAP"
  case SET:
    return "SET"
  case LIST:
    return "LIST"
  case ENUM:
    return "ENUM"
  case UTF16:
    return "UTF16"
  case GENERIC:
    return "GENERIC"
  }
  return "Unknown"
}

func (p TType) IsBaseType() bool {
  switch p {
  case BOOL, BYTE, DOUBLE, I16, I32, I64, STRING, UTF8, UTF16:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsEmptyType() bool {
  switch p {
  case STOP, VOID:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsEnum() bool {
  switch p {
  case ENUM:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsNumericType() bool {
  switch p {
  case ENUM, BOOL, BYTE, DOUBLE, I16, I32, I64:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsStringType() bool {
  switch p {
  case STRING, UTF8, UTF16:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsContainer() bool {
  switch p {
  case MAP, SET, LIST:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsStruct() bool {
  switch p {
  case STRUCT:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsMap() bool {
  switch p {
  case MAP:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsList() bool {
  switch p {
  case LIST:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsSet() bool {
  switch p {
  case SET:
    return true
  default:
    return false
  }
  return false
}

func (p TType) IsInt() bool {
  switch p {
  case BYTE, I16, I32, I64:
    return true
  default:
    return false
  }
  return false
}

func (p TType) Coerce(other interface{}) TType {
  if other == nil {
    return TType(STOP)
  }
  switch b := other.(type) {
  default:
    return TType(STOP)
  case nil:
    return TType(STOP)
  case TType:
    return b
  case byte:
    return TType(b)
  case int:
    return TType(byte(b))
  case int8:
    return TType(byte(b))
  case int32:
    return TType(byte(b))
  case int64:
    return TType(byte(b))
  case uint:
    return TType(byte(b))
  case uint32:
    return TType(byte(b))
  case uint64:
    return TType(byte(b))
  case float32:
    return TType(byte(int(b)))
  case float64:
    return TType(byte(int(b)))
  }
  return TType(STOP)
}

func (p TType) LessType(other interface{}) bool {
  return p < p.Coerce(other)
}

func (p TType) Less(i, j interface{}) bool {
  cmp, ok := p.Compare(i, j)
  return ok && cmp > 0
}


func (p TType) Compare(i, j interface{}) (int, bool) {
  if i == j {
    return 0, true
  }
  if i == nil {
    if j == nil {
      return 0, true
    }
    return -1, true
  }
  if j == nil {
    return 1, true
  }
  ci, iok := p.CoerceData(i)
  cj, jok := p.CoerceData(j)
  if iok && !jok {
    return 1, true
  }
  if !iok && jok {
    return -1, true
  }
  // hopefully this doesn't happen as Compare() would continuously return 0, false
  if !iok && !jok {
    return 0, false
  }
  if ci == cj {
    return 0, true
  }
  if ci == nil {
    if cj == nil {
      return 0, true
    }
    return -1, true
  }
  if cj == nil {
    return 1, true
  }
  switch p {
  case STOP, VOID:
    // hopefully this doesn't happen as Compare() would continuously return 0, false
    return 0, false
  case BOOL:
    vi := ci.(bool)
    vj := cj.(bool)
    if vi == vj {
      return 0, true
    }
    if vi == false {
      return -1, true
    }
    return 1, true
  case BYTE:
    vi := ci.(byte)
    vj := cj.(byte)
    if vi == vj {
      return 0, true
    }
    if vi < vj {
      return -1, true
    }
    return 1, true
  case DOUBLE:
    vi := ci.(float64)
    vj := cj.(float64)
    if vi == vj {
      return 0, true
    }
    if vi < vj {
      return -1, true
    }
    return 1, true
  case I16:
    vi := ci.(int16)
    vj := cj.(int16)
    if vi == vj {
      return 0, true
    }
    if vi < vj {
      return -1, true
    }
    return 1, true
  case I32:
    vi := ci.(int32)
    vj := cj.(int32)
    if vi == vj {
      return 0, true
    }
    if vi < vj {
      return -1, true
    }
    return 1, true
  case I64:
    vi := ci.(int64)
    vj := cj.(int64)
    if vi == vj {
      return 0, true
    }
    if vi < vj {
      return -1, true
    }
    return 1, true
  case STRING, UTF8, UTF16:
    vi := ci.(string)
    vj := cj.(string)
    if vi == vj {
      return 0, true
    }
    if vi < vj {
      return -1, true
    }
    return 1, true
  case STRUCT:
    si := ci.(TStruct)
    sj := cj.(TStruct)
    if cmp := CompareString(si.ThriftName(), sj.ThriftName()); cmp != 0 {
      return cmp, true
    }
    if cmp, ok := si.TStructFields().CompareTo(sj.TStructFields()); !ok || cmp != 0 {
      return cmp, ok
    }
    for field := range si.TStructFields().Iter() {
      a := si.AttributeFromFieldId(field.Id())
      b := sj.AttributeFromFieldId(field.Id())
      if cmp, ok := field.TypeId().Compare(a, b); !ok || cmp != 0 {
        return cmp, ok
      }
    }
    return 0, true
  case MAP:
    mi := ci.(TMap)
    mj := cj.(TMap)
    ei := mi.KeyType()
    if ej := mj.KeyType(); ei != ej {
      return CompareInt(int(ei), int(ej)), true
    }
    if size := mi.Len(); size != mj.Len() {
      return CompareInt(size, mj.Len()), true
    }
    if c, cok := ei.Compare(mi.Keys(), mj.Keys()); c != 0 || !cok {
      return c, cok
    }
    return ei.Compare(mi.Values(), mj.Values())
  case LIST:
    li := ci.(TList)
    lj := cj.(TList)
    ei := li.ElemType()
    ej := lj.ElemType()
    if ei != ej {
      return CompareInt(int(ei), int(ej)), true
    }
    size := li.Len()
    if size != lj.Len() {
      return CompareInt(size, lj.Len()), true
    }
    for k := 0; k < size; k++ {
      vi := li.At(k)
      vj := lj.At(k)
      c, cok := ei.Compare(vi, vj)
      if c != 0 || !cok {
        return c, cok
      }
    }
    return 0, true
  case SET:
    li := ci.(TSet)
    lj := cj.(TSet)
    ei := li.ElemType()
    ej := lj.ElemType()
    if ei != ej {
      return CompareInt(int(ei), int(ej)), true
    }
    size := li.Len()
    if size != lj.Len() {
      return CompareInt(size, lj.Len()), true
    }
    return ei.Compare(li.Values(), lj.Values())
  default:
    panic("Invalid thrift type to coerce")
  }
  return 0, false
}

func (p TType) CompareValueArrays(li, lj []interface{}) (int, bool) {
  size := len(li)
  if cmp := CompareInt(size, len(lj)); cmp != 0 {
    return cmp, true
  }
  for i := 0; i < size; i++ {
    vi := li[i]
    vj := lj[i]
    c, cok := p.Compare(vi, vj)
    if c != 0 || !cok {
      return c, cok
    }
  }
  return 0, true
}

func (p TType) Equals(other interface{}) bool {
  return p == p.Coerce(other)
}

type Stringer interface {
  String() string
}

type Enumer interface {
  String() string
  Value() int
  IsEnum() bool
}

func TypeFromValue(data interface{}) TType {
  switch i := data.(type) {
  default:
    return STOP
  case nil:
    return VOID
  case bool:
    return BOOL
  case float32, float64:
    return DOUBLE
  case int, int32:
    return I32
  case byte:
    return BYTE
  case int8:
    return I08
  case int16:
    return I16
  case int64:
    return I64
  case string:
    return STRING
  case TStruct:
    return STRUCT
  case TMap:
    return MAP
  case TSet:
    return SET
  case []interface{}, *list.List, *vector.Vector, TList:
    return LIST
  }
  return STOP
}

func (p TType) CoerceData(data interface{}) (interface{}, bool) {
  if data == nil {
    switch p {
    case STOP:
      return nil, true
    case VOID:
      return nil, true
    case BOOL:
      return false, true
    case BYTE:
      return byte(0), true
    case DOUBLE:
      return float64(0), true
    case I16:
      return int16(0), true
    case I32:
      return int32(0), true
    case I64:
      return int64(0), true
    case STRING, UTF8, UTF16:
      return "", true
    case STRUCT:
      return NewTStructEmpty(""), true
    case MAP:
      return NewTMapDefault(), true
    case LIST:
      return NewTListDefault(), true
    case SET:
      return NewTSetDefault(), true
    default:
      panic("Invalid thrift type to coerce")
    }
  }
  switch p {
  case STOP:
    return nil, true
  case VOID:
    return nil, true
  case BOOL:
    switch b := data.(type) {
    default:
      return false, false
    case bool:
      return b, true
    case Numeric:
      return bool(b.Int() != 0), true
    case int:
      return b != 0, true
    case byte:
      return b != 0, true
    case int8:
      return b != 0, true
    case int16:
      return b != 0, true
    case int32:
      return b != 0, true
    case int64:
      return b != 0, true
    case uint:
      return b != 0, true
    case uint16:
      return b != 0, true
    case uint32:
      return b != 0, true
    case uint64:
      return b != 0, true
    case float32:
      return b != 0, true
    case float64:
      return b != 0, true
    case Stringer:
      v := b.String()
      if v == "false" || v == "0" || len(v) == 0 {
        return false, true
      }
      return true, true
    case string:
      if b == "false" || b == "0" || len(b) == 0 {
        return false, true
      }
      return true, true
    }
  case BYTE:
    if b, ok := data.(byte); ok {
      return b, true
    }
    if b, ok := data.(Numeric); ok {
      return b.Byte(), true
    }
    if b, ok := data.(bool); ok {
      if b {
        return byte(1), true
      }
      return byte(0), true
    }
    if b, ok := data.(int); ok {
      return byte(b), true
    }
    if b, ok := data.(int8); ok {
      return byte(b), true
    }
    if b, ok := data.(int16); ok {
      return byte(b), true
    }
    if b, ok := data.(int32); ok {
      return byte(b), true
    }
    if b, ok := data.(int64); ok {
      return byte(b), true
    }
    if b, ok := data.(uint); ok {
      return byte(b), true
    }
    if b, ok := data.(uint8); ok {
      return byte(b), true
    }
    if b, ok := data.(uint16); ok {
      return byte(b), true
    }
    if b, ok := data.(uint32); ok {
      return byte(b), true
    }
    if b, ok := data.(uint64); ok {
      return byte(b), true
    }
    if b, ok := data.(float32); ok {
      return byte(int(b)), true
    }
    if b, ok := data.(float64); ok {
      return byte(int(b)), true
    }
    if b, ok := data.(Stringer); ok {
      data = b.String()
    }
    if b, ok := data.(string); ok {
      i1, err := strconv.Atoi(b)
      if err != nil {
        return byte(int(i1)), true
      }
    }
    return byte(0), false
  case DOUBLE:
    if b, ok := data.(float32); ok {
      return float64(b), true
    }
    if b, ok := data.(float64); ok {
      return b, true
    }
    if b, ok := data.(Numeric); ok {
      return bool(b.Float64() != 0.0), true
    }
    if b, ok := data.(byte); ok {
      return float64(b), true
    }
    if b, ok := data.(bool); ok {
      if b {
        return float64(1.0), true
      }
      return float64(0.0), true
    }
    if b, ok := data.(int); ok {
      return float64(b), true
    }
    if b, ok := data.(int8); ok {
      return float64(b), true
    }
    if b, ok := data.(int16); ok {
      return float64(b), true
    }
    if b, ok := data.(int32); ok {
      return float64(b), true
    }
    if b, ok := data.(int64); ok {
      return float64(b), true
    }
    if b, ok := data.(uint); ok {
      return float64(b), true
    }
    if b, ok := data.(uint8); ok {
      return float64(b), true
    }
    if b, ok := data.(uint16); ok {
      return float64(b), true
    }
    if b, ok := data.(uint32); ok {
      return float64(b), true
    }
    if b, ok := data.(uint64); ok {
      return float64(b), true
    }
    if b, ok := data.(Stringer); ok {
      data = b.String()
    }
    if b, ok := data.(string); ok {
      d1, err := strconv.Atof64(b)
      if err != nil {
        return d1, true
      }
    }
    return float64(0), false
  case I16:
    if b, ok := data.(int16); ok {
      return b, true
    }
    if b, ok := data.(int); ok {
      return int16(b), true
    }
    if b, ok := data.(Numeric); ok {
      return bool(b.Int16() != 0), true
    }
    if b, ok := data.(byte); ok {
      return int16(b), true
    }
    if b, ok := data.(bool); ok {
      if b {
        return int16(1.0), true
      }
      return int16(0.0), true
    }
    if b, ok := data.(int8); ok {
      return int16(b), true
    }
    if b, ok := data.(int32); ok {
      return int16(b), true
    }
    if b, ok := data.(int64); ok {
      return int16(b), true
    }
    if b, ok := data.(uint); ok {
      return int16(b), true
    }
    if b, ok := data.(uint8); ok {
      return int16(b), true
    }
    if b, ok := data.(uint16); ok {
      return int16(b), true
    }
    if b, ok := data.(uint32); ok {
      return int16(b), true
    }
    if b, ok := data.(uint64); ok {
      return int16(b), true
    }
    if b, ok := data.(float32); ok {
      return int16(int(b)), true
    }
    if b, ok := data.(float64); ok {
      return int16(int(b)), true
    }
    if b, ok := data.(Stringer); ok {
      data = b.String()
    }
    if b, ok := data.(string); ok {
      i1, err := strconv.Atoi(b)
      if err != nil {
        return int16(i1), true
      }
    }
    return int16(0), false
  case I32:
    if b, ok := data.(int32); ok {
      return b, true
    }
    if b, ok := data.(int); ok {
      return int32(b), true
    }
    if b, ok := data.(Numeric); ok {
      return bool(b.Int32() != 0), true
    }
    if b, ok := data.(byte); ok {
      return int32(b), true
    }
    if b, ok := data.(bool); ok {
      if b {
        return int32(1.0), true
      }
      return int32(0.0), true
    }
    if b, ok := data.(int8); ok {
      return int32(b), true
    }
    if b, ok := data.(int16); ok {
      return int32(b), true
    }
    if b, ok := data.(int64); ok {
      return int32(b), true
    }
    if b, ok := data.(uint); ok {
      return int32(b), true
    }
    if b, ok := data.(uint8); ok {
      return int32(b), true
    }
    if b, ok := data.(uint16); ok {
      return int32(b), true
    }
    if b, ok := data.(uint32); ok {
      return int32(b), true
    }
    if b, ok := data.(uint64); ok {
      return int32(b), true
    }
    if b, ok := data.(float32); ok {
      return int32(int(b)), true
    }
    if b, ok := data.(float64); ok {
      return int32(int(b)), true
    }
    if b, ok := data.(Stringer); ok {
      data = b.String()
    }
    if b, ok := data.(string); ok {
      i1, err := strconv.Atoi(b)
      if err != nil {
        return int32(i1), true
      }
    }
    return int32(0), false
  case I64:
    if b, ok := data.(int64); ok {
      return b, true
    }
    if b, ok := data.(int32); ok {
      return int64(b), true
    }
    if b, ok := data.(int); ok {
      return int64(b), true
    }
    if b, ok := data.(Numeric); ok {
      return bool(b.Int64() != 0), true
    }
    if b, ok := data.(byte); ok {
      return int64(b), true
    }
    if b, ok := data.(bool); ok {
      if b {
        return int64(1.0), true
      }
      return int64(0.0), true
    }
    if b, ok := data.(int8); ok {
      return int64(b), true
    }
    if b, ok := data.(int16); ok {
      return int64(b), true
    }
    if b, ok := data.(uint); ok {
      return int64(b), true
    }
    if b, ok := data.(uint8); ok {
      return int64(b), true
    }
    if b, ok := data.(uint16); ok {
      return int64(b), true
    }
    if b, ok := data.(uint32); ok {
      return int64(b), true
    }
    if b, ok := data.(uint64); ok {
      return int64(b), true
    }
    if b, ok := data.(float32); ok {
      return int64(b), true
    }
    if b, ok := data.(float64); ok {
      return int64(b), true
    }
    if b, ok := data.(Stringer); ok {
      data = b.String()
    }
    if b, ok := data.(string); ok {
      i1, err := strconv.Atoi64(b)
      if err != nil {
        return i1, true
      }
    }
    return int64(0), false
  case STRING, UTF8, UTF16:
    if b, ok := data.(Enumer); ok {
      if i1, ok := data.(int); ok {
        return i1, true
      }
      return b.String(), true
    }
    if b, ok := data.(Stringer); ok {
      return b.String(), true
    }
    if b, ok := data.(string); ok {
      return b, true
    }
    if b, ok := data.(int); ok {
      return string(b), true
    }
    if b, ok := data.(byte); ok {
      return string(b), true
    }
    if b, ok := data.(bool); ok {
      if b {
        return "true", true
      }
      return "false", true
    }
    if b, ok := data.(int8); ok {
      return string(b), true
    }
    if b, ok := data.(int16); ok {
      return string(b), true
    }
    if b, ok := data.(int32); ok {
      return string(b), true
    }
    if b, ok := data.(int64); ok {
      return string(b), true
    }
    if b, ok := data.(uint); ok {
      return string(b), true
    }
    if b, ok := data.(uint8); ok {
      return string(b), true
    }
    if b, ok := data.(uint16); ok {
      return string(b), true
    }
    if b, ok := data.(uint32); ok {
      return string(b), true
    }
    if b, ok := data.(uint64); ok {
      return string(b), true
    }
    if b, ok := data.(float32); ok {
      return strconv.Ftoa32(b, 'g', -1), true
    }
    if b, ok := data.(float64); ok {
      return strconv.Ftoa64(b, 'g', -1), true
    }
    return "", false
  case STRUCT:
    if b, ok := data.(TStruct); ok {
      return b, true
    }
    return NewTStructEmpty(""), true
  case MAP:
    if b, ok := data.(TMap); ok {
      return b, true
    }
    return NewTMapDefault(), false
  case LIST:
    if b, ok := data.(TList); ok {
      return b, true
    }
    return NewTListDefault(), false
  case SET:
    if b, ok := data.(TSet); ok {
      return b, true
    }
    return NewTSetDefault(), false
  default:
    panic("Invalid thrift type to coerce")
  }
  return nil, false
}

type EqualsOtherInterface interface {
  Equals(other interface{}) bool
}

type EqualsMap interface {
  Equals(other TMap) bool
}

type EqualsSet interface {
  Equals(other TSet) bool
}

type EqualsList interface {
  Equals(other TList) bool
}

type EqualsStruct interface {
  Equals(other TStruct) bool
}
