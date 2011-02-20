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
  "reflect"
)

/**
 * Helper class that encapsulates map metadata.
 *
 */
type TMap interface {
  KeyType() TType
  ValueType() TType
  Len() int
  Set(key, value interface{})
  Get(key interface{}) (interface{}, bool)
  Contains(key interface{}) bool
  Iter() <-chan TMapElem
  KeyIter() <-chan interface{}
  ValueIter() <-chan interface{}
  Keys() []interface{}
  Values() []interface{}
  Less(other interface{}) bool
  Equals(other interface{}) bool
  CompareTo(other interface{}) (int, bool)
}

type TMapElem interface {
  Key() interface{}
  Value() interface{}
}

type tMap struct {
  keyType   TType
  valueType TType
  size      int
  l         *list.List
  b         map[bool]interface{}
  i08       map[byte]interface{}
  i16       map[int16]interface{}
  i32       map[int32]interface{}
  i64       map[int64]interface{}
  f64       map[float64]interface{}
  s         map[string]interface{}
}

type tMapElem struct {
  key   interface{}
  value interface{}
}

func (p *tMapElem) Key() interface{} {
  return p.key
}

func (p *tMapElem) Value() interface{} {
  return p.value
}

func NewTMapElem(k, v interface{}) TMapElem {
  return &tMapElem{key: k, value: v}
}

func NewTMap(k, v TType, s int) TMap {
  return &tMap{keyType: k, valueType: v, size: s, l: list.New()}
}

func NewTMapDefault() TMap {
  return NewTMap(STOP, STOP, 0)
}

func (p *tMap) KeyType() TType {
  return p.keyType
}

func (p *tMap) ValueType() TType {
  return p.valueType
}

func (p *tMap) Len() int {
  if p.l.Len() != 0 {
    return p.l.Len()
  }
  switch p.KeyType() {
  case STOP, VOID:
    return 0
  case BOOL:
    return len(p.b)
  case BYTE:
    return len(p.i08)
  case I16:
    return len(p.i16)
  case I32:
    return len(p.i32)
  case I64:
    return len(p.i64)
  case DOUBLE:
    return len(p.f64)
  case STRING, UTF8, UTF16:
    return len(p.s)
  default:
    return p.size
  }
  return p.size
}

func (p *tMap) Get(key interface{}) (interface{}, bool) {
  if p.KeyType().IsEmptyType() {
    return nil, false
  }
  if key == nil {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        return e.Value(), true
      }
    }
    return nil, false
  }
  useKey, ok := p.KeyType().CoerceData(key)
  if !ok {
    return nil, false
  }
  switch p.KeyType() {
  case STOP, VOID:
    // if here, then we don't have a key type yet and key is not nil
    // so this is pretty much an empty map
    return nil, false
  case BOOL:
    m := p.b
    if m == nil {
      return nil, false
    }
    if v, ok := m[useKey.(bool)]; ok {
      return v, true
    }
    return nil, true
  case BYTE:
    m := p.i08
    if v, ok := m[useKey.(byte)]; ok {
      return v, true
    }
    return nil, false
  case DOUBLE:
    m := p.f64
    if m == nil {
      return nil, false
    }
    if v, ok := m[useKey.(float64)]; ok {
      return v, true
    }
    return nil, false
  case I16:
    m := p.i16
    if m == nil {
      return nil, false
    }
    if v, ok := m[useKey.(int16)]; ok {
      return v, true
    }
    return nil, false
  case I32:
    m := p.i32
    if m == nil {
      return nil, false
    }
    if v, ok := m[useKey.(int32)]; ok {
      return v, true
    }
    return nil, false
  case I64:
    m := p.i64
    if m == nil {
      return nil, false
    }
    if v, ok := m[useKey.(int64)]; ok {
      return v, true
    }
    return nil, false
  case STRING, UTF8, UTF16:
    // TODO(pomack) properly handle ENUM
    m := p.s
    if m == nil {
      return nil, false
    }
    if v, ok := m[useKey.(string)]; ok {
      return v, true
    }
    return nil, false
  case STRUCT:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      structkey, ok := k.(TStruct)
      if ok {
        if structkey.Equals(useKey.(TStruct)) {
          return e.Value(), true
        }
        continue
      }
      if reflect.DeepEqual(useKey, k) {
        return e.Value(), true
      }
    }
    return nil, false
  case MAP:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      mapkey, ok := k.(TMap)
      if ok {
        if mapkey.Equals(useKey.(TMap)) {
          return e.Value(), true
        }
        continue
      }
      if reflect.DeepEqual(useKey, k) {
        return e.Value(), true
      }
    }
    return nil, false
  case SET:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      setkey, ok := k.(TSet)
      if ok {
        if setkey.Equals(useKey.(TSet)) {
          return e.Value(), true
        }
        continue
      }
      if reflect.DeepEqual(useKey, k) {
        return e.Value(), true
      }
    }
    return nil, false
  case LIST:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      listkey, ok := k.(TList)
      if ok {
        if listkey.Equals(useKey.(TList)) {
          return e.Value(), true
        }
        continue
      }
      if reflect.DeepEqual(useKey, k) {
        return e.Value(), true
      }
    }
    return nil, false
  default:
    panic("Invalid Thrift element type")
  }
  return nil, false
}


func (p *tMap) Set(key, value interface{}) {
  if p.KeyType() == STOP || p.KeyType() == VOID {
    p.keyType = TypeFromValue(key)
  }
  coercedKey, ok := p.KeyType().CoerceData(key)
  if !ok {
    return
  }
  if p.ValueType() == STOP || p.ValueType() == VOID {
    p.valueType = TypeFromValue(value)
  }
  coercedValue, ok := p.ValueType().CoerceData(value)
  if !ok {
    return
  }
  newElem := NewTMapElem(coercedKey, coercedValue)
  if !p.KeyType().IsBaseType() {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      k := elem.Value.(TMapElem).Key()
      if cmp, ok := p.KeyType().Compare(coercedKey, k); ok && cmp >= 0 {
        if cmp == 0 {
          p.l.InsertAfter(newElem, elem)
          p.l.Remove(elem)
          return
        }
        p.l.InsertBefore(newElem, elem)
        return
      }
    }
    p.l.PushBack(newElem)
    return
  }
  if key == nil {
    return
  }
  switch p.KeyType() {
  case STOP, VOID:
    // if here, then we don't have a key type yet and key is not nil
    // so this is pretty much an empty map
    return
  case BOOL:
    if p.b == nil {
      p.b = make(map[bool]interface{})
    }
    b := coercedKey.(bool)
    p.b[b] = value
  case BYTE:
    if p.i08 == nil {
      p.i08 = make(map[byte]interface{})
    }
    b := coercedKey.(byte)
    p.i08[b] = value
  case DOUBLE:
    if p.f64 == nil {
      p.f64 = make(map[float64]interface{})
    }
    b := coercedKey.(float64)
    p.f64[b] = value
  case I16:
    if p.i16 == nil {
      p.i16 = make(map[int16]interface{})
    }
    b := coercedKey.(int16)
    p.i16[b] = value
  case I32:
    if p.i32 == nil {
      p.i32 = make(map[int32]interface{})
    }
    b := coercedKey.(int32)
    p.i32[b] = value
  case I64:
    if p.i64 == nil {
      p.i64 = make(map[int64]interface{})
    }
    b := coercedKey.(int64)
    p.i64[b] = value
  case STRING, UTF8, UTF16:
    if p.s == nil {
      p.s = make(map[string]interface{})
    }
    b := coercedKey.(string)
    p.s[b] = value
  case STRUCT, MAP, SET, LIST:
    panic("Should never be here")
  default:
    panic("Should never be here")
  }
}

func (p *tMap) Contains(key interface{}) bool {
  coercedKey, ok := p.KeyType().CoerceData(key)
  if !ok {
    return false
  }
  if coercedKey == nil {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      k := elem.Value.(TMapElem).Key()
      if k == nil {
        return true
      }
    }
    return false
  }
  if !ok {
    return false
  }
  switch p.KeyType() {
  case STOP:
    // if here, then we don't have a key type yet and key is not nil
    // so this is pretty much an empty map
    return false
  case VOID:
    // if here, then we don't have a key type yet and key is not nil
    // so this is pretty much an empty map
    return false
  case BOOL:
    m := p.b
    if m == nil {
      return false
    }
    _, ok := m[coercedKey.(bool)]
    return ok
  case BYTE:
    m := p.i08
    _, ok := m[coercedKey.(byte)]
    return ok
  case DOUBLE:
    m := p.f64
    if m == nil {
      return false
    }
    _, ok := m[coercedKey.(float64)]
    return ok
  case I16:
    m := p.i16
    if m == nil {
      return false
    }
    _, ok := m[coercedKey.(int16)]
    return ok
  case I32:
    m := p.i32
    if m == nil {
      return false
    }
    _, ok := m[coercedKey.(int32)]
    return ok
  case I64:
    m := p.i64
    if m == nil {
      return false
    }
    _, ok := m[coercedKey.(int64)]
    return ok
  case STRING, UTF8, UTF16:
    // TODO(pomack) properly handle ENUM
    m := p.s
    if m == nil {
      return false
    }
    _, ok := m[coercedKey.(string)]
    return ok
  case STRUCT:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      structkey, ok := k.(TStruct)
      if ok {
        if structkey.Equals(coercedKey.(TStruct)) {
          return true
        }
        continue
      }
      if reflect.DeepEqual(coercedKey, k) {
        return true
      }
    }
    return false
  case MAP:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      mapkey, ok := k.(TMap)
      if ok {
        if mapkey.Equals(coercedKey.(TMap)) {
          return true
        }
        continue
      }
    }
    return false
  case SET:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      setkey, ok := k.(TSet)
      if ok {
        if setkey.Equals(coercedKey.(TSet)) {
          return true
        }
        continue
      }
    }
    return false
  case LIST:
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      e := elem.Value.(TMapElem)
      k := e.Key()
      if k == nil {
        continue
      }
      listkey, ok := k.(TList)
      if ok {
        if listkey.Equals(coercedKey.(TList)) {
          return true
        }
        continue
      }
    }
    return false
  default:
    panic("Invalid Thrift element type")
  }
  return false
}

// Iterate over all elements; driver for range
func (p *tMap) iterate(c chan<- TMapElem) {
  switch p.KeyType() {
  case STOP, VOID:
    close(c)
  case BOOL:
    for k, v := range p.b {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case BYTE:
    for k, v := range p.i08 {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case I16:
    for k, v := range p.i16 {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case I32:
    for k, v := range p.i32 {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case I64:
    for k, v := range p.i64 {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case DOUBLE:
    for k, v := range p.f64 {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case STRING, UTF8, UTF16:
    for k, v := range p.s {
      c <- NewTMapElem(k, v)
    }
    close(c)
  case STRUCT:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem)
    }
    close(c)
  case LIST:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem)
    }
    close(c)
  case SET:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem)
    }
    close(c)
  default:
    panic("Invalid Thrift type")
  }
}

// Channel iterator for range.
func (p *tMap) Iter() <-chan TMapElem {
  c := make(chan TMapElem)
  go p.iterate(c)
  return c
}

// Iterate over all keys; driver for range
func (p *tMap) iterateKeys(c chan<- interface{}) {
  switch p.KeyType() {
  case STOP, VOID:
    close(c)
  case BOOL:
    for k, _ := range p.b {
      c <- k
    }
    close(c)
  case BYTE:
    for k, _ := range p.i08 {
      c <- k
    }
    close(c)
  case I16:
    for k, _ := range p.i16 {
      c <- k
    }
    close(c)
  case I32:
    for k, _ := range p.i32 {
      c <- k
    }
    close(c)
  case I64:
    for k, _ := range p.i64 {
      c <- k
    }
    close(c)
  case DOUBLE:
    for k, _ := range p.f64 {
      c <- k
    }
    close(c)
  case STRING, UTF8, UTF16:
    for k, _ := range p.s {
      c <- k
    }
    close(c)
  case STRUCT:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem).Key()
    }
    close(c)
  case LIST:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem).Key()
    }
    close(c)
  case SET:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem).Key()
    }
    close(c)
  default:
    panic("Invalid Thrift type")
  }
}

func (p *tMap) KeyIter() <-chan interface{} {
  c := make(chan interface{})
  go p.iterateKeys(c)
  return c
}

// Iterate over all values; driver for range
func (p *tMap) iterateValues(c chan<- interface{}) {
  switch p.KeyType() {
  case STOP, VOID:
    close(c)
  case BOOL:
    for _, v := range p.b {
      c <- v
    }
    close(c)
  case BYTE:
    for _, v := range p.i08 {
      c <- v
    }
    close(c)
  case I16:
    for _, v := range p.i16 {
      c <- v
    }
    close(c)
  case I32:
    for _, v := range p.i32 {
      c <- v
    }
    close(c)
  case I64:
    for _, v := range p.i64 {
      c <- v
    }
    close(c)
  case DOUBLE:
    for _, v := range p.f64 {
      c <- v
    }
    close(c)
  case STRING, UTF8, UTF16:
    for _, v := range p.s {
      c <- v
    }
    close(c)
  case STRUCT:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem).Value()
    }
    close(c)
  case LIST:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem).Value()
    }
    close(c)
  case SET:
    for v := p.l.Front(); v != nil; v = v.Next() {
      c <- v.Value.(TMapElem).Value()
    }
    close(c)
  default:
    panic("Invalid Thrift type")
  }
}

func (p *tMap) ValueIter() <-chan interface{} {
  c := make(chan interface{})
  go p.iterateValues(c)
  return c
}


func (p *tMap) Less(other interface{}) bool {
  cmp, ok := p.CompareTo(other)
  return ok && cmp > 0
}

func (p *tMap) Equals(other interface{}) bool {
  c, cok := p.CompareTo(other)
  return cok && c == 0
}

func (p *tMap) CompareTo(other interface{}) (int, bool) {
  return TType(MAP).Compare(p, other)
}

func (p *tMap) Keys() []interface{} {
  size := p.Len()
  values := make([]interface{}, size, size)
  i := 0
  for k := range p.KeyIter() {
    values[i] = k
    i++
  }
  return values
}

func (p *tMap) Values() []interface{} {
  size := p.Len()
  values := make([]interface{}, size, size)
  i := 0
  for v := range p.ValueIter() {
    values[i] = v
    i++
  }
  return values
}
