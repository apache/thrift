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
)

/**
 * Helper class that encapsulates list metadata.
 *
 */
type TList interface {
  TContainer
  ElemType() TType
  At(i int) interface{}
  Set(i int, data interface{})
  Push(data interface{})
  Pop() interface{}
  Swap(i, j int)
  Insert(i int, data interface{})
  Delete(i int)
  Less(i, j int) bool
  Iter() <-chan interface{}
}

type tList struct {
  elemType TType
  l        []interface{}
}

func NewTList(t TType, s int) TList {
  //this automatically grows using append so 0
  v := make([]interface{}, 0)
  return &tList{elemType: t, l: v}
}

func NewTListDefault() TList {
  return &tList{elemType: TType(STOP), l: make([]interface{}, 0)}
}

func (p *tList) ElemType() TType {
  return p.elemType
}

func (p *tList) Len() int {
  return len(p.l)
}

func (p *tList) At(i int) interface{} {
  return p.l[i]
}

func (p *tList) Set(i int, data interface{}) {
  if p.elemType.IsEmptyType() {
    p.elemType = TypeFromValue(data)
  }
  if data, ok := p.elemType.CoerceData(data); ok {
    if len(p.l) >= i {
      p.l[i] = data
    } else {
      p.l = append(p.l, data)
    }
  }
}

func (p *tList) Push(data interface{}) {
  if p.elemType.IsEmptyType() {
    p.elemType = TypeFromValue(data)
  }
  data, ok := p.elemType.CoerceData(data)
  if ok {
    p.l = append(p.l, data)
  }
}

func (p *tList) Pop() interface{} {
  var x interface{}
  x, p.l = p.l[len(p.l)-1], p.l[:len(p.l)-1]
  return x
}

func (p *tList) Swap(i, j int) {
  x, y := p.l[i], p.l[j]
  p.l[i] = y
  p.l[j] = x
}

func (p *tList) Insert(i int, data interface{}) {
  newl := make([]interface{}, 1)
  newl[0] = data
  p.l = append(p.l[:i], append(newl, p.l[i:]...)...)
}

func (p *tList) Delete(i int) {
  p.l = append(p.l[:i], p.l[i+1:]...)
}

func (p *tList) Contains(data interface{}) bool {
  return p.indexOf(data) >= 0
}

func (p *tList) Less(i, j int) bool {
  _, less := p.elemType.Compare(p.l[i], p.l[j])
  return less
}

func (p *tList) Iter() <-chan interface{} {
  c := make(chan interface{})
  go p.iterate(c)
  return c
}

func (p *tList) iterate(c chan<- interface{}) {
  for _, elem := range p.l {
    c <- elem
  }
  close(c)
}

func (p *tList) indexOf(data interface{}) int {
  if data == nil {
    size := len(p.l)
    for i := 0; i < size; i++ {
      if p.l[i] == nil {
        return i
      }
    }
    return -1
  }
  data, ok := p.elemType.CoerceData(data)
  if data == nil || !ok {
    return -1
  }
  size := len(p.l)
  if p.elemType == BINARY {
    for i := 0; i < size; i++ {
      if bytes.Compare(data.([]byte), p.l[i].([]byte)) == 0 {
        return i
      }
    }
    return -1
  }
  if p.elemType.IsBaseType() || p.elemType.IsEnum() {
    for i := 0; i < size; i++ {
      if data == p.l[i] {
        return i
      }
    }
    return -1
  }
  if cmp, ok := data.(EqualsOtherInterface); ok {
    for i := 0; i < size; i++ {
      if cmp.Equals(p.l[i]) {
        return i
      }
    }
    return -1
  }
  switch p.elemType {
  case MAP:
    if cmp, ok := data.(EqualsMap); ok {
      for i := 0; i < size; i++ {
        v := p.l[i]
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TMap)) {
          return i
        }
      }
      return -1
    }
  case SET:
    if cmp, ok := data.(EqualsSet); ok {
      for i := 0; i < size; i++ {
        v := p.l[i]
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TSet)) {
          return i
        }
      }
      return -1
    }
  case LIST:
    if cmp, ok := data.(EqualsList); ok {
      for i := 0; i < size; i++ {
        v := p.l[i]
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TList)) {
          return i
        }
      }
      return -1
    }
  case STRUCT:
    if cmp, ok := data.(EqualsStruct); ok {
      for i := 0; i < size; i++ {
        v := p.l[i]
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TStruct)) {
          return i
        }
      }
      return -1
    }
  }
  return -1
}

func (p *tList) Equals(other interface{}) bool {
  c, cok := p.CompareTo(other)
  return cok && c == 0
}

func (p *tList) CompareTo(other interface{}) (int, bool) {
  return TType(LIST).Compare(p, other)
}
