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
  l        *vector.Vector
}

func NewTList(t TType, s int) TList {
  var v vector.Vector
  return &tList{elemType: t, l: v.Resize(s, s)}
}

func NewTListDefault() TList {
  var v vector.Vector
  return &tList{elemType: TType(STOP), l: &v}
}

func (p *tList) ElemType() TType {
  return p.elemType
}

func (p *tList) Len() int {
  return p.l.Len()
}

func (p *tList) At(i int) interface{} {
  return p.l.At(i)
}

func (p *tList) Set(i int, data interface{}) {
  if p.elemType.IsEmptyType() {
    p.elemType = TypeFromValue(data)
  }
  if data, ok := p.elemType.CoerceData(data); ok {
    p.l.Set(i, data)
  }
}

func (p *tList) Push(data interface{}) {
  if p.elemType.IsEmptyType() {
    p.elemType = TypeFromValue(data)
  }
  data, ok := p.elemType.CoerceData(data)
  if ok {
    p.l.Push(data)
  }
}

func (p *tList) Pop() interface{} {
  return p.l.Pop()
}

func (p *tList) Swap(i, j int) {
  p.l.Swap(i, j)
}

func (p *tList) Insert(i int, data interface{}) {
  p.l.Insert(i, data)
}

func (p *tList) Delete(i int) {
  p.l.Delete(i)
}

func (p *tList) Contains(data interface{}) bool {
  return p.indexOf(data) >= 0
}

func (p *tList) Less(i, j int) bool {
  return p.l.Less(i, j)
}

func (p *tList) Iter() <-chan interface{} {
  c := make(chan interface{})
  go p.iterate(c)
  return c
}

func (p *tList) iterate(c chan<- interface{}) {
  for _, elem := range *p.l {
    c <- elem
  }
  close(c)
}

func (p *tList) indexOf(data interface{}) int {
  if data == nil {
    size := p.l.Len()
    for i := 0; i < size; i++ {
      if p.l.At(i) == nil {
        return i
      }
    }
    return -1
  }
  data, ok := p.elemType.CoerceData(data)
  if data == nil || !ok {
    return -1
  }
  size := p.l.Len()
  if p.elemType.IsBaseType() || p.elemType.IsEnum() {
    for i := 0; i < size; i++ {
      if data == p.l.At(i) {
        return i
      }
    }
    return -1
  }
  if cmp, ok := data.(EqualsOtherInterface); ok {
    for i := 0; i < size; i++ {
      if cmp.Equals(p.l.At(i)) {
        return i
      }
    }
    return -1
  }
  switch p.elemType {
  case MAP:
    if cmp, ok := data.(EqualsMap); ok {
      for i := 0; i < size; i++ {
        v := p.l.At(i)
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
        v := p.l.At(i)
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
        v := p.l.At(i)
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
        v := p.l.At(i)
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
