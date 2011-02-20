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
)

/**
 * Helper class that encapsulates set metadata.
 *
 */
type TSet interface {
  TContainer
  ElemType() TType
  Add(data interface{})
  Remove(data interface{})
  Less(other interface{}) bool
  Front() *list.Element
  Back() *list.Element
  Values() []interface{}
}

type tSet struct {
  elemType TType
  size     int
  l        *list.List
}

func NewTSet(t TType, s int) TSet {
  return &tSet{elemType: t, size: s, l: list.New()}
}

func NewTSetDefault() TSet {
  return NewTSet(STOP, 0)
}

func (p *tSet) ElemType() TType {
  return p.elemType
}

func (p *tSet) Front() *list.Element {
  return p.l.Front()
}

func (p *tSet) Back() *list.Element {
  return p.l.Back()
}

func (p *tSet) Len() int {
  if p.l.Len() != 0 {
    return p.l.Len()
  }
  return p.size
}

func (p *tSet) Contains(data interface{}) bool {
  return p.find(data) != nil
}

func (p *tSet) Add(other interface{}) {
  if data, ok := p.elemType.CoerceData(other); ok {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      if cmp, ok := p.elemType.Compare(data, elem.Value); ok && cmp >= 0 {
        if cmp > 0 {
          p.l.InsertBefore(data, elem)
        }
        return
      }
    }
  }
}

func (p *tSet) Remove(data interface{}) {
  elem := p.find(data)
  if elem != nil {
    p.l.Remove(elem)
  }
}

func (p *tSet) Less(other interface{}) bool {
  cmp, ok := p.CompareTo(other)
  return ok && cmp > 0
}

func (p *tSet) Equals(other interface{}) bool {
  c, cok := p.CompareTo(other)
  return cok && c == 0
}

func (p *tSet) CompareTo(other interface{}) (int, bool) {
  return TType(SET).Compare(p, other)
}

func (p *tSet) find(data interface{}) *list.Element {
  if data == nil {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      if elem.Value == nil {
        return elem
      }
    }
    return nil
  }
  data, ok := p.elemType.CoerceData(data)
  if data == nil || !ok {
    return nil
  }
  if p.elemType.IsBaseType() || p.elemType.IsEnum() {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      if data == elem.Value {
        return elem
      }
    }
    return nil
  }
  if cmp, ok := data.(EqualsOtherInterface); ok {
    for elem := p.l.Front(); elem != nil; elem = elem.Next() {
      if cmp.Equals(elem.Value) {
        return elem
      }
    }
    return nil
  }
  switch p.elemType {
  case MAP:
    if cmp, ok := data.(EqualsMap); ok {
      for elem := p.l.Front(); elem != nil; elem = elem.Next() {
        v := elem.Value
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TMap)) {
          return elem
        }
      }
      return nil
    }
  case SET:
    if cmp, ok := data.(EqualsSet); ok {
      for elem := p.l.Front(); elem != nil; elem = elem.Next() {
        v := elem.Value
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TSet)) {
          return elem
        }
      }
      return nil
    }
  case LIST:
    if cmp, ok := data.(EqualsList); ok {
      for elem := p.l.Front(); elem != nil; elem = elem.Next() {
        v := elem.Value
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TList)) {
          return elem
        }
      }
      return nil
    }
  case STRUCT:
    if cmp, ok := data.(EqualsStruct); ok {
      for elem := p.l.Front(); elem != nil; elem = elem.Next() {
        v := elem.Value
        if v == nil {
          continue
        }
        if cmp.Equals(v.(TStruct)) {
          return elem
        }
      }
      return nil
    }
  }
  return nil
}

func (p *tSet) Values() []interface{} {
  size := p.l.Len()
  values := make([]interface{}, size, size)
  i := 0
  for v := p.l.Front(); v != nil; v = v.Next() {
    values[i] = v.Value
    i++
  }
  return values
}
