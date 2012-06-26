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
  "sort"
)

/**
 * Helper class that encapsulates field metadata.
 *
 */
type TField interface {
  Name() string
  TypeId() TType
  Id() int
  String() string
  CompareTo(other interface{}) (int, bool)
  Equals(other interface{}) bool
}

type tField struct {
  name   string
  typeId TType
  id     int
}

func NewTFieldDefault() TField {
  return ANONYMOUS_FIELD
}

func NewTField(n string, t TType, i int) TField {
  return &tField{name: n, typeId: t, id: i}
}

func (p *tField) Name() string {
  if p == nil {
    return ""
  }
  return p.name
}

func (p *tField) TypeId() TType {
  if p == nil {
    return TType(VOID)
  }
  return p.typeId
}

func (p *tField) Id() int {
  if p == nil {
    return -1
  }
  return p.id
}

func (p *tField) String() string {
  if p == nil {
    return "<nil>"
  }
  return "<TField name:'" + p.name + "' type:" + string(p.typeId) + " field-id:" + string(p.id) + ">"
}

func (p *tField) CompareTo(other interface{}) (int, bool) {
  if other == nil {
    return 1, true
  }
  if data, ok := other.(TField); ok {
    if p.Id() != data.Id() {
      return CompareInt(p.Id(), data.Id()), true
    }
    if p.TypeId() != data.TypeId() {
      return CompareByte(byte(p.TypeId()), byte(data.TypeId())), true
    }
    return CompareString(p.Name(), data.Name()), true
  }
  return 0, false
}

func (p *tField) Equals(other interface{}) bool {
  if p == nil {
    return other == nil
  }
  if other == nil {
    return false
  }
  if data, ok := other.(TField); ok {
    return p.TypeId() == data.TypeId() && p.Id() == data.Id()
  }
  return false
}

var ANONYMOUS_FIELD TField

type tFieldArray []TField

func (p tFieldArray) Len() int {
  return len(p)
}

func (p tFieldArray) Less(i, j int) bool {
  return p[i].Id() < p[j].Id()
}

func (p tFieldArray) Swap(i, j int) {
  p[i], p[j] = p[j], p[i]
}

type TFieldContainer interface {
  TContainer
  FieldNameFromFieldId(id int) string
  FieldIdFromFieldName(name string) int
  FieldFromFieldId(id int) TField
  FieldFromFieldName(name string) TField
  At(i int) TField
  Iter() <-chan TField
}

type tFieldContainer struct {
  fields         []TField
  nameToFieldMap map[string]TField
  idToFieldMap   map[int]TField
}

func NewTFieldContainer(fields []TField) TFieldContainer {
  sortedFields := make([]TField, len(fields))
  nameToFieldMap := make(map[string]TField)
  idToFieldMap := make(map[int]TField)
  for i, field := range fields {
    sortedFields[i] = field
    idToFieldMap[field.Id()] = field
    if field.Name() != "" {
      nameToFieldMap[field.Name()] = field
    }
  }
  sort.Sort(tFieldArray(sortedFields))
  return &tFieldContainer{
    fields:         fields,
    nameToFieldMap: nameToFieldMap,
    idToFieldMap:   idToFieldMap,
  }
}

func (p *tFieldContainer) FieldNameFromFieldId(id int) string {
  if field, ok := p.idToFieldMap[id]; ok {
    return field.Name()
  }
  return ""
}

func (p *tFieldContainer) FieldIdFromFieldName(name string) int {
  if field, ok := p.nameToFieldMap[name]; ok {
    return field.Id()
  }
  return -1
}

func (p *tFieldContainer) FieldFromFieldId(id int) TField {
  if field, ok := p.idToFieldMap[id]; ok {
    return field
  }
  return ANONYMOUS_FIELD
}

func (p *tFieldContainer) FieldFromFieldName(name string) TField {
  if field, ok := p.nameToFieldMap[name]; ok {
    return field
  }
  return ANONYMOUS_FIELD
}

func (p *tFieldContainer) Len() int {
  return len(p.fields)
}

func (p *tFieldContainer) At(i int) TField {
  return p.FieldFromFieldId(i)
}

func (p *tFieldContainer) Contains(data interface{}) bool {
  if i, ok := data.(int); ok {
    for _, field := range p.fields {
      if field.Id() == i {
        return true
      }
    }
  } else if i, ok := data.(int16); ok {
    for _, field := range p.fields {
      if field.Id() == int(i) {
        return true
      }
    }
  } else if s, ok := data.(string); ok {
    for _, field := range p.fields {
      if field.Name() == s {
        return true
      }
    }
  } else if f, ok := data.(TField); ok {
    for _, field := range p.fields {
      if field.Equals(f) {
        return true
      }
    }
  }
  return false
}

func (p *tFieldContainer) Equals(other interface{}) bool {
  if other == nil {
    return false
  }
  if data, ok := other.(TFieldContainer); ok {
    if p.Len() != data.Len() {
      return false
    }
    for _, field := range p.fields {
      if !data.Contains(field) {
        return false
      }
    }
    return true
  }
  return false
}

func (p *tFieldContainer) CompareTo(other interface{}) (int, bool) {
  if other == nil {
    return 1, true
  }
  if data, ok := other.(TFieldContainer); ok {
    cont, ok2 := data.(*tFieldContainer)
    if ok2 && p == cont {
      return 0, true
    }
    if cmp := CompareInt(p.Len(), data.Len()); cmp != 0 {
      return cmp, true
    }
    for _, field := range p.fields {
      if cmp, ok3 := field.CompareTo(data.At(field.Id())); !ok3 || cmp != 0 {
        return cmp, ok3
      }
    }
    return 0, true
  }
  return 0, false
}

func (p *tFieldContainer) Iter() <-chan TField {
  c := make(chan TField)
  go p.iterate(c)
  return c
}

func (p *tFieldContainer) iterate(c chan<- TField) {
  for _, v := range p.fields {
    c <- v
  }
  close(c)
}

func init() {
  ANONYMOUS_FIELD = NewTField("", STOP, 0)
}
