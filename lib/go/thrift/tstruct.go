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

/**
 * Helper class that encapsulates struct metadata.
 *
 */
type TStruct interface {
  TFieldContainer
  TStructName() string
  ThriftName() string
  TStructFields() TFieldContainer
  String() string
  AttributeFromFieldId(fieldId int) interface{}
  AttributeFromFieldName(fieldName string) interface{}
}

type tStruct struct {
  TFieldContainer
  name string
}

func NewTStructEmpty(name string) TStruct {
  return &tStruct{
    name:            name,
    TFieldContainer: NewTFieldContainer(make([]TField, 0, 0)),
  }
}

func NewTStruct(name string, fields []TField) TStruct {
  return &tStruct{
    name:            name,
    TFieldContainer: NewTFieldContainer(fields),
  }
}

func (p *tStruct) TStructName() string {
  return p.name
}

func (p *tStruct) ThriftName() string {
  return p.name
}

func (p *tStruct) TStructFields() TFieldContainer {
  return p.TFieldContainer
}

func (p *tStruct) String() string {
  return p.name
}

func (p *tStruct) Equals(other interface{}) bool {
  cmp, ok := p.CompareTo(other)
  return ok && cmp == 0
}

func (p *tStruct) CompareTo(other interface{}) (int, bool) {
  return TType(STRUCT).Compare(p, other)
}

func (p *tStruct) AttributeFromFieldId(fieldId int) interface{} {
  return nil
}

func (p *tStruct) AttributeFromFieldName(fieldName string) interface{} {
  return p.AttributeFromFieldId(p.FieldIdFromFieldName(fieldName))
}


var ANONYMOUS_STRUCT TStruct

func init() {
  ANONYMOUS_STRUCT = NewTStructEmpty("")
}
