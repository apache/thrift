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
  "os"
)

/**
 * Protocol exceptions.
 *
 */
type TProtocolException interface {
  TException
  TypeId() int
}

const (
  UNKNOWN_PROTOCOL_EXCEPTION = 0
  INVALID_DATA               = 1
  NEGATIVE_SIZE              = 2
  SIZE_LIMIT                 = 3
  BAD_VERSION                = 4
  NOT_IMPLEMENTED            = 5
)

type tProtocolException struct {
  typeId  int
  message string
}

func (p *tProtocolException) TypeId() int {
  return p.typeId
}

func (p *tProtocolException) String() string {
  return p.message
}

func NewTProtocolExceptionDefault() TProtocolException {
  return NewTProtocolExceptionDefaultType(UNKNOWN_PROTOCOL_EXCEPTION)
}

func NewTProtocolExceptionDefaultType(t int) TProtocolException {
  return NewTProtocolException(t, "")
}

func NewTProtocolExceptionDefaultString(m string) TProtocolException {
  return NewTProtocolException(UNKNOWN_PROTOCOL_EXCEPTION, m)
}

func NewTProtocolException(t int, m string) TProtocolException {
  return &tProtocolException{typeId: t, message: m}
}

func NewTProtocolExceptionReadField(fieldId int, fieldName string, structName string, e TProtocolException) TProtocolException {
  t := e.TypeId()
  if t == UNKNOWN_PROTOCOL_EXCEPTION {
    t = INVALID_DATA
  }
  return NewTProtocolException(t, "Unable to read field "+string(fieldId)+" ("+fieldName+") in "+structName+" due to: "+e.String())
}

func NewTProtocolExceptionWriteField(fieldId int, fieldName string, structName string, e TProtocolException) TProtocolException {
  t := e.TypeId()
  if t == UNKNOWN_PROTOCOL_EXCEPTION {
    t = INVALID_DATA
  }
  return NewTProtocolException(t, "Unable to write field "+string(fieldId)+" ("+fieldName+") in "+structName+" due to: "+e.String())
}

func NewTProtocolExceptionReadStruct(structName string, e TProtocolException) TProtocolException {
  t := e.TypeId()
  if t == UNKNOWN_PROTOCOL_EXCEPTION {
    t = INVALID_DATA
  }
  return NewTProtocolException(t, "Unable to read struct "+structName+" due to: "+e.String())
}

func NewTProtocolExceptionWriteStruct(structName string, e TProtocolException) TProtocolException {
  t := e.TypeId()
  if t == UNKNOWN_PROTOCOL_EXCEPTION {
    t = INVALID_DATA
  }
  return NewTProtocolException(t, "Unable to write struct "+structName+" due to: "+e.String())
}

func NewTProtocolExceptionFromOsError(e os.Error) TProtocolException {
  if e == nil {
    return nil
  }
  if t, ok := e.(TProtocolException); ok {
    return t
  }
  if te, ok := e.(TTransportException); ok {
    return NewTProtocolExceptionFromTransportException(te)
  }
  if _, ok := e.(base64.CorruptInputError); ok {
    return NewTProtocolException(INVALID_DATA, e.String())
  }
  return NewTProtocolExceptionDefaultString(e.String())
}

func NewTProtocolExceptionFromTransportException(e TTransportException) TProtocolException {
  if e == nil {
    return nil
  }
  if t, ok := e.(TProtocolException); ok {
    return t
  }
  return NewTProtocolExceptionDefaultString(e.String())
}
