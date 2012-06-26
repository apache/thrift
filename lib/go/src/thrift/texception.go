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

import ()

/**
 * Generic exception class for Thrift.
 *
 */

type TException interface {
  Error() string
}

type tException struct {
  message string
}

func (p *tException) Error() string {
  return p.message
}

func NewTException(m string) TException {
  return &tException{message: m}
}

func NewTExceptionFromOsError(e error) TException {
  if e == nil {
    return nil
  }
  t, ok := e.(TException)
  if ok {
    return t
  }
  return NewTException(e.Error())
}
