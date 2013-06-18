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
	"io"
)

// Thrift Transport exception
type TTransportException interface {
	TException
	TypeId() int
}

const (
	UNKNOWN_TRANSPORT_EXCEPTION = 0
	NOT_OPEN                    = 1
	ALREADY_OPEN                = 2
	TIMED_OUT                   = 3
	END_OF_FILE                 = 4
)

type tTransportException struct {
	typeId  int
	message string
}

func (p *tTransportException) TypeId() int {
	return p.typeId
}

func (p *tTransportException) Error() string {
	return p.message
}

func NewTTransportException(t int, m string) TTransportException {
	return &tTransportException{typeId: t, message: m}
}

func NewTTransportExceptionFromError(e error) TTransportException {
	if e == nil {
		return nil
	}
	if t, ok := e.(TTransportException); ok {
		return t
	}
	if e == io.EOF {
		return NewTTransportException(END_OF_FILE, e.Error())
	}
	return NewTTransportException(UNKNOWN_TRANSPORT_EXCEPTION, e.Error())
}
