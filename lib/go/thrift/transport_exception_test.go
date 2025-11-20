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
	"errors"
	"fmt"
	"io"
	"testing"
)

type timeout struct{ timedout bool }

func (t *timeout) Timeout() bool {
	return t.timedout
}

func (t *timeout) Error() string {
	return fmt.Sprintf("Timeout: %v", t.timedout)
}

func TestTExceptionTimeout(t *testing.T) {
	timeout := &timeout{true}
	exception := NewTTransportExceptionFromError(timeout)
	if timeout.Error() != exception.Error() {
		t.Errorf("Error did not match: expected %q, got %q", timeout.Error(), exception.Error())
	}

	if exception.TypeId() != TIMED_OUT {
		t.Errorf("TypeId was not TIMED_OUT: expected %v, got %v", TIMED_OUT, exception.TypeId())
	}

	if unwrapped := errors.Unwrap(exception); unwrapped != timeout {
		t.Errorf("Unwrapped exception did not match: expected %v, got %v", timeout, unwrapped)
	}
}

func TestTExceptionEOF(t *testing.T) {
	exception := NewTTransportExceptionFromError(io.EOF)
	if io.EOF.Error() != exception.Error() {
		t.Errorf("Error did not match: expected %q, got %q", io.EOF.Error(), exception.Error())
	}

	if exception.TypeId() != END_OF_FILE {
		t.Errorf("TypeId was not END_OF_FILE: expected %v, got %v", END_OF_FILE, exception.TypeId())
	}

	if unwrapped := errors.Unwrap(exception); unwrapped != io.EOF {
		t.Errorf("Unwrapped exception did not match: expected %v, got %v", io.EOF, unwrapped)
	}
}

func TestIsTimeoutError(t *testing.T) {
	te := &timeout{true}
	if !isTimeoutError(te) {
		t.Error("isTimeoutError expected true, got false")
	}
	e := NewTTransportExceptionFromError(te)
	if !isTimeoutError(e) {
		t.Error("isTimeoutError on wrapped TTransportException expected true, got false")
	}

	te = &timeout{false}
	if isTimeoutError(te) {
		t.Error("isTimeoutError expected false, got true")
	}
	e = NewTTransportExceptionFromError(te)
	if isTimeoutError(e) {
		t.Error("isTimeoutError on wrapped TTransportException expected false, got true")
	}

	err := errors.New("foo")
	if isTimeoutError(err) {
		t.Error("isTimeoutError expected false, got true")
	}
	e = NewTTransportExceptionFromError(err)
	if isTimeoutError(e) {
		t.Error("isTimeoutError on wrapped TTransportException expected false, got true")
	}
}
