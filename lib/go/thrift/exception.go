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
	"reflect"
)

// Generic Thrift exception
type TException interface {
	error

	TExceptionType() TExceptionType
}

// Prepends additional information to an error without losing the Thrift exception interface
func PrependError(prepend string, err error) error {
	msg := prepend + err.Error()

	var te TException
	if errors.As(err, &te) {
		switch te.TExceptionType() {
		case TExceptionTypeTransport:
			if t, ok := err.(TTransportException); ok {
				return prependTTransportException(prepend, t)
			}
		case TExceptionTypeProtocol:
			if t, ok := err.(TProtocolException); ok {
				return prependTProtocolException(prepend, t)
			}
		case TExceptionTypeApplication:
			var t TApplicationException
			if errors.As(err, &t) {
				return NewTApplicationException(t.TypeId(), msg)
			}
		}

		return wrappedTException{
			err:            err,
			msg:            msg,
			tExceptionType: te.TExceptionType(),
		}
	}

	return errors.New(msg)
}

// TExceptionType is an enum type to categorize different "subclasses" of TExceptions.
type TExceptionType byte

// TExceptionType values
const (
	TExceptionTypeUnknown     TExceptionType = iota
	TExceptionTypeCompiled                   // TExceptions defined in thrift files and generated by thrift compiler
	TExceptionTypeApplication                // TApplicationExceptions
	TExceptionTypeProtocol                   // TProtocolExceptions
	TExceptionTypeTransport                  // TTransportExceptions
)

// WrapTException wraps an error into TException.
//
// If err is nil or already TException, it's returned as-is.
// Otherwise it will be wraped into TException with TExceptionType() returning
// TExceptionTypeUnknown, and Unwrap() returning the original error.
func WrapTException(err error) TException {
	if err == nil {
		return nil
	}

	if te, ok := err.(TException); ok {
		return te
	}

	return wrappedTException{
		err:            err,
		msg:            err.Error(),
		tExceptionType: TExceptionTypeUnknown,
	}
}

type wrappedTException struct {
	err            error
	msg            string
	tExceptionType TExceptionType
}

func (w wrappedTException) Error() string {
	return w.msg
}

func (w wrappedTException) TExceptionType() TExceptionType {
	return w.tExceptionType
}

func (w wrappedTException) Unwrap() error {
	return w.err
}

var _ TException = wrappedTException{}

// ExtractExceptionFromResult extracts exceptions defined in thrift IDL from
// result TStruct used in TClient.Call.
//
// For a endpoint defined in thrift IDL like this:
//
//	service MyService {
//	  FooResponse foo(1: FooRequest request) throws (
//	    1: Exception1 error1,
//	    2: Exception2 error2,
//	  )
//	}
//
// The thrift compiler generated go code for the result TStruct would be like:
//
//	type MyServiceFooResult struct {
//	  Success *FooResponse `thrift:"success,0" db:"success" json:"success,omitempty"`
//	  Error1 *Exception1 `thrift:"error1,1" db:"error1" json:"error1,omitempty"`
//	  Error2 *Exception2 `thrift:"error2,2" db:"error2" json:"error2,omitempty"`
//	}
//
// And this function extracts the first non-nil exception out of
// *MyServiceFooResult.
func ExtractExceptionFromResult(result TStruct) error {
	v := reflect.Indirect(reflect.ValueOf(result))
	if v.Kind() != reflect.Struct {
		return nil
	}
	typ := v.Type()
	for i := range v.NumField() {
		if typ.Field(i).Name == "Success" {
			continue
		}
		field := v.Field(i)
		if field.IsZero() {
			continue
		}
		tExc, ok := field.Interface().(TException)
		if ok && tExc != nil && tExc.TExceptionType() == TExceptionTypeCompiled {
			return tExc
		}
	}
	return nil
}
