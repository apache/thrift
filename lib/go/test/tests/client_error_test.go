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

package tests

import (
	"context"
	"errors"
	"errortest"
	"testing"
	"thrift"

	"github.com/golang/mock/gomock"
)

// TestCase: Comprehensive call and reply workflow in the client.
// Setup mock to fail at a certain position. Return true if position exists otherwise false.
func prepareClientCallReply(protocol *MockTProtocol, failAt int, failWith error) bool {
	// NOTE: here the number 50 is the same as the last number at the end of
	// this function. If more function calls are added in the future, this
	// number needs to be increased accordingly.
	//
	// It's needed for go 1.14+. Before go 1.14 we only call gomock
	// controller's Finish function when this function returns true.
	// Starting from go 1.14 the gomock will take advantage of
	// testing.T.Cleanup interface, which means the Finish function will
	// always be called by t.Cleanup, even if we return false here.
	// As a result, in the case we need to return false by this function,
	// we must return before calling any of the
	// protocol.EXPECT().* functions.
	const lastFailAt = 50
	if failAt > lastFailAt {
		return false
	}

	var err error = nil

	if failAt == 0 {
		err = failWith
	}
	last := protocol.EXPECT().WriteMessageBegin(context.Background(), "testStruct", thrift.CALL, int32(1)).Return(err)
	if failAt == 0 {
		return true
	}
	if failAt == 1 {
		err = failWith
	}
	last = protocol.EXPECT().WriteStructBegin(context.Background(), "testStruct_args").Return(err).After(last)
	if failAt == 1 {
		return true
	}
	if failAt == 2 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldBegin(context.Background(), "thing", thrift.TType(thrift.STRUCT), int16(1)).Return(err).After(last)
	if failAt == 2 {
		return true
	}
	if failAt == 3 {
		err = failWith
	}
	last = protocol.EXPECT().WriteStructBegin(context.Background(), "TestStruct").Return(err).After(last)
	if failAt == 3 {
		return true
	}
	if failAt == 4 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldBegin(context.Background(), "m", thrift.TType(thrift.MAP), int16(1)).Return(err).After(last)
	if failAt == 4 {
		return true
	}
	if failAt == 5 {
		err = failWith
	}
	last = protocol.EXPECT().WriteMapBegin(context.Background(), thrift.TType(thrift.STRING), thrift.TType(thrift.STRING), 0).Return(err).After(last)
	if failAt == 5 {
		return true
	}
	if failAt == 6 {
		err = failWith
	}
	last = protocol.EXPECT().WriteMapEnd(context.Background()).Return(err).After(last)
	if failAt == 6 {
		return true
	}
	if failAt == 7 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 7 {
		return true
	}
	if failAt == 8 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldBegin(context.Background(), "l", thrift.TType(thrift.LIST), int16(2)).Return(err).After(last)
	if failAt == 8 {
		return true
	}
	if failAt == 9 {
		err = failWith
	}
	last = protocol.EXPECT().WriteListBegin(context.Background(), thrift.TType(thrift.STRING), 0).Return(err).After(last)
	if failAt == 9 {
		return true
	}
	if failAt == 10 {
		err = failWith
	}
	last = protocol.EXPECT().WriteListEnd(context.Background()).Return(err).After(last)
	if failAt == 10 {
		return true
	}
	if failAt == 11 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 11 {
		return true
	}
	if failAt == 12 {
		err = failWith
	}

	last = protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.SET), int16(3)).Return(err).After(last)
	if failAt == 12 {
		return true
	}
	if failAt == 13 {
		err = failWith
	}
	last = protocol.EXPECT().WriteSetBegin(context.Background(), thrift.TType(thrift.STRING), 0).Return(err).After(last)
	if failAt == 13 {
		return true
	}
	if failAt == 14 {
		err = failWith
	}
	last = protocol.EXPECT().WriteSetEnd(context.Background()).Return(err).After(last)
	if failAt == 14 {
		return true
	}
	if failAt == 15 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 15 {
		return true
	}
	if failAt == 16 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldBegin(context.Background(), "i", thrift.TType(thrift.I32), int16(4)).Return(err).After(last)
	if failAt == 16 {
		return true
	}
	if failAt == 17 {
		err = failWith
	}
	last = protocol.EXPECT().WriteI32(context.Background(), int32(3)).Return(err).After(last)
	if failAt == 17 {
		return true
	}
	if failAt == 18 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 18 {
		return true
	}
	if failAt == 19 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldStop(context.Background()).Return(err).After(last)
	if failAt == 19 {
		return true
	}
	if failAt == 20 {
		err = failWith
	}
	last = protocol.EXPECT().WriteStructEnd(context.Background()).Return(err).After(last)
	if failAt == 20 {
		return true
	}
	if failAt == 21 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 21 {
		return true
	}
	if failAt == 22 {
		err = failWith
	}
	last = protocol.EXPECT().WriteFieldStop(context.Background()).Return(err).After(last)
	if failAt == 22 {
		return true
	}
	if failAt == 23 {
		err = failWith
	}
	last = protocol.EXPECT().WriteStructEnd(context.Background()).Return(err).After(last)
	if failAt == 23 {
		return true
	}
	if failAt == 24 {
		err = failWith
	}
	last = protocol.EXPECT().WriteMessageEnd(context.Background()).Return(err).After(last)
	if failAt == 24 {
		return true
	}
	if failAt == 25 {
		err = failWith
	}
	last = protocol.EXPECT().Flush(context.Background()).Return(err).After(last)
	if failAt == 25 {
		return true
	}
	if failAt == 26 {
		err = failWith
	}
	last = protocol.EXPECT().ReadMessageBegin(context.Background()).Return("testStruct", thrift.REPLY, int32(1), err).After(last)
	if failAt == 26 {
		return true
	}
	if failAt == 27 {
		err = failWith
	}
	last = protocol.EXPECT().ReadStructBegin(context.Background()).Return("testStruct_args", err).After(last)
	if failAt == 27 {
		return true
	}
	if failAt == 28 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("_", thrift.TType(thrift.STRUCT), int16(0), err).After(last)
	if failAt == 28 {
		return true
	}
	if failAt == 29 {
		err = failWith
	}
	last = protocol.EXPECT().ReadStructBegin(context.Background()).Return("TestStruct", err).After(last)
	if failAt == 29 {
		return true
	}
	if failAt == 30 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("m", thrift.TType(thrift.MAP), int16(1), err).After(last)
	if failAt == 30 {
		return true
	}
	if failAt == 31 {
		err = failWith
	}
	last = protocol.EXPECT().ReadMapBegin(context.Background()).Return(thrift.TType(thrift.STRING), thrift.TType(thrift.STRING), 0, err).After(last)
	if failAt == 31 {
		return true
	}
	if failAt == 32 {
		err = failWith
	}
	last = protocol.EXPECT().ReadMapEnd(context.Background()).Return(err).After(last)
	if failAt == 32 {
		return true
	}
	if failAt == 33 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 33 {
		return true
	}
	if failAt == 34 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("l", thrift.TType(thrift.LIST), int16(2), err).After(last)
	if failAt == 34 {
		return true
	}
	if failAt == 35 {
		err = failWith
	}
	last = protocol.EXPECT().ReadListBegin(context.Background()).Return(thrift.TType(thrift.STRING), 0, err).After(last)
	if failAt == 35 {
		return true
	}
	if failAt == 36 {
		err = failWith
	}
	last = protocol.EXPECT().ReadListEnd(context.Background()).Return(err).After(last)
	if failAt == 36 {
		return true
	}
	if failAt == 37 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 37 {
		return true
	}
	if failAt == 38 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("s", thrift.TType(thrift.SET), int16(3), err).After(last)
	if failAt == 38 {
		return true
	}
	if failAt == 39 {
		err = failWith
	}
	last = protocol.EXPECT().ReadSetBegin(context.Background()).Return(thrift.TType(thrift.STRING), 0, err).After(last)
	if failAt == 39 {
		return true
	}
	if failAt == 40 {
		err = failWith
	}
	last = protocol.EXPECT().ReadSetEnd(context.Background()).Return(err).After(last)
	if failAt == 40 {
		return true
	}
	if failAt == 41 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 41 {
		return true
	}
	if failAt == 42 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("i", thrift.TType(thrift.I32), int16(4), err).After(last)
	if failAt == 42 {
		return true
	}
	if failAt == 43 {
		err = failWith
	}
	last = protocol.EXPECT().ReadI32(context.Background()).Return(int32(3), err).After(last)
	if failAt == 43 {
		return true
	}
	if failAt == 44 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 44 {
		return true
	}
	if failAt == 45 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("_", thrift.TType(thrift.STOP), int16(5), err).After(last)
	if failAt == 45 {
		return true
	}
	if failAt == 46 {
		err = failWith
	}
	last = protocol.EXPECT().ReadStructEnd(context.Background()).Return(err).After(last)
	if failAt == 46 {
		return true
	}
	if failAt == 47 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 47 {
		return true
	}
	if failAt == 48 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("_", thrift.TType(thrift.STOP), int16(1), err).After(last)
	if failAt == 48 {
		return true
	}
	if failAt == 49 {
		err = failWith
	}
	last = protocol.EXPECT().ReadStructEnd(context.Background()).Return(err).After(last)
	if failAt == 49 {
		return true
	}
	if failAt == 50 {
		err = failWith
	}
	last = protocol.EXPECT().ReadMessageEnd(context.Background()).Return(err).After(last)
	if failAt == 50 {
		return true
	}
	return false
}

// TestCase: Comprehensive call and reply workflow in the client.
// Expecting TTransportError on fail.
func TestClientReportTTransportErrors(t *testing.T) {
	mockCtrl := gomock.NewController(t)

	thing := errortest.NewTestStruct()
	thing.M = make(map[string]string)
	thing.L = make([]string, 0)
	thing.S = make([]string, 0)
	thing.I = 3

	err := thrift.NewTTransportException(thrift.TIMED_OUT, "test")
	for i := 0; ; i++ {
		protocol := NewMockTProtocol(mockCtrl)
		if !prepareClientCallReply(protocol, i, err) {
			return
		}
		client := errortest.NewErrorTestClient(thrift.NewTStandardClient(protocol, protocol))
		_, retErr := client.TestStruct(defaultCtx, thing)
		mockCtrl.Finish()
		mockCtrl = gomock.NewController(t)
		err2, ok := retErr.(thrift.TTransportException)
		if !ok {
			t.Fatal("Expected a TTrasportException")
		}

		if err2.TypeId() != thrift.TIMED_OUT {
			t.Fatal("Expected TIMED_OUT error")
		}
	}
}

// TestCase: Comprehensive call and reply workflow in the client.
// Expecting TTransportError on fail.
// Similar to TestClientReportTTransportErrors, but using legacy client constructor.
func TestClientReportTTransportErrorsLegacy(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	transport := thrift.NewTMemoryBuffer()
	thing := errortest.NewTestStruct()
	thing.M = make(map[string]string)
	thing.L = make([]string, 0)
	thing.S = make([]string, 0)
	thing.I = 3

	err := thrift.NewTTransportException(thrift.TIMED_OUT, "test")
	for i := 0; ; i++ {
		protocol := NewMockTProtocol(mockCtrl)
		if !prepareClientCallReply(protocol, i, err) {
			return
		}
		client := errortest.NewErrorTestClientProtocol(transport, protocol, protocol)
		_, retErr := client.TestStruct(defaultCtx, thing)
		mockCtrl.Finish()
		mockCtrl = gomock.NewController(t)
		err2, ok := retErr.(thrift.TTransportException)
		if !ok {
			t.Fatal("Expected a TTrasportException")
		}

		if err2.TypeId() != thrift.TIMED_OUT {
			t.Fatal("Expected TIMED_OUT error")
		}
	}
}

// TestCase: Comprehensive call and reply workflow in the client.
// Expecting TTProtocolErrors on fail.
func TestClientReportTProtocolErrors(t *testing.T) {
	mockCtrl := gomock.NewController(t)

	thing := errortest.NewTestStruct()
	thing.M = make(map[string]string)
	thing.L = make([]string, 0)
	thing.S = make([]string, 0)
	thing.I = 3

	err := thrift.NewTProtocolExceptionWithType(thrift.INVALID_DATA, errors.New("test"))
	for i := 0; ; i++ {
		protocol := NewMockTProtocol(mockCtrl)
		if !prepareClientCallReply(protocol, i, err) {
			return
		}
		client := errortest.NewErrorTestClient(thrift.NewTStandardClient(protocol, protocol))
		_, retErr := client.TestStruct(defaultCtx, thing)
		mockCtrl.Finish()
		mockCtrl = gomock.NewController(t)
		err2, ok := retErr.(thrift.TProtocolException)
		if !ok {
			t.Fatal("Expected a TProtocolException")
		}
		if err2.TypeId() != thrift.INVALID_DATA {
			t.Fatal("Expected INVALID_DATA error")
		}
	}
}

// TestCase: Comprehensive call and reply workflow in the client.
// Expecting TTProtocolErrors on fail.
// Similar to TestClientReportTProtocolErrors, but using legacy client constructor.
func TestClientReportTProtocolErrorsLegacy(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	transport := thrift.NewTMemoryBuffer()
	thing := errortest.NewTestStruct()
	thing.M = make(map[string]string)
	thing.L = make([]string, 0)
	thing.S = make([]string, 0)
	thing.I = 3

	err := thrift.NewTProtocolExceptionWithType(thrift.INVALID_DATA, errors.New("test"))
	for i := 0; ; i++ {
		protocol := NewMockTProtocol(mockCtrl)
		if !prepareClientCallReply(protocol, i, err) {
			return
		}
		client := errortest.NewErrorTestClientProtocol(transport, protocol, protocol)
		_, retErr := client.TestStruct(defaultCtx, thing)
		mockCtrl.Finish()
		mockCtrl = gomock.NewController(t)
		err2, ok := retErr.(thrift.TProtocolException)
		if !ok {
			t.Fatal("Expected a TProtocolException")
		}
		if err2.TypeId() != thrift.INVALID_DATA {
			t.Fatal("Expected INVALID_DATA error")
		}
	}
}

// TestCase: call and reply with exception workflow in the client.
// Setup mock to fail at a certain position. Return true if position exists otherwise false.
func prepareClientCallException(protocol *MockTProtocol, failAt int, failWith error) bool {
	var err error = nil

	// No need to test failure in this block, because it is covered in other test cases
	last := protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1))
	last = protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args").After(last)
	last = protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)).After(last)
	last = protocol.EXPECT().WriteString(context.Background(), "test").After(last)
	last = protocol.EXPECT().WriteFieldEnd(context.Background()).After(last)
	last = protocol.EXPECT().WriteFieldStop(context.Background()).After(last)
	last = protocol.EXPECT().WriteStructEnd(context.Background()).After(last)
	last = protocol.EXPECT().WriteMessageEnd(context.Background()).After(last)
	last = protocol.EXPECT().Flush(context.Background()).After(last)

	// Reading the exception, might fail.
	if failAt == 0 {
		err = failWith
	}
	last = protocol.EXPECT().ReadMessageBegin(context.Background()).Return("testString", thrift.EXCEPTION, int32(1), err).After(last)
	if failAt == 0 {
		return true
	}
	if failAt == 1 {
		err = failWith
	}
	last = protocol.EXPECT().ReadStructBegin(context.Background()).Return("TApplicationException", err).After(last)
	if failAt == 1 {
		return true
	}
	if failAt == 2 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("message", thrift.TType(thrift.STRING), int16(1), err).After(last)
	if failAt == 2 {
		return true
	}
	if failAt == 3 {
		err = failWith
	}
	last = protocol.EXPECT().ReadString(context.Background()).Return("test", err).After(last)
	if failAt == 3 {
		return true
	}
	if failAt == 4 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 4 {
		return true
	}
	if failAt == 5 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("type", thrift.TType(thrift.I32), int16(2), err).After(last)
	if failAt == 5 {
		return true
	}
	if failAt == 6 {
		err = failWith
	}
	last = protocol.EXPECT().ReadI32(context.Background()).Return(int32(thrift.PROTOCOL_ERROR), err).After(last)
	if failAt == 6 {
		return true
	}
	if failAt == 7 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldEnd(context.Background()).Return(err).After(last)
	if failAt == 7 {
		return true
	}
	if failAt == 8 {
		err = failWith
	}
	last = protocol.EXPECT().ReadFieldBegin(context.Background()).Return("_", thrift.TType(thrift.STOP), int16(2), err).After(last)
	if failAt == 8 {
		return true
	}
	if failAt == 9 {
		err = failWith
	}
	last = protocol.EXPECT().ReadStructEnd(context.Background()).Return(err).After(last)
	if failAt == 9 {
		return true
	}
	if failAt == 10 {
		err = failWith
	}
	last = protocol.EXPECT().ReadMessageEnd(context.Background()).Return(err).After(last)
	if failAt == 10 {
		return true
	}

	return false
}

// TestCase: call and reply with exception workflow in the client.
func TestClientCallException(t *testing.T) {
	mockCtrl := gomock.NewController(t)

	err := thrift.NewTTransportException(thrift.TIMED_OUT, "test")
	for i := 0; ; i++ {
		protocol := NewMockTProtocol(mockCtrl)
		willComplete := !prepareClientCallException(protocol, i, err)

		client := errortest.NewErrorTestClient(thrift.NewTStandardClient(protocol, protocol))
		_, retErr := client.TestString(defaultCtx, "test")
		mockCtrl.Finish()
		mockCtrl = gomock.NewController(t)

		if !willComplete {
			err2, ok := retErr.(thrift.TTransportException)
			if !ok {
				t.Fatal("Expected a TTransportException")
			}
			if err2.TypeId() != thrift.TIMED_OUT {
				t.Fatal("Expected TIMED_OUT error")
			}
		} else {
			err2, ok := retErr.(thrift.TApplicationException)
			if !ok {
				t.Fatal("Expected a TApplicationException")
			}
			if err2.TypeId() != thrift.PROTOCOL_ERROR {
				t.Fatal("Expected PROTOCOL_ERROR error")
			}
			break
		}
	}
}

// TestCase: call and reply with exception workflow in the client.
// Similar to TestClientCallException, but using legacy client constructor.
func TestClientCallExceptionLegacy(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	transport := thrift.NewTMemoryBuffer()
	err := thrift.NewTTransportException(thrift.TIMED_OUT, "test")
	for i := 0; ; i++ {
		protocol := NewMockTProtocol(mockCtrl)
		willComplete := !prepareClientCallException(protocol, i, err)

		client := errortest.NewErrorTestClientProtocol(transport, protocol, protocol)
		_, retErr := client.TestString(defaultCtx, "test")
		mockCtrl.Finish()
		mockCtrl = gomock.NewController(t)

		if !willComplete {
			err2, ok := retErr.(thrift.TTransportException)
			if !ok {
				t.Fatal("Expected a TTransportException")
			}
			if err2.TypeId() != thrift.TIMED_OUT {
				t.Fatal("Expected TIMED_OUT error")
			}
		} else {
			err2, ok := retErr.(thrift.TApplicationException)
			if !ok {
				t.Fatal("Expected a TApplicationException")
			}
			if err2.TypeId() != thrift.PROTOCOL_ERROR {
				t.Fatal("Expected PROTOCOL_ERROR error")
			}
			break
		}
	}
}

// TestCase: Mismatching sequence id has been received in the client.
func TestClientSeqIdMismatch(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	protocol := NewMockTProtocol(mockCtrl)
	gomock.InOrder(
		protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1)),
		protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args"),
		protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)),
		protocol.EXPECT().WriteString(context.Background(), "test"),
		protocol.EXPECT().WriteFieldEnd(context.Background()),
		protocol.EXPECT().WriteFieldStop(context.Background()),
		protocol.EXPECT().WriteStructEnd(context.Background()),
		protocol.EXPECT().WriteMessageEnd(context.Background()),
		protocol.EXPECT().Flush(context.Background()),
		protocol.EXPECT().ReadMessageBegin(context.Background()).Return("testString", thrift.REPLY, int32(2), nil),
	)

	client := errortest.NewErrorTestClient(thrift.NewTStandardClient(protocol, protocol))
	_, err := client.TestString(defaultCtx, "test")
	mockCtrl.Finish()
	appErr, ok := err.(thrift.TApplicationException)
	if !ok {
		t.Fatal("Expected TApplicationException")
	}
	if appErr.TypeId() != thrift.BAD_SEQUENCE_ID {
		t.Fatal("Expected BAD_SEQUENCE_ID error")
	}
}

// TestCase: Mismatching sequence id has been received in the client.
// Similar to TestClientSeqIdMismatch, but using legacy client constructor.
func TestClientSeqIdMismatchLegeacy(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	transport := thrift.NewTMemoryBuffer()
	protocol := NewMockTProtocol(mockCtrl)
	gomock.InOrder(
		protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1)),
		protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args"),
		protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)),
		protocol.EXPECT().WriteString(context.Background(), "test"),
		protocol.EXPECT().WriteFieldEnd(context.Background()),
		protocol.EXPECT().WriteFieldStop(context.Background()),
		protocol.EXPECT().WriteStructEnd(context.Background()),
		protocol.EXPECT().WriteMessageEnd(context.Background()),
		protocol.EXPECT().Flush(context.Background()),
		protocol.EXPECT().ReadMessageBegin(context.Background()).Return("testString", thrift.REPLY, int32(2), nil),
	)

	client := errortest.NewErrorTestClientProtocol(transport, protocol, protocol)
	_, err := client.TestString(defaultCtx, "test")
	mockCtrl.Finish()
	appErr, ok := err.(thrift.TApplicationException)
	if !ok {
		t.Fatal("Expected TApplicationException")
	}
	if appErr.TypeId() != thrift.BAD_SEQUENCE_ID {
		t.Fatal("Expected BAD_SEQUENCE_ID error")
	}
}

// TestCase: Wrong method name has been received in the client.
func TestClientWrongMethodName(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	protocol := NewMockTProtocol(mockCtrl)
	gomock.InOrder(
		protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1)),
		protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args"),
		protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)),
		protocol.EXPECT().WriteString(context.Background(), "test"),
		protocol.EXPECT().WriteFieldEnd(context.Background()),
		protocol.EXPECT().WriteFieldStop(context.Background()),
		protocol.EXPECT().WriteStructEnd(context.Background()),
		protocol.EXPECT().WriteMessageEnd(context.Background()),
		protocol.EXPECT().Flush(context.Background()),
		protocol.EXPECT().ReadMessageBegin(context.Background()).Return("unknown", thrift.REPLY, int32(1), nil),
	)

	client := errortest.NewErrorTestClient(thrift.NewTStandardClient(protocol, protocol))
	_, err := client.TestString(defaultCtx, "test")
	mockCtrl.Finish()
	appErr, ok := err.(thrift.TApplicationException)
	if !ok {
		t.Fatal("Expected TApplicationException")
	}
	if appErr.TypeId() != thrift.WRONG_METHOD_NAME {
		t.Fatal("Expected WRONG_METHOD_NAME error")
	}
}

// TestCase: Wrong method name has been received in the client.
// Similar to TestClientWrongMethodName, but using legacy client constructor.
func TestClientWrongMethodNameLegacy(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	transport := thrift.NewTMemoryBuffer()
	protocol := NewMockTProtocol(mockCtrl)
	gomock.InOrder(
		protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1)),
		protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args"),
		protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)),
		protocol.EXPECT().WriteString(context.Background(), "test"),
		protocol.EXPECT().WriteFieldEnd(context.Background()),
		protocol.EXPECT().WriteFieldStop(context.Background()),
		protocol.EXPECT().WriteStructEnd(context.Background()),
		protocol.EXPECT().WriteMessageEnd(context.Background()),
		protocol.EXPECT().Flush(context.Background()),
		protocol.EXPECT().ReadMessageBegin(context.Background()).Return("unknown", thrift.REPLY, int32(1), nil),
	)

	client := errortest.NewErrorTestClientProtocol(transport, protocol, protocol)
	_, err := client.TestString(defaultCtx, "test")
	mockCtrl.Finish()
	appErr, ok := err.(thrift.TApplicationException)
	if !ok {
		t.Fatal("Expected TApplicationException")
	}
	if appErr.TypeId() != thrift.WRONG_METHOD_NAME {
		t.Fatal("Expected WRONG_METHOD_NAME error")
	}
}

// TestCase: Wrong message type has been received in the client.
func TestClientWrongMessageType(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	protocol := NewMockTProtocol(mockCtrl)
	gomock.InOrder(
		protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1)),
		protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args"),
		protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)),
		protocol.EXPECT().WriteString(context.Background(), "test"),
		protocol.EXPECT().WriteFieldEnd(context.Background()),
		protocol.EXPECT().WriteFieldStop(context.Background()),
		protocol.EXPECT().WriteStructEnd(context.Background()),
		protocol.EXPECT().WriteMessageEnd(context.Background()),
		protocol.EXPECT().Flush(context.Background()),
		protocol.EXPECT().ReadMessageBegin(context.Background()).Return("testString", thrift.INVALID_TMESSAGE_TYPE, int32(1), nil),
	)

	client := errortest.NewErrorTestClient(thrift.NewTStandardClient(protocol, protocol))
	_, err := client.TestString(defaultCtx, "test")
	mockCtrl.Finish()
	appErr, ok := err.(thrift.TApplicationException)
	if !ok {
		t.Fatal("Expected TApplicationException")
	}
	if appErr.TypeId() != thrift.INVALID_MESSAGE_TYPE_EXCEPTION {
		t.Fatal("Expected INVALID_MESSAGE_TYPE_EXCEPTION error")
	}
}

// TestCase: Wrong message type has been received in the client.
// Similar to TestClientWrongMessageType, but using legacy client constructor.
func TestClientWrongMessageTypeLegacy(t *testing.T) {
	mockCtrl := gomock.NewController(t)
	transport := thrift.NewTMemoryBuffer()
	protocol := NewMockTProtocol(mockCtrl)
	gomock.InOrder(
		protocol.EXPECT().WriteMessageBegin(context.Background(), "testString", thrift.CALL, int32(1)),
		protocol.EXPECT().WriteStructBegin(context.Background(), "testString_args"),
		protocol.EXPECT().WriteFieldBegin(context.Background(), "s", thrift.TType(thrift.STRING), int16(1)),
		protocol.EXPECT().WriteString(context.Background(), "test"),
		protocol.EXPECT().WriteFieldEnd(context.Background()),
		protocol.EXPECT().WriteFieldStop(context.Background()),
		protocol.EXPECT().WriteStructEnd(context.Background()),
		protocol.EXPECT().WriteMessageEnd(context.Background()),
		protocol.EXPECT().Flush(context.Background()),
		protocol.EXPECT().ReadMessageBegin(context.Background()).Return("testString", thrift.INVALID_TMESSAGE_TYPE, int32(1), nil),
	)

	client := errortest.NewErrorTestClientProtocol(transport, protocol, protocol)
	_, err := client.TestString(defaultCtx, "test")
	mockCtrl.Finish()
	appErr, ok := err.(thrift.TApplicationException)
	if !ok {
		t.Fatal("Expected TApplicationException")
	}
	if appErr.TypeId() != thrift.INVALID_MESSAGE_TYPE_EXCEPTION {
		t.Fatal("Expected INVALID_MESSAGE_TYPE_EXCEPTION error")
	}
}
