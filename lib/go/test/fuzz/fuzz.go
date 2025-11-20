//go:build gofuzz
// +build gofuzz

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

package fuzz

import (
	"context"
	"fmt"
	"strconv"

	"github.com/apache/thrift/lib/go/test/fuzz/gen-go/shared"
	"github.com/apache/thrift/lib/go/test/fuzz/gen-go/tutorial"
	"github.com/apache/thrift/lib/go/test/fuzz/gen-go/fuzztest"
	"github.com/apache/thrift/lib/go/thrift"
)

const nbFuzzedProtocols = 2

// 10MB message size limit to prevent over-allocation during fuzzing
const FUZZ_MAX_MESSAGE_SIZE = 10 * 1024 * 1024

func fuzzChooseProtocol(d byte, t thrift.TTransport) thrift.TProtocol {
	switch d % nbFuzzedProtocols {
	default:
		fallthrough
	case 0:
		return thrift.NewTBinaryProtocolFactoryConf(nil).GetProtocol(t)
	case 1:
		return thrift.NewTCompactProtocolFactoryConf(nil).GetProtocol(t)
	case 2:
		return thrift.NewTJSONProtocolFactory().GetProtocol(t)
	}
}

func FuzzTutorial(data []byte) int {
	if len(data) < 2 {
		return 0
	}
	inputTransport := thrift.NewTMemoryBuffer()
	inputTransport.Buffer.Write(data[2:])
	outputTransport := thrift.NewTMemoryBuffer()
	outputProtocol := fuzzChooseProtocol(data[0], outputTransport)
	inputProtocol := fuzzChooseProtocol(data[1], inputTransport)
	ctx := thrift.SetResponseHelper(
		context.Background(),
		thrift.TResponseHelper{
			THeaderResponseHelper: thrift.NewTHeaderResponseHelper(outputProtocol),
		},
	)
	handler := NewCalculatorHandler()
	processor := tutorial.NewCalculatorProcessor(handler)
	ok := true
	var err error
	for ok {
		ok, err = processor.Process(ctx, inputProtocol, outputProtocol)
		if err != nil {
			// Handle parse error
			return 0
		}
		res := make([]byte, 1024)
		n, err := outputTransport.Buffer.Read(res)
		fmt.Printf("lol %d %s %v\n", n, err, res)
	}
	return 1
}

type CalculatorHandler struct {
	log map[int]*shared.SharedStruct
}

func NewCalculatorHandler() *CalculatorHandler {
	return &CalculatorHandler{log: make(map[int]*shared.SharedStruct)}
}

func (p *CalculatorHandler) Ping(ctx context.Context) (err error) {
	fmt.Print("ping()\n")
	return nil
}

func (p *CalculatorHandler) Add(ctx context.Context, num1 int32, num2 int32) (retval17 int32, err error) {
	fmt.Print("add(", num1, ",", num2, ")\n")
	return num1 + num2, nil
}

func (p *CalculatorHandler) Calculate(ctx context.Context, logid int32, w *tutorial.Work) (val int32, err error) {
	fmt.Print("calculate(", logid, ", {", w.Op, ",", w.Num1, ",", w.Num2, "})\n")
	switch w.Op {
	case tutorial.Operation_ADD:
		val = w.Num1 + w.Num2
		break
	case tutorial.Operation_SUBTRACT:
		val = w.Num1 - w.Num2
		break
	case tutorial.Operation_MULTIPLY:
		val = w.Num1 * w.Num2
		break
	case tutorial.Operation_DIVIDE:
		if w.Num2 == 0 {
			ouch := tutorial.NewInvalidOperation()
			ouch.WhatOp = int32(w.Op)
			ouch.Why = "Cannot divide by 0"
			err = ouch
			return
		}
		val = w.Num1 / w.Num2
		break
	default:
		ouch := tutorial.NewInvalidOperation()
		ouch.WhatOp = int32(w.Op)
		ouch.Why = "Unknown operation"
		err = ouch
		return
	}
	entry := shared.NewSharedStruct()
	entry.Key = logid
	entry.Value = strconv.Itoa(int(val))
	k := int(logid)
	p.log[k] = entry
	return val, err
}

func (p *CalculatorHandler) GetStruct(ctx context.Context, key int32) (*shared.SharedStruct, error) {
	fmt.Print("getStruct(", key, ")\n")
	v, _ := p.log[int(key)]
	return v, nil
}

func (p *CalculatorHandler) Zip(ctx context.Context) (err error) {
	fmt.Print("zip()\n")
	return nil
}

func FuzzParseBinary(data []byte) int {
	// Skip if input is too small
	if len(data) < 1 {
		return 0
	}

	// Create transport and protocol
	transport := thrift.NewTMemoryBufferLen(len(data))
	defer func() {
		transport.Close()
		// Reset the buffer to release memory
		transport.Buffer.Reset()
	}()
	transport.Write(data)
	config := &thrift.TConfiguration{
		MaxMessageSize: FUZZ_MAX_MESSAGE_SIZE,
	}
	protocol := thrift.NewTBinaryProtocolFactoryConf(config).GetProtocol(transport)

	// Try to read the FuzzTest structure
	fuzzTest := fuzztest.NewFuzzTest()
	err := fuzzTest.Read(context.Background(), protocol)
	if err != nil {
		// Invalid input, but not a crash
		return 0
	}

	// Successfully parsed
	return 1
}

func FuzzParseCompact(data []byte) int {
	// Skip if input is too small
	if len(data) < 1 {
		return 0
	}

	// Create transport and protocol
	transport := thrift.NewTMemoryBufferLen(len(data))
	defer func() {
		transport.Close()
		// Reset the buffer to release memory
		transport.Buffer.Reset()
	}()
	transport.Write(data)
	config := &thrift.TConfiguration{
		MaxMessageSize: FUZZ_MAX_MESSAGE_SIZE,
	}
	protocol := thrift.NewTCompactProtocolFactoryConf(config).GetProtocol(transport)

	// Try to read the FuzzTest structure
	fuzzTest := fuzztest.NewFuzzTest()
	err := fuzzTest.Read(context.Background(), protocol)
	if err != nil {
		// Invalid input, but not a crash
		return 0
	}

	// Successfully parsed
	return 1
}

func FuzzParseJson(data []byte) int {
	// Skip if input is too small
	if len(data) < 1 {
		return 0
	}

	// Create transport and protocol
	transport := thrift.NewTMemoryBufferLen(len(data))
	defer func() {
		transport.Close()
		// Reset the buffer to release memory
		transport.Buffer.Reset()
	}()
	transport.Write(data)
	protocol := thrift.NewTJSONProtocolFactory().GetProtocol(transport)

	// Try to read the FuzzTest structure
	fuzzTest := fuzztest.NewFuzzTest()
	err := fuzzTest.Read(context.Background(), protocol)
	if err != nil {
		// Invalid input, but not a crash
		return 0
	}

	// Successfully parsed
	return 1
}

func FuzzRoundtripBinary(data []byte) int {
	// Skip if input is too small
	if len(data) < 1 {
		return 0
	}

	config := &thrift.TConfiguration{
		MaxMessageSize: FUZZ_MAX_MESSAGE_SIZE,
	}

	// First parse
	transport := thrift.NewTMemoryBufferLen(len(data))
	transport.Write(data)
	protocol := thrift.NewTBinaryProtocolFactoryConf(config).GetProtocol(transport)

	// Try to read the FuzzTest structure
	test1 := fuzztest.NewFuzzTest()
	err := test1.Read(context.Background(), protocol)
	if err != nil {
		// Invalid input, but not a crash
		return 0
	}

	// Serialize back
	outTransport := thrift.NewTMemoryBuffer()
	outProtocol := thrift.NewTBinaryProtocolFactoryConf(config).GetProtocol(outTransport)
	err = test1.Write(context.Background(), outProtocol)
	if err != nil {
		return 0
	}

	// Get serialized data and deserialize again
	serialized := outTransport.Bytes()
	reTransport := thrift.NewTMemoryBufferLen(len(serialized))
	reTransport.Write(serialized)
	reProtocol := thrift.NewTBinaryProtocolFactoryConf(config).GetProtocol(reTransport)

	test2 := fuzztest.NewFuzzTest()
	err = test2.Read(context.Background(), reProtocol)
	if err != nil {
		return 0
	}

	// Verify equality
	if !test1.Equals(test2) {
		panic("Roundtrip failed: objects not equal after deserialization")
	}

	return 1
}

func FuzzRoundtripCompact(data []byte) int {
	// Skip if input is too small
	if len(data) < 1 {
		return 0
	}

	config := &thrift.TConfiguration{
		MaxMessageSize: FUZZ_MAX_MESSAGE_SIZE,
	}

	// First parse
	transport := thrift.NewTMemoryBufferLen(len(data))
	transport.Write(data)
	protocol := thrift.NewTCompactProtocolFactoryConf(config).GetProtocol(transport)

	// Try to read the FuzzTest structure
	test1 := fuzztest.NewFuzzTest()
	err := test1.Read(context.Background(), protocol)
	if err != nil {
		// Invalid input, but not a crash
		return 0
	}

	// Serialize back
	outTransport := thrift.NewTMemoryBuffer()
	outProtocol := thrift.NewTCompactProtocolFactoryConf(config).GetProtocol(outTransport)
	err = test1.Write(context.Background(), outProtocol)
	if err != nil {
		return 0
	}

	// Get serialized data and deserialize again
	serialized := outTransport.Bytes()
	reTransport := thrift.NewTMemoryBufferLen(len(serialized))
	reTransport.Write(serialized)
	reProtocol := thrift.NewTCompactProtocolFactoryConf(config).GetProtocol(reTransport)

	test2 := fuzztest.NewFuzzTest()
	err = test2.Read(context.Background(), reProtocol)
	if err != nil {
		return 0
	}

	// Verify equality
	if !test1.Equals(test2) {
		panic("Roundtrip failed: objects not equal after deserialization")
	}

	return 1
}

func FuzzRoundtripJson(data []byte) int {
	// Skip if input is too small
	if len(data) < 1 {
		return 0
	}

	// First parse
	transport := thrift.NewTMemoryBufferLen(len(data))
	transport.Write(data)
	protocol := thrift.NewTJSONProtocolFactory().GetProtocol(transport)

	// Try to read the FuzzTest structure
	test1 := fuzztest.NewFuzzTest()
	err := test1.Read(context.Background(), protocol)
	if err != nil {
		// Invalid input, but not a crash
		return 0
	}

	// Serialize back
	outTransport := thrift.NewTMemoryBuffer()
	outProtocol := thrift.NewTJSONProtocolFactory().GetProtocol(outTransport)
	err = test1.Write(context.Background(), outProtocol)
	if err != nil {
		return 0
	}

	// Get serialized data and deserialize again
	serialized := outTransport.Bytes()
	reTransport := thrift.NewTMemoryBufferLen(len(serialized))
	reTransport.Write(serialized)
	reProtocol := thrift.NewTJSONProtocolFactory().GetProtocol(reTransport)

	test2 := fuzztest.NewFuzzTest()
	err = test2.Read(context.Background(), reProtocol)
	if err != nil {
		return 0
	}

	// Verify equality
	if !test1.Equals(test2) {
		panic("Roundtrip failed: objects not equal after deserialization")
	}

	return 1
}
