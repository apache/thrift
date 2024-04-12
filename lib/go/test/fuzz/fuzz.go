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
	"github.com/apache/thrift/lib/go/thrift"
)

const nbFuzzedProtocols = 2

func fuzzChooseProtocol(d byte, t thrift.TTransport) thrift.TProtocol {
	switch d % nbFuzzedProtocols {
	default:
		fallthrough
	case 0:
		return thrift.NewTBinaryProtocolFactoryConf(nil).GetProtocol(t)
	case 1:
		return thrift.NewTCompactProtocolFactoryConf(nil).GetProtocol(t)
	}
}

func Fuzz(data []byte) int {
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
