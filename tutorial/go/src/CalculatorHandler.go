package main;

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

import (
  "fmt"
  "os"
  "strconv"
  "thriftlib/tutorial"
  "thriftlib/shared"
)

type CalculatorHandler struct {
  log map[int]*shared.SharedStruct
}

func NewCalculatorHandler() *CalculatorHandler {
  return &CalculatorHandler{log:make(map[int]*shared.SharedStruct)}
}

func (p *CalculatorHandler) Ping() (err os.Error) {
  fmt.Print("ping()\n")
  return nil
}

func (p *CalculatorHandler) Add(num1 int32, num2 int32) (retval17 int32, err os.Error) {
  fmt.Print("add(", num1, ",", num2, ")\n")
  return num1 + num2, nil
}

func (p *CalculatorHandler) Calculate(logid int32, w *tutorial.Work) (val int32, ouch *tutorial.InvalidOperation, err os.Error) {
  fmt.Print("calculate(", logid, ", {", w.Op, ",", w.Num1, ",", w.Num2, "})\n")
  switch(w.Op) {
  case tutorial.ADD:
    val = w.Num1 + w.Num2
    break
  case tutorial.SUBTRACT:
    val = w.Num1 - w.Num2
    break
  case tutorial.MULTIPLY:
    val = w.Num1 * w.Num2
    break
  case tutorial.DIVIDE:
    if w.Num2 == 0 {
      ouch = tutorial.NewInvalidOperation()
      ouch.What = int32(w.Op)
      ouch.Why = "Cannot divide by 0"
      return
    }
    val = w.Num1 / w.Num2
    break
  default:
    ouch = tutorial.NewInvalidOperation()
    ouch.What = int32(w.Op)
    ouch.Why = "Unknown operation"
    return
  }
  entry := shared.NewSharedStruct()
  entry.Key = logid
  entry.Value = strconv.Itoa(int(val))
  k := int(logid)
  /*
  oldvalue, exists := p.log[k]
  if exists {
    fmt.Print("Replacing ", oldvalue, " with ", entry, " for key ", k, "\n")
  } else {
    fmt.Print("Adding ", entry, " for key ", k, "\n")
  }
  */
  p.log[k] = entry, true
  return val, ouch, err
}

func (p *CalculatorHandler) GetStruct(key int32) (*shared.SharedStruct, os.Error) {
  fmt.Print("getStruct(", key, ")\n")
  v, _ := p.log[int(key)]
  return v, nil
}

func (p *CalculatorHandler) Zip() (err os.Error) {
  fmt.Print("zip()\n")
  return nil
}

