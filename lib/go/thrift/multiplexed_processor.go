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
	"context"
	"fmt"
	"strings"
)

/*
TMultiplexedProcessor2 is a TProcessor allowing
a single TServer to provide multiple services with
context support in TProcessor.

To do so, you instantiate the processor and then register additional
processors with it, as shown in the following example:

var processor = thrift.NewTMultiplexedProcessor2()

firstProcessor :=
processor.RegisterProcessor("FirstService", firstProcessor)

processor.registerProcessor(
  "Calculator",
  Calculator.NewCalculatorProcessor(&CalculatorHandler{}),
)

processor.registerProcessor(
  "WeatherReport",
  WeatherReport.NewWeatherReportProcessor(&WeatherReportHandler{}),
)

serverTransport, err := thrift.NewTServerSocketTimeout(addr, TIMEOUT)
if err != nil {
  t.Fatal("Unable to create server socket", err)
}
server := thrift.NewTSimpleServer2(processor, serverTransport)
server.Serve();
*/

type TMultiplexedProcessor2 struct {
	serviceProcessorMap map[string]TProcessor2
	DefaultProcessor    TProcessor2
}

func NewTMultiplexedProcessor2() *TMultiplexedProcessor2 {
	return &TMultiplexedProcessor2{
		serviceProcessorMap: make(map[string]TProcessor2),
	}
}

func (t *TMultiplexedProcessor2) RegisterDefault(processor TProcessor2) {
	t.DefaultProcessor = processor
}

func (t *TMultiplexedProcessor2) RegisterProcessor(name string, processor TProcessor2) {
	if t.serviceProcessorMap == nil {
		t.serviceProcessorMap = make(map[string]TProcessor2)
	}
	t.serviceProcessorMap[name] = processor
}

func (t *TMultiplexedProcessor2) Process(ctx context.Context, in, out TProtocol) (bool, TException) {
	name, typeId, seqid, err := in.ReadMessageBegin()
	if err != nil {
		return false, err
	}
	if typeId != CALL && typeId != ONEWAY {
		return false, fmt.Errorf("Unexpected message type %v", typeId)
	}
	//extract the service name
	v := strings.SplitN(name, MULTIPLEXED_SEPARATOR, 2)
	if len(v) != 2 {
		if t.DefaultProcessor != nil {
			smb := NewStoredMessageProtocol(in, name, typeId, seqid)
			return t.DefaultProcessor.Process(ctx, smb, out)
		}
		return false, fmt.Errorf("Service name not found in message name: %s.  Did you forget to use a TMultiplexProtocol in your client?", name)
	}
	actualProcessor, ok := t.serviceProcessorMap[v[0]]
	if !ok {
		return false, fmt.Errorf("Service name not found: %s.  Did you forget to call registerProcessor()?", v[0])
	}
	smb := NewStoredMessageProtocol(in, v[1], typeId, seqid)
	return actualProcessor.Process(ctx, smb, out)
}
