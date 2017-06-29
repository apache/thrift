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
	"log"
	"runtime/debug"
	"sync"
)

/*
 * This is only a simple sample same as TSimpleServer but add context
 * usage support.
 */
type TSimpleServer2 struct {
	quit chan struct{}

	processorFactory       TProcessorFactory2
	serverTransport        TServerTransport
	inputTransportFactory  TTransportFactory
	outputTransportFactory TTransportFactory
	inputProtocolFactory   TProtocolFactory
	outputProtocolFactory  TProtocolFactory
	sync.WaitGroup
}

func NewTSimpleServerWithContext(processor TProcessor2, serverTransport TServerTransport) *TSimpleServer2 {
	return NewTSimpleServerFactoryWithContext(NewTProcessorFactory2(processor), serverTransport)
}

func NewTSimpleServerFactoryWithContext(processorFactory TProcessorFactory2, serverTransport TServerTransport) *TSimpleServer2 {
	return &TSimpleServer2{
		quit:                   make(chan struct{}, 1),
		processorFactory:       processorFactory,
		serverTransport:        serverTransport,
		inputTransportFactory:  NewTTransportFactory(),
		outputTransportFactory: NewTTransportFactory(),
		inputProtocolFactory:   NewTBinaryProtocolFactoryDefault(),
		outputProtocolFactory:  NewTBinaryProtocolFactoryDefault(),
	}
}

func (p *TSimpleServer2) ProcessorFactory() TProcessorFactory2 {
	return p.processorFactory
}

func (p *TSimpleServer2) ServerTransport() TServerTransport {
	return p.serverTransport
}

func (p *TSimpleServer2) InputTransportFactory() TTransportFactory {
	return p.inputTransportFactory
}

func (p *TSimpleServer2) OutputTransportFactory() TTransportFactory {
	return p.outputTransportFactory
}

func (p *TSimpleServer2) InputProtocolFactory() TProtocolFactory {
	return p.inputProtocolFactory
}

func (p *TSimpleServer2) OutputProtocolFactory() TProtocolFactory {
	return p.outputProtocolFactory
}

func (p *TSimpleServer2) Listen() error {
	return p.serverTransport.Listen()
}

func (p *TSimpleServer2) AcceptLoop() error {
	for {
		client, err := p.serverTransport.Accept()
		if err != nil {
			select {
			case <-p.quit:
				return nil
			default:
			}
			return err
		}
		if client != nil {
			p.Add(1)
			go func() {
				if err := p.processRequests(client); err != nil {
					log.Println("error processing request:", err)
				}
			}()
		}
	}
}

func (p *TSimpleServer2) Serve() error {
	err := p.Listen()
	if err != nil {
		return err
	}
	p.AcceptLoop()
	return nil
}

var once2 sync.Once

func (p *TSimpleServer2) Stop() error {
	q := func() {
		close(p.quit)
		p.serverTransport.Interrupt()
		p.Wait()
	}
	once2.Do(q)
	return nil
}

func (p *TSimpleServer2) processRequests(client TTransport) error {
	defer p.Done()

	processor := p.processorFactory.GetProcessor(client)
	inputTransport, err := p.inputTransportFactory.GetTransport(client)
	if err != nil {
		return err
	}
	outputTransport, err := p.outputTransportFactory.GetTransport(client)
	if err != nil {
		return err
	}
	inputProtocol := p.inputProtocolFactory.GetProtocol(inputTransport)
	outputProtocol := p.outputProtocolFactory.GetProtocol(outputTransport)
	defer func() {
		if e := recover(); e != nil {
			log.Printf("panic in processor: %s: %s", e, debug.Stack())
		}
	}()

	if inputTransport != nil {
		defer inputTransport.Close()
	}
	if outputTransport != nil {
		defer outputTransport.Close()
	}
	for {
		select {
		case <-p.quit:
			return nil
		default:
		}

		ctx := context.Background()
		ok, err := processor.Process(ctx, inputProtocol, outputProtocol)
		if err, ok := err.(TTransportException); ok && err.TypeId() == END_OF_FILE {
			return nil
		} else if err != nil {
			return err
		}
		if err, ok := err.(TApplicationException); ok && err.TypeId() == UNKNOWN_METHOD {
			continue
		}
		if !ok {
			break
		}
	}
	return nil
}
