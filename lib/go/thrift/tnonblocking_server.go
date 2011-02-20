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
  "os"
)

/**
 * A nonblocking TServer implementation. This allows for fairness amongst all
 * connected clients in terms of invocations.
 *
 * This server is inherently single-threaded. If you want a limited thread pool
 * coupled with invocation-fairness, see THsHaServer.
 *
 * To use this server, you MUST use a TFramedTransport at the outermost
 * transport, otherwise this server will be unable to determine when a whole
 * method call has been read off the wire. Clients must also use TFramedTransport.
 */
type TNonblockingServer struct {
  /** Flag for stopping the server */
  stopped bool

  processorFactory       TProcessorFactory
  serverTransport        TServerTransport
  inputTransportFactory  TTransportFactory
  outputTransportFactory TTransportFactory
  inputProtocolFactory   TProtocolFactory
  outputProtocolFactory  TProtocolFactory
}


func NewTNonblockingServer2(processor TProcessor, serverTransport TServerTransport) *TNonblockingServer {
  return NewTNonblockingServerFactory2(NewTProcessorFactory(processor), serverTransport)
}

func NewTNonblockingServer4(processor TProcessor, serverTransport TServerTransport, transportFactory TTransportFactory, protocolFactory TProtocolFactory) *TNonblockingServer {
  return NewTNonblockingServerFactory4(NewTProcessorFactory(processor),
    serverTransport,
    transportFactory,
    protocolFactory,
  )
}

func NewTNonblockingServer6(processor TProcessor, serverTransport TServerTransport, inputTransportFactory TTransportFactory, outputTransportFactory TTransportFactory, inputProtocolFactory TProtocolFactory, outputProtocolFactory TProtocolFactory) *TNonblockingServer {
  return NewTNonblockingServerFactory6(NewTProcessorFactory(processor),
    serverTransport,
    inputTransportFactory,
    outputTransportFactory,
    inputProtocolFactory,
    outputProtocolFactory,
  )
}

func NewTNonblockingServerFactory2(processorFactory TProcessorFactory, serverTransport TServerTransport) *TNonblockingServer {
  return NewTNonblockingServerFactory6(processorFactory,
    serverTransport,
    NewTTransportFactory(),
    NewTTransportFactory(),
    NewTBinaryProtocolFactoryDefault(),
    NewTBinaryProtocolFactoryDefault(),
  )
}

func NewTNonblockingServerFactory4(processorFactory TProcessorFactory, serverTransport TServerTransport, transportFactory TTransportFactory, protocolFactory TProtocolFactory) *TNonblockingServer {
  return NewTNonblockingServerFactory6(processorFactory,
    serverTransport,
    transportFactory,
    transportFactory,
    protocolFactory,
    protocolFactory,
  )
}

func NewTNonblockingServerFactory6(processorFactory TProcessorFactory, serverTransport TServerTransport, inputTransportFactory TTransportFactory, outputTransportFactory TTransportFactory, inputProtocolFactory TProtocolFactory, outputProtocolFactory TProtocolFactory) *TNonblockingServer {
  return &TNonblockingServer{processorFactory: processorFactory,
    serverTransport:        serverTransport,
    inputTransportFactory:  inputTransportFactory,
    outputTransportFactory: outputTransportFactory,
    inputProtocolFactory:   inputProtocolFactory,
    outputProtocolFactory:  outputProtocolFactory,
  }
}

func (p *TNonblockingServer) ProcessorFactory() TProcessorFactory {
  return p.processorFactory
}

func (p *TNonblockingServer) ServerTransport() TServerTransport {
  return p.serverTransport
}

func (p *TNonblockingServer) InputTransportFactory() TTransportFactory {
  return p.inputTransportFactory
}

func (p *TNonblockingServer) OutputTransportFactory() TTransportFactory {
  return p.outputTransportFactory
}

func (p *TNonblockingServer) InputProtocolFactory() TProtocolFactory {
  return p.inputProtocolFactory
}

func (p *TNonblockingServer) OutputProtocolFactory() TProtocolFactory {
  return p.outputProtocolFactory
}

func (p *TNonblockingServer) Serve() os.Error {
  p.stopped = false
  err := p.serverTransport.Listen()
  if err != nil {
    return err
  }
  for !p.stopped {
    client, err := p.serverTransport.Accept()
    if err != nil {
      return err
    }
    if client != nil {
      go p.processRequest(client)
    }
  }
  return nil
}

func (p *TNonblockingServer) Stop() os.Error {
  p.stopped = true
  p.serverTransport.Interrupt()
  return nil
}

func (p *TNonblockingServer) IsStopped() bool {
  return p.stopped
}

func (p *TNonblockingServer) processRequest(client TTransport) {
  processor := p.processorFactory.GetProcessor(client)
  inputTransport := p.inputTransportFactory.GetTransport(client)
  outputTransport := p.outputTransportFactory.GetTransport(client)
  inputProtocol := p.inputProtocolFactory.GetProtocol(inputTransport)
  outputProtocol := p.outputProtocolFactory.GetProtocol(outputTransport)
  if inputTransport != nil {
    defer inputTransport.Close()
  }
  if outputTransport != nil {
    defer outputTransport.Close()
  }
  for {
    ok, e := processor.Process(inputProtocol, outputProtocol)
    if e != nil {
      if !p.stopped {
        // TODO(pomack) log error
        break
      }
    }
    if !ok {
      break
    }
  }
}
