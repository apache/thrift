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
  "net"
  "thrift"
  "thriftlib/tutorial"
)


type GoServer struct {
  handler tutorial.ICalculator
  processor *tutorial.CalculatorProcessor
}

func NewGoServer() *GoServer {
  handler := NewCalculatorHandler()
  processor := tutorial.NewCalculatorProcessor(handler)
  return &GoServer{handler:handler, processor:processor}
}

func Simple(processor *tutorial.CalculatorProcessor, transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory, ch chan int) {
  addr, err := net.ResolveTCPAddr("localhost:9090")
  if err != nil {
    fmt.Print("Error resolving address: ", err.String(), "\n")
    return
  }
  serverTransport, err := thrift.NewTServerSocketAddr(addr)
  if err != nil {
    fmt.Print("Error creating server socket: ", err.String(), "\n")
    return
  }
  server := thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)
  // Use this for a multithreaded server
  // TServer server = new TThreadPoolServer(new TThreadPoolServer.Args(serverTransport).processor(processor));
  
  fmt.Print("Starting the simple server... on ", addr, "\n")
  for {
    err = server.Serve()
    if err != nil {
      fmt.Print("Error during simple server: ", err.String(), "\n")
      return
    }
  }
  fmt.Print("Done with the simple server\n")
  ch <- 1
}

func Secure(processor *tutorial.CalculatorProcessor) {
  addr, _ := net.ResolveTCPAddr("localhost:9091")
  serverTransport, _ := thrift.NewTNonblockingServerSocketAddr(addr)
  server := thrift.NewTSimpleServer2(processor, serverTransport)
  fmt.Print("Starting the secure server... on ", addr, "\n")
  server.Serve()
  fmt.Print("Done with the secure server\n")
}

func RunServer(transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory) {
  server := NewGoServer()
  ch := make(chan int)
  go Simple(server.processor, transportFactory, protocolFactory, ch)
  //go Secure(server.processor)
  _ = <- ch
}
