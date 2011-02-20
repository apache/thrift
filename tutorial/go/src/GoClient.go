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
  "os"
  "thrift"
  "thriftlib/tutorial"
)

func Perform(client *tutorial.CalculatorClient) (err os.Error) {
  client.Ping()
  fmt.Print("ping()\n")
  
  sum, _ := client.Add(1, 1)
  fmt.Print("1+1=", sum, "\n")
  
  work := tutorial.NewWork()
  work.Op = tutorial.DIVIDE
  work.Num1 = 1
  work.Num2 = 0
  quotient, ouch, err := client.Calculate(1, work)
  if err != nil {
    fmt.Print("Error during operation: ", err.String(), "\n")
    return err
  } else if ouch != nil {
    fmt.Print("Invalid operation: ", ouch.String(), "\n")
  } else {
    fmt.Print("Whoa we can divide by 0 with new value: ", quotient, "\n")
  }
  
  work.Op = tutorial.SUBTRACT
  work.Num1 = 15
  work.Num2 = 10
  diff, ouch, err := client.Calculate(1, work)
  if err != nil {
    fmt.Print("Error during operation: ", err.String(), "\n")
    return err
  } else if ouch != nil {
    fmt.Print("Invalid operation: ", ouch.String(), "\n")
  } else {
    fmt.Print("15-10=", diff, "\n")
  }
  
  log, err := client.GetStruct(1)
  if err != nil {
    fmt.Print("Unable to get struct: ", err.String(), "\n")
    return err
  } else {
    fmt.Print("Check log: ", log.Value, "\n")
  }
  return err
}


func RunClient(transportFactory thrift.TTransportFactory, protocolFactory thrift.TProtocolFactory) os.Error {
  addr, err := net.ResolveTCPAddr("localhost:9090")
  if err != nil {
    fmt.Print("Error resolving address: ", err.String(), "\n")
    return err
  }
  transport := thrift.NewTSocketAddr(addr)
  if err = transport.Open(); err != nil {
    fmt.Print("Error opening connection for protocol ", addr.Network(), " to ", addr.String(), ": ", err.String(), "\n")
    return err
  }
  useTransport := transportFactory.GetTransport(transport)
  client := tutorial.NewCalculatorClientFactory(useTransport, protocolFactory)
  Perform(client)
  return transport.Close()
}
