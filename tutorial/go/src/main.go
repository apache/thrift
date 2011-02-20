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
  "flag"
  "fmt"
  "os"
  "thrift"
)

func Usage() {
  fmt.Fprint(os.Stderr, "Usage of ", os.Args[0], " <--server | --client>:\n")
  flag.PrintDefaults()
  fmt.Fprint(os.Stderr, "\n")
  os.Exit(0)
}

func main() {
  flag.Usage = Usage
  var client bool
  var server bool
  var protocol string
  var framed bool
  var useHttp bool
  var help bool
  
  flag.BoolVar(&client, "client", false, "Run client")
  flag.BoolVar(&server, "server", false, "Run server")
  flag.StringVar(&protocol, "P", "binary", "Specify the protocol (binary, compact, simplejson)")
  flag.BoolVar(&framed, "framed", false, "Use framed transport")
  flag.BoolVar(&useHttp, "http", false, "Use http")
  flag.BoolVar(&help, "help", false, "See usage string")
  flag.Parse()
  if help || (client && server) || !(client || server) {
    fmt.Print("flag.NArg() == ", flag.NArg(), "\n")
    flag.Usage()
  }
  var protocolFactory thrift.TProtocolFactory
  switch protocol {
  case "compact":
    protocolFactory = thrift.NewTCompactProtocolFactory()
  case "simplejson":
    protocolFactory = thrift.NewTSimpleJSONProtocolFactory()
  case "json":
    protocolFactory = thrift.NewTJSONProtocolFactory()
  case "binary", "":
    protocolFactory = thrift.NewTBinaryProtocolFactoryDefault()
  default:
    fmt.Fprint(os.Stderr, "Invalid protocol specified", protocol, "\n")
    Usage()
    os.Exit(1)
  }
  transportFactory := thrift.NewTTransportFactory()
  if framed {
    transportFactory = thrift.NewTFramedTransportFactory(transportFactory)
  }
  
  if(client) {
    RunClient(transportFactory, protocolFactory)
  } else if(server) {
    RunServer(transportFactory, protocolFactory)
  } else {
    flag.Usage()
  }
  os.Exit(0)
}
