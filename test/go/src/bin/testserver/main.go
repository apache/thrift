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

package main

import (
	"common"
	"flag"
	"log"
)

var host = flag.String("host", "localhost", "Host to connect")
var port = flag.Int64("port", 9090, "Port number to connect")
var domain_socket = flag.String("domain-socket", "", "Domain Socket (e.g. /tmp/ThriftTest.thrift), instead of host and port")
var transport = flag.String("transport", "buffered", "Transport: buffered, framed, http")
var protocol = flag.String("protocol", "binary", "Protocol: binary, compact, json")
var ssl = flag.Bool("ssl", false, "Encrypted Transport using SSL")

func main() {
	flag.Parse()
	server, err := common.StartServer(*host, *port, *domain_socket, *transport, *protocol, *ssl, common.PrintingHandler)
	if err != nil {
		log.Fatalf("Unable to start server: ", err)
	}
	server.Serve()
}
