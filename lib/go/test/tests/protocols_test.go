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

package tests

import (
	"testing"

	"github.com/apache/thrift/lib/go/test/gopath/src/thrifttest"
	"github.com/apache/thrift/lib/go/thrift"
)

func RunSocketTestSuite(
	t *testing.T,
	protocolFactory thrift.TProtocolFactory,
	transportFactory thrift.TTransportFactory,
) {
	// server
	var err error
	addr = FindAvailableTCPServerPort()
	serverTransport, err := thrift.NewTServerSocketTimeout(addr.String(), TIMEOUT)
	if err != nil {
		t.Fatal("Unable to create server socket", err)
	}
	processor := thrifttest.NewThriftTestProcessor(NewThriftTestHandler())
	server = thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)
	server.Listen()

	go server.Serve()

	// client
	cfg := &thrift.TConfiguration{
		ConnectTimeout: TIMEOUT,
		SocketTimeout:  TIMEOUT,
	}
	thrift.PropagateTConfiguration(transportFactory, cfg)
	var transport thrift.TTransport = thrift.NewTSocketFromAddrConf(addr, cfg)
	transport, err = transportFactory.GetTransport(transport)
	if err != nil {
		t.Fatal(err)
	}
	var protocol thrift.TProtocol = protocolFactory.GetProtocol(transport)
	thriftTestClient := thrifttest.NewThriftTestClient(thrift.NewTStandardClient(protocol, protocol))
	err = transport.Open()
	if err != nil {
		t.Fatal("Unable to open client socket", err)
	}

	driver := NewThriftTestDriver(t, thriftTestClient)
	driver.Start()
}

// Run test suite using TJSONProtocol
func TestTJSONProtocol(t *testing.T) {
	RunSocketTestSuite(
		t,
		thrift.NewTJSONProtocolFactory(),
		thrift.NewTTransportFactory(),
	)
	RunSocketTestSuite(
		t,
		thrift.NewTJSONProtocolFactory(),
		thrift.NewTBufferedTransportFactory(8912),
	)
	RunSocketTestSuite(
		t,
		thrift.NewTJSONProtocolFactory(),
		thrift.NewTFramedTransportFactoryConf(thrift.NewTTransportFactory(), nil),
	)
}

// Run test suite using TBinaryProtocol
func TestTBinaryProtocol(t *testing.T) {
	RunSocketTestSuite(
		t,
		thrift.NewTBinaryProtocolFactoryConf(nil),
		thrift.NewTTransportFactory(),
	)
	RunSocketTestSuite(
		t,
		thrift.NewTBinaryProtocolFactoryConf(nil),
		thrift.NewTBufferedTransportFactory(8912),
	)
	RunSocketTestSuite(
		t,
		thrift.NewTBinaryProtocolFactoryConf(nil),
		thrift.NewTFramedTransportFactoryConf(thrift.NewTTransportFactory(), nil),
	)
}

// Run test suite using TCompactBinaryProtocol
func TestTCompactProtocol(t *testing.T) {
	RunSocketTestSuite(
		t,
		thrift.NewTCompactProtocolFactoryConf(nil),
		thrift.NewTTransportFactory(),
	)
	RunSocketTestSuite(
		t,
		thrift.NewTCompactProtocolFactoryConf(nil),
		thrift.NewTBufferedTransportFactory(8912),
	)
	RunSocketTestSuite(
		t,
		thrift.NewTCompactProtocolFactoryConf(nil),
		thrift.NewTFramedTransportFactoryConf(thrift.NewTTransportFactory(), nil),
	)
}
