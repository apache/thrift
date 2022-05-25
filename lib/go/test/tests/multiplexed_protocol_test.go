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
	"context"
	"net"
	"testing"
	"time"

	"github.com/apache/thrift/lib/go/test/gopath/src/multiplexedprotocoltest"
	"github.com/apache/thrift/lib/go/thrift"
)

func FindAvailableTCPServerPort() net.Addr {
	if l, err := net.Listen("tcp", "127.0.0.1:0"); err != nil {
		panic("Could not find available server port")
	} else {
		defer l.Close()
		return l.Addr()
	}
}

type FirstImpl struct{}

func (f *FirstImpl) ReturnOne(ctx context.Context) (r int64, err error) {
	return 1, nil
}

type SecondImpl struct{}

func (s *SecondImpl) ReturnTwo(ctx context.Context) (r int64, err error) {
	return 2, nil
}

func createTransport(addr net.Addr) (thrift.TTransport, error) {
	cfg := &thrift.TConfiguration{
		ConnectTimeout: TIMEOUT,
		SocketTimeout:  TIMEOUT,
	}
	socket := thrift.NewTSocketFromAddrConf(addr, cfg)
	transport := thrift.NewTFramedTransportConf(socket, cfg)
	err := transport.Open()
	if err != nil {
		return nil, err
	}
	return transport, nil
}

func TestMultiplexedProtocolFirst(t *testing.T) {
	processor := thrift.NewTMultiplexedProcessor()
	protocolFactory := thrift.NewTBinaryProtocolFactoryConf(nil)
	transportFactory := thrift.NewTTransportFactory()
	transportFactory = thrift.NewTFramedTransportFactoryConf(transportFactory, nil)
	addr := FindAvailableTCPServerPort()
	serverTransport, err := thrift.NewTServerSocketTimeout(addr.String(), TIMEOUT)
	if err != nil {
		t.Fatal("Unable to create server socket", err)
	}
	server = thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)

	firstProcessor := multiplexedprotocoltest.NewFirstProcessor(&FirstImpl{})
	processor.RegisterProcessor("FirstService", firstProcessor)

	secondProcessor := multiplexedprotocoltest.NewSecondProcessor(&SecondImpl{})
	processor.RegisterProcessor("SecondService", secondProcessor)

	defer server.Stop()
	go server.Serve()
	time.Sleep(10 * time.Millisecond)

	transport, err := createTransport(addr)
	if err != nil {
		t.Fatal(err)
	}
	defer transport.Close()
	protocol := thrift.NewTMultiplexedProtocol(thrift.NewTBinaryProtocolConf(transport, nil), "FirstService")

	client := multiplexedprotocoltest.NewFirstClient(thrift.NewTStandardClient(protocol, protocol))

	ret, err := client.ReturnOne(defaultCtx)
	if err != nil {
		t.Fatal("Unable to call first server:", err)
	} else if ret != 1 {
		t.Fatal("Unexpected result from server: ", ret)
	}
}

func TestMultiplexedProtocolSecond(t *testing.T) {
	processor := thrift.NewTMultiplexedProcessor()
	protocolFactory := thrift.NewTBinaryProtocolFactoryConf(nil)
	transportFactory := thrift.NewTTransportFactory()
	transportFactory = thrift.NewTFramedTransportFactoryConf(transportFactory, nil)
	addr := FindAvailableTCPServerPort()
	serverTransport, err := thrift.NewTServerSocketTimeout(addr.String(), TIMEOUT)
	if err != nil {
		t.Fatal("Unable to create server socket", err)
	}
	server = thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)

	firstProcessor := multiplexedprotocoltest.NewFirstProcessor(&FirstImpl{})
	processor.RegisterProcessor("FirstService", firstProcessor)

	secondProcessor := multiplexedprotocoltest.NewSecondProcessor(&SecondImpl{})
	processor.RegisterProcessor("SecondService", secondProcessor)

	defer server.Stop()
	go server.Serve()
	time.Sleep(10 * time.Millisecond)

	transport, err := createTransport(addr)
	if err != nil {
		t.Fatal(err)
	}
	defer transport.Close()
	protocol := thrift.NewTMultiplexedProtocol(thrift.NewTBinaryProtocolConf(transport, nil), "SecondService")

	client := multiplexedprotocoltest.NewSecondClient(thrift.NewTStandardClient(protocol, protocol))

	ret, err := client.ReturnTwo(defaultCtx)
	if err != nil {
		t.Fatal("Unable to call second server:", err)
	} else if ret != 2 {
		t.Fatal("Unexpected result from server: ", ret)
	}
}

func TestMultiplexedProtocolLegacy(t *testing.T) {
	processor := thrift.NewTMultiplexedProcessor()
	protocolFactory := thrift.NewTBinaryProtocolFactoryConf(nil)
	transportFactory := thrift.NewTTransportFactory()
	transportFactory = thrift.NewTFramedTransportFactoryConf(transportFactory, nil)
	addr := FindAvailableTCPServerPort()
	serverTransport, err := thrift.NewTServerSocketTimeout(addr.String(), TIMEOUT)
	if err != nil {
		t.Fatal("Unable to create server socket", err)
	}
	server = thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)

	firstProcessor := multiplexedprotocoltest.NewFirstProcessor(&FirstImpl{})
	processor.RegisterProcessor("FirstService", firstProcessor)

	secondProcessor := multiplexedprotocoltest.NewSecondProcessor(&SecondImpl{})
	processor.RegisterProcessor("SecondService", secondProcessor)

	defer server.Stop()
	go server.Serve()
	time.Sleep(10 * time.Millisecond)

	transport, err := createTransport(addr)
	if err != nil {
		t.Error(err)
		return
	}
	defer transport.Close()

	protocol := thrift.NewTBinaryProtocolConf(transport, nil)
	client := multiplexedprotocoltest.NewSecondClient(thrift.NewTStandardClient(protocol, protocol))

	_, err = client.ReturnTwo(defaultCtx)
	//expect error since default processor is not registered
	if err == nil {
		t.Fatal("Expecting error")
	}

	//register default processor and call again
	processor.RegisterDefault(multiplexedprotocoltest.NewSecondProcessor(&SecondImpl{}))
	transport, err = createTransport(addr)
	if err != nil {
		t.Error(err)
		return
	}
	defer transport.Close()

	protocol = thrift.NewTBinaryProtocolConf(transport, nil)
	client = multiplexedprotocoltest.NewSecondClient(thrift.NewTStandardClient(protocol, protocol))

	ret, err := client.ReturnTwo(defaultCtx)
	if err != nil {
		t.Fatal("Unable to call legacy server:", err)
	}
	if ret != 2 {
		t.Fatal("Unexpected result from server: ", ret)
	}
}
