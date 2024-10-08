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

package common

import (
	"compress/zlib"
	"crypto/tls"
	"flag"
	"fmt"

	"github.com/apache/thrift/lib/go/thrift"
	"github.com/apache/thrift/test/go/src/gen/thrifttest"
)

var (
	debugServerProtocol bool
)

func init() {
	flag.BoolVar(&debugServerProtocol, "debug_server_protocol", false, "turn server protocol trace on")
}

func GetServerParams(
	host string,
	port int64,
	domain_socket string,
	transport string,
	protocol string,
	ssl bool,
	certPath string,
	handler thrifttest.ThriftTest,
) (thrift.TProcessor, thrift.TServerTransport, thrift.TTransportFactory, thrift.TProtocolFactory, string /* addr */, error) {

	var err error
	hostPort := fmt.Sprintf("%s:%d", host, port)
	var cfg *thrift.TConfiguration = nil

	var protocolFactory thrift.TProtocolFactory
	switch protocol {
	case "compact":
		protocolFactory = thrift.NewTCompactProtocolFactoryConf(cfg)
	case "simplejson":
		protocolFactory = thrift.NewTSimpleJSONProtocolFactoryConf(cfg)
	case "json":
		protocolFactory = thrift.NewTJSONProtocolFactory()
	case "binary":
		protocolFactory = thrift.NewTBinaryProtocolFactoryConf(nil)
	case "header":
		protocolFactory = thrift.NewTHeaderProtocolFactoryConf(nil)
	default:
		return nil, nil, nil, nil, "", fmt.Errorf("invalid protocol specified %s", protocol)
	}
	if debugServerProtocol {
		protocolFactory = thrift.NewTDebugProtocolFactoryWithLogger(protocolFactory, "server:", thrift.StdLogger(nil))
	}

	var serverTransport thrift.TServerTransport
	var addr string
	if transport == "http" {
		// In cross-test servers, we would call http.ListenAndServe
		// again on the host:port, so don't use the listen to fill the
		// addr and just generate it here instead.
		addr = hostPort
		if domain_socket != "" {
			addr = domain_socket
		}
	}
	if ssl {
		cfg := new(tls.Config)
		if cert, err := tls.LoadX509KeyPair(certPath+"/server.crt", certPath+"/server.key"); err != nil {
			return nil, nil, nil, nil, "", err
		} else {
			cfg.Certificates = append(cfg.Certificates, cert)
		}
		serverSocket, transportErr := thrift.NewTSSLServerSocket(hostPort, cfg)
		if transportErr == nil {
			if transport != "http" {
				listenErr := serverSocket.Listen()
				if listenErr == nil {
					serverTransport = serverSocket
					addr = serverSocket.Addr().String()
				} else {
					err = listenErr
				}
			}
		} else {
			err = transportErr
		}
	} else {
		if domain_socket != "" {
			serverTransport, err = thrift.NewTServerSocket(domain_socket)
			addr = domain_socket
		} else {
			serverSocket, transportErr := thrift.NewTServerSocket(hostPort)
			if transportErr == nil {
				if transport != "http" {
					listenErr := serverSocket.Listen()
					if listenErr == nil {
						serverTransport = serverSocket
						addr = serverSocket.Addr().String()
					} else {
						err = listenErr
					}
				}
			} else {
				err = transportErr
			}
		}
	}
	if err != nil {
		return nil, nil, nil, nil, "", err
	}

	var transportFactory thrift.TTransportFactory

	switch transport {
	case "http":
		// there is no such factory, and we don't need any
		transportFactory = nil
	case "framed":
		transportFactory = thrift.NewTTransportFactory()
		transportFactory = thrift.NewTFramedTransportFactoryConf(transportFactory, nil)
	case "buffered":
		transportFactory = thrift.NewTBufferedTransportFactory(8192)
	case "zlib":
		transportFactory = thrift.NewTZlibTransportFactory(zlib.BestCompression)
	case "":
		transportFactory = thrift.NewTTransportFactory()
	default:
		return nil, nil, nil, nil, "", fmt.Errorf("invalid transport specified %s", transport)
	}
	processor := thrifttest.NewThriftTestProcessor(handler)

	return processor, serverTransport, transportFactory, protocolFactory, addr, nil
}
