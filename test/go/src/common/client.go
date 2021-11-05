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
	"net/http"

	"github.com/apache/thrift/lib/go/thrift"
	"github.com/apache/thrift/test/go/src/gen/thrifttest"
)

var debugClientProtocol bool

func init() {
	flag.BoolVar(&debugClientProtocol, "debug_client_protocol", false, "turn client protocol trace on")
}

func StartClient(
	host string,
	port int64,
	domain_socket string,
	transport string,
	protocol string,
	ssl bool,
) (client *thrifttest.ThriftTestClient, trans thrift.TTransport, err error) {
	hostPort := fmt.Sprintf("%s:%d", host, port)
	cfg := &thrift.TConfiguration{
		TLSConfig: &tls.Config{
			InsecureSkipVerify: true,
		},
	}

	var protocolFactory thrift.TProtocolFactory
	switch protocol {
	case "compact":
		protocolFactory = thrift.NewTCompactProtocolFactoryConf(cfg)
	case "simplejson":
		protocolFactory = thrift.NewTSimpleJSONProtocolFactoryConf(cfg)
	case "json":
		protocolFactory = thrift.NewTJSONProtocolFactory()
	case "binary":
		protocolFactory = thrift.NewTBinaryProtocolFactoryConf(cfg)
	case "header":
		protocolFactory = thrift.NewTHeaderProtocolFactoryConf(cfg)
	default:
		return nil, nil, fmt.Errorf("invalid protocol specified %s", protocol)
	}
	if debugClientProtocol {
		protocolFactory = thrift.NewTDebugProtocolFactoryWithLogger(protocolFactory, "client:", thrift.StdLogger(nil))
	}
	if ssl {
		trans = thrift.NewTSSLSocketConf(hostPort, cfg)
	} else {
		if domain_socket != "" {
			trans = thrift.NewTSocketConf(domain_socket, nil)
		} else {
			trans = thrift.NewTSocketConf(hostPort, nil)
		}
	}
	if err != nil {
		return nil, nil, err
	}
	switch transport {
	case "http":
		if ssl {
			tr := &http.Transport{
				TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
			}
			client := &http.Client{Transport: tr}
			trans, err = thrift.NewTHttpClientWithOptions(fmt.Sprintf("https://%s/", hostPort), thrift.THttpClientOptions{Client: client})
			fmt.Println(hostPort)
		} else {
			trans, err = thrift.NewTHttpClient(fmt.Sprintf("http://%s/", hostPort))
		}
	case "framed":
		trans = thrift.NewTFramedTransportConf(trans, cfg)
	case "buffered":
		trans = thrift.NewTBufferedTransport(trans, 8192)
	case "zlib":
		trans, err = thrift.NewTZlibTransport(trans, zlib.BestCompression)
	case "":
		// Do nothing
	default:
		return nil, nil, fmt.Errorf("invalid transport specified %s", transport)
	}
	if err != nil {
		return nil, nil, err
	}
	if err = trans.Open(); err != nil {
		return nil, nil, err
	}
	iprot := protocolFactory.GetProtocol(trans)
	oprot := protocolFactory.GetProtocol(trans)
	client = thrifttest.NewThriftTestClient(thrift.NewTStandardClient(iprot, oprot))
	return
}
