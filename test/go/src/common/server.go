package common

import (
	"crypto/tls"
	"flag"
	"fmt"
	"gen/ThriftTest"
	"thrift"
)

var (
	debugServerProtocol bool
	certPath            string
)

func init() {
	flag.BoolVar(&debugServerProtocol, "debug_server_protocol", false, "turn server protocol trace on")
}

func StartServer(
	host string,
	port int64,
	domain_socket string,
	transport string,
	protocol string,
	ssl bool,
	handler ThriftTest.ThriftTest) (srv *thrift.TSimpleServer, err error) {

	hostPort := fmt.Sprintf("%s:%d", host, port)

	var protocolFactory thrift.TProtocolFactory
	switch protocol {
	case "compact":
		protocolFactory = thrift.NewTCompactProtocolFactory()
	case "simplejson":
		protocolFactory = thrift.NewTSimpleJSONProtocolFactory()
	case "json":
		protocolFactory = thrift.NewTJSONProtocolFactory()
	case "binary":
		protocolFactory = thrift.NewTBinaryProtocolFactoryDefault()
	default:
		return nil, fmt.Errorf("Invalid protocol specified %s", protocol)
	}
	if debugServerProtocol {
		protocolFactory = thrift.NewTDebugProtocolFactory(protocolFactory, "server:")
	}

	var serverTransport thrift.TServerTransport
	if ssl {
		cfg := new(tls.Config)
		if cert, err := tls.LoadX509KeyPair(certPath+"/server.crt", certPath+"/server.key"); err != nil {
			return nil, err
		} else {
			cfg.Certificates = append(cfg.Certificates, cert)
		}
		serverTransport, err = thrift.NewTSSLServerSocket(hostPort, cfg)
	} else {
		if domain_socket != "" {
			serverTransport, err = thrift.NewTServerSocket(domain_socket)
		} else {
			serverTransport, err = thrift.NewTServerSocket(hostPort)
		}
	}
	if err != nil {
		return nil, err
	}

	var transportFactory thrift.TTransportFactory

	switch transport {
	case "http":
		return nil, fmt.Errorf("Http server transport is not supported")
		// trans, err = thrift.NewTHttpClient(fmt.Sprintf("http://%s/service", hostPort))
		// if err != nil {
		// 	return nil, err
		// }
	case "framed":
		transportFactory = thrift.NewTTransportFactory()
		transportFactory = thrift.NewTFramedTransportFactory(transportFactory)
	case "buffered":
		transportFactory = thrift.NewTBufferedTransportFactory(8192)
	case "":
		transportFactory = thrift.NewTTransportFactory()
	default:
		return nil, fmt.Errorf("Invalid transport specified %s", transport)
	}
	processor := ThriftTest.NewThriftTestProcessor(handler)
	server := thrift.NewTSimpleServer4(processor, serverTransport, transportFactory, protocolFactory)
	if err = server.Listen(); err != nil {
		return
	}
	go server.AcceptLoop()
	return server, nil
}
