package common

import (
	"crypto/tls"
	"flag"
	"fmt"
	"gen/ThriftTest"
	"thrift"
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
	ssl bool) (client *ThriftTest.ThriftTestClient, err error) {

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
	if debugClientProtocol {
		protocolFactory = thrift.NewTDebugProtocolFactory(protocolFactory, "client:")
	}
	var trans thrift.TTransport
	if ssl {
		trans, err = thrift.NewTSSLSocket(hostPort, &tls.Config{InsecureSkipVerify: true})
	} else {
		if domain_socket != "" {
			trans, err = thrift.NewTSocket(domain_socket)
		} else {
			trans, err = thrift.NewTSocket(hostPort)
		}
	}
	if err != nil {
		return nil, err
	}
	switch transport {
	case "http":
		trans, err = thrift.NewTHttpClient(fmt.Sprintf("http://%s/service", hostPort))
		if err != nil {
			return nil, err
		}
	case "framed":
		trans = thrift.NewTFramedTransport(trans)
	case "buffered":
		trans = thrift.NewTBufferedTransport(trans, 8192)
	case "":
		trans = trans
	default:
		return nil, fmt.Errorf("Invalid transport specified %s", transport)
	}

	if err = trans.Open(); err != nil {
		return nil, err
	}
	client = ThriftTest.NewThriftTestClientFactory(trans, protocolFactory)
	return
}
