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
